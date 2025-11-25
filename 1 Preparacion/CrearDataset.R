################################################################################
# Configuración
################################################################################

# Cargar funciones de apoyo.
source("Scripts/0 Comun/Utilidades.R")

# Cargar paquetes.
importarPaquetes(c("tidyverse"))



################################################################################
# Constantes
################################################################################

RUTA_FUENTE <- "Datos/1 Fuente"

ARCHIVOS <- list(encuesta = "Encuesta.csv", carreras = "Carreras.csv",
                 admision = "Admision.csv", llaves = "Llaves.csv",
                 notas = "Notas.csv", pdt = "PDT.csv")

ARCHIVOS <- lapply(ARCHIVOS, function(x) file.path(RUTA_FUENTE, x))
DATASET <- "Datos/2 Brutos/Dataset bruto.csv"



################################################################################
# Funciones para procesar respuestas a consentimiento informado e instrumentos
# de medición de competencias informacionales autopercibidas (CInfA) y
# observadas (CInfO)
################################################################################

leerEncuesta <- function() {
  # Lee el archivo con las respuestas a la encuesta aplicada mediante LimeSurvey
  # con el consentimiento informado y los instrumentos para medir CInfA y CInfO.
  # Preprocesa el conjunto de datos:
  # - Descarta columnas propias de LimeSurvey que no aportan información.
  # - Da formato a las columnas.
  # - Descarta duplicados.
  # - Descarta observaciones correspondientes a estudiantes que no consintieron
  #   en participar o que no autorizaron el uso de sus calificaciones.
  # - Asigna identificador único a las observaciones.
  # - Descarta columnas de identificación y consentimiento.
  # Entrada: ninguna.
  # Salida: dataframe con las respuestas a los instrumentos para medir CInfA y
  # CInfO.
  
  # Cargar datos de encuesta y eliminar columnas propias de LimeSurvey.
  df <- read.csv2(ARCHIVOS$encuesta)
  df <- df[-c(1, 1:5)]
  
  # Renombrar columnas de identificación, dejar RUN terminado en K con
  # mayúscula y descartar estudiantes duplicados.
  df <- df %>% rename(RUN = D01)
  df$RUN <- toupper(df$RUN)
  df <- df %>% filter(duplicated(RUN) == FALSE)
  
  # Reemplazar strings vacíos por "N".
  df[df == ""] <- "N"
  
  # Descartar alumnos que no aceptaron el consentimiento o el uso de sus
  # calificaciones, eliminar columnas de consentimiento y retornar las
  # respuestas.
  df <- df %>% filter(C01 == "AO01")
  df <- df %>% filter(C02 == "AO01")
  
  # Anonimizar asignando identificador único leído desde el archivo de llaves.
  llaves <- read.csv2(ARCHIVOS$llaves) %>% select(c(RUN, ID))
  df <- merge(df, llaves, by = "RUN", all.x = FALSE, all.y = FALSE)
  df <- df %>% relocate(ID, .before = RUN)
  
  # Eliminar columnas de identificación y consentimiento, y retornar las
  # respuestas.
  df <- df %>% select(-c(C01, C02, D02, RUN))
  return(df)
}

puntuarCInfA <- function(df) {
  # Calcula los puntajes obtenidos para cada dimensión de CInfA.
  # Entrada:
  # - df: dataframe con las respuestas a los instrumentos para medir CInfA y
  #   CInfO.
  # Salida: dataframe con los puntajes obtenidos para cada dimensión de CInfA y
  # las respuestas al instrumento para medir CInfO.
  
  # Renombrar columnas para cada pregunta de CInfA.
  df <- df %>% rename(A01 = A00.SQ001., A02 = A00.SQ002., A03 = A00.SQ003.,
                      A04 = A00.SQ004., A05 = A00.SQ005., A06 = A00.SQ006.,
                      A07 = A00.SQ007., A08 = A00.SQ008., A09 = A00.SQ009.)
  
  # Puntuar CInfA por dimensión y descartar respuestas a las preguntas.
  df$A_BUS <- df$A01 + df$A02 + df$A03 + df$A04
  df$A_EVAL = df$A05 + df$A06 + df$A07 + df$A08 + df$A09
  df <- df %>% select(-starts_with("A0"))
  
  # Normalizar puntajes para cada dimensión de CInfA, calcular puntaje total y
  # devolver el dataframe resultante.
  df$A_BUS <- 0.625 * df$A_BUS - 2.5
  df$A_EVAL <- 0.5 * df$A_EVAL - 2.5
  df$A_TOT <- df$A_BUS + df$A_EVAL
  return(df)
}

puntuarPreguntasCInfO <- function(df) {
  # Calcula los puntajes obtenidos para cada pregunta de CIO.
  # Entrada:
  # - df: dataframe con los puntajes obtenidos para cada dimensión de CInfA y
  #   las respuestas al instrumento para medir CInfO.
  # Salida: dataframe con los puntajes obtenidos para cada dimensión de CInfA y
  # cada pregunta del instrumento para medir CInfO.
  
  # Pregunta 1.
  n <- nrow(df)
  e1 <- rep(0, n)
  e2 <- rep(0, n)
  e3 <- rep(0, n)
  e4 <- rep(0, n)
  e5 <- rep(0, n)
  e1[df$O01.SQ001. != "Y"] <- 0.2
  e2[df$O01.SQ002. == "Y"] <- 0.2
  e3[df$O01.SQ003. == "Y"] <- 0.2
  e4[df$O01.SQ004. != "Y"] <- 0.2
  e5[df$O01.SQ005. == "Y"] <- 0.2
  resultado <- data.frame(O01 = e1 + e2 + e3 + e4 + e5)
  
  # Pregunta 2.
  e1 <- rep(0, n)
  e2 <- rep(0, n)
  e3 <- rep(0, n)
  e4 <- rep(0, n)
  e5 <- rep(0, n)
  e1[df$O02.SQ001. == "Y"] <- 0.2
  e2[df$O02.SQ002. != "Y"] <- 0.2
  e3[df$O02.SQ003. != "Y"] <- 0.2
  e4[df$O02.SQ004. == "Y"] <- 0.2
  e5[df$O02.SQ005. == "Y"] <- 0.2
  resultado$O02 <- e1 + e2 + e3 + e4 + e5
  
  # Pregunta 3.
  aux <-  paste(df$O03.SQ001., df$O03.SQ002., df$O03.SQ003., df$O03.SQ004.,
                sep = "")
  
  resultado$O03 <- sapply(aux, function(x) {switch(x, "ABACADAA" = 1,
                                                   "ABADACAA" = 0.875,
                                                   "ACABADAA" = 0.75,
                                                   "AAACADAB" = 0.625,
                                                   "AAADACAB" = 0.625,
                                                   "ADACABAA" = 0.5,
                                                   "ACADABAA" = 0.375,
                                                   "ADABACAA" = 0.375,
                                                   "ABAAADAC" = 0.375,
                                                   "AAABADAC" = 0.25,
                                                   "AAACABAD" = 0.25,
                                                   "ABACAAAD" = 0.25,
                                                   "ACAAADAB" = 0.125,
                                                   "ABAAACAD" = 0.125,
                                                   "ABADAAAC" = 0.125,
                                                   "ADACAAAB" = 0.125, 0)})
  
  # Pregunta 4.
  resultado$O04 <- rep(0, n)
  resultado$O04[df$O04a == "AO03"] <- 1
  
  # Pregunta 5.
  e1 <- rep(0, n)
  e2 <- rep(0, n)
  e3 <- rep(0, n)
  e4 <- rep(0, n)
  e1[df$O05.SQ001. == "Y"] <- 0.25
  e2[df$O05.SQ002. != "Y"] <- 0.25
  e3[df$O05.SQ003. != "Y"] <- 0.25
  e4[df$O05.SQ004. == "Y"] <- 0.25
  resultado$O05 <- e1 + e2 + e3 + e4
  
  # Pregunta 6.
  e1 <- rep(0, n)
  e2 <- rep(0, n)
  e3 <- rep(0, n)
  e4 <- rep(0, n)
  e5 <- rep(0, n)
  e1[df$O06.SQ001. == "Y"] <- 0.2
  e2[df$O06.SQ002. != "Y"] <- 0.2
  e3[df$O06.SQ003. != "Y"] <- 0.2
  e4[df$O06.SQ004. != "Y"] <- 0.2
  e5[df$O06.SQ005. == "Y"] <- 0.2
  resultado$O06 <- e1 + e2 + e3 + e4 + e5
  
  # Pregunta 7.
  e1 <- rep(0, n)
  e2 <- rep(0, n)
  e3 <- rep(0, n)
  e4 <- rep(0, n)
  e1[df$O07a.SQ001. != "Y"] <- 0.125
  e2[df$O07a.SQ002. == "Y"] <- 0.125
  e3[df$O07a.SQ003. == "Y"] <- 0.125
  e4[df$O07a.SQ004. != "Y"] <- 0.125
  Oa <- e1 + e2 + e3 + e4
  Ob <- rep(0, n)
  Ob[df$O07b == "AO01"] <- 0.5
  resultado$O07 <- Oa + Ob
  
  # Pregunta 8.
  Oa <- rep(0, n)
  Oa[df$O08a == "AO03"] <- 1 / 6
  Ob <- rep(0, n)
  Ob[df$O08b == "AO03"] <- 1 / 6
  Oc <- rep(0, n)
  Oc[df$O08c == "AO01"] <- 1 / 6
  Od <- rep(0, n)
  Od[df$O08d == "AO02"] <- 1 / 6
  Oe <- rep(0, n)
  Oe[df$O08e == "AO01"] <- 1 / 6
  Of <- rep(0, n)
  Of[df$O08f == "AO02"] <- 1 / 6
  resultado$O08 <- Oa + Ob + Oc + Od + Oe + Of
  
  # Pregunta 9.
  Oa <- rep(0, n)
  Oa[df$O09a == "AO02"] <- 1 / 3
  Ob <- rep(0, n)
  Ob[df$O09b == "AO02"] <- 1 / 3
  Oc <- rep(0, n)
  Oc[df$O09c == "AO01"] <- 1 / 3
  resultado$O09 <- Oa + Ob + Oc
  
  # Pregunta 10.
  resultado$O10 <- rep(0, n)
  resultado$O10[df$O10 == "AO02"] <- 1
  
  # Pregunta 11.
  resultado$O11 <- rep(0, n)
  resultado$O11[df$O11 == "AO01"] <- 1
  
  # Pregunta 12.
  e1 <- rep(0, n)
  e2 <- rep(0, n)
  e3 <- rep(0, n)
  e4 <- rep(0, n)
  e5 <- rep(0, n)
  e1[df$O12.SQ001. == "Y"] <- 0.2
  e2[df$O12.SQ002. != "Y"] <- 0.2
  e3[df$O12.SQ003. == "Y"] <- 0.2
  e4[df$O12.SQ004. != "Y"] <- 0.2
  e5[df$O12.SQ005. == "Y"] <- 0.2
  resultado$O12 <- e1 + e2 + e3 + e4 + e5
  
  # Pregunta 13.
  resultado$O13 <- rep(0, n)
  resultado$O13[df$O13 == "AO03"] <- 1
  
  # Pregunta 14.
  Oa <- rep(0, n)
  Oa[df$O14a == "AO01"] <- 0.2
  Ob <- rep(0, n)
  Ob[df$O14b == "AO02"] <- 0.2
  Oc <- rep(0, n)
  Oc[df$O14c == "AO02"] <- 0.2
  Od <- rep(0, n)
  Od[df$O14d == "AO02"] <- 0.2
  Oe <- rep(0, n)
  Oe[df$O14e == "AO01"] <- 0.2
  resultado$O14 <- Oa + Ob + Oc + Od + Oe
  
  # Pregunta 15.
  Oa <- rep(0, n)
  Oa[df$O15a == "AO03"] <- 1 / 3
  Ob <- rep(0, n)
  Ob[df$O15b == "AO01"] <- 1 / 3
  Oc <- rep(0, n)
  Oc[df$O15c == "AO04"] <- 1 / 3
  resultado$O15 <- Oa + Ob + Oc
  
  # Pregunta 16.
  e1 <- rep(0, n)
  e2 <- rep(0, n)
  e3 <- rep(0, n)
  e4 <- rep(0, n)
  e5 <- rep(0, n)
  e1[df$O16.SQ001. != "Y"] <- 0.2
  e2[df$O16.SQ002. == "Y"] <- 0.2
  e3[df$O16.SQ003. != "Y"] <- 0.2
  e4[df$O16.SQ004. != "Y"] <- 0.2
  e5[df$O16.SQ005. == "Y"] <- 0.2
  resultado$O16 <- e1 + e2 + e3 + e4 + e5
  
  # Pregunta 17.
  Oa <- rep(0, n)
  Oa[df$O17a == "AO01"] <- 0.25
  Ob <- rep(0, n)
  Ob[df$O17b == "AO02"] <- 0.25
  Oc <- rep(0, n)
  Oc[df$O17c == "AO04"] <- 0.25
  Od <- rep(0, n)
  Od[df$O17d == "AO01"] <- 0.25
  resultado$O17 <- Oa + Ob + Oc + Od
  df <- df %>% select(-c(O17a, O17b, O17c, O17d))
  
  # Pregunta 18.
  resultado$O18 <- rep(0, n)
  resultado$O18[df$O18 == "AO03"] <- 1
  
  # Reemplazar respuestas por puntajes y devolver el data frame modificado.
  df <- df %>% select(-starts_with("O"))
  df <- cbind(df, resultado)
  return(df)
}

puntuarIndicadoresCInfO <- function(df) {
  # Calcula los puntajes obtenidos para cada indicador de CIO.
  # Entrada:
  # - df: dataframe con los puntajes obtenidos para cada dimensión de CInfA y
  #   cada pregunta del instrumento para medir CInfO.
  # Salida: dataframe con los puntajes obtenidos para cada dimensión de CInfA y
  # cada indicador del instrumento para medir CInfO.
  
  # Puntuar indicadores.
  resultado <- data.frame(O01 = df$O01 + df$O02 + df$O04 + df$O05,
                          O02 = df$O01 + df$O02 + df$O05,
                          O05 = df$O06,
                          O06 = df$O06,
                          O07 = df$O03 + df$O04,
                          O08 = df$O02 + df$O03 + df$O04,
                          O14 = df$O08 + df$O09,
                          O17 = df$O07 + df$O08 + df$O09, O19 = df$O07,
                          O21 = df$O10 + df$O11 + df$O13, O22 = df$O13,
                          O23 = df$O12,
                          O24 = df$O11 + df$O12 + df$O14,
                          O25 = df$O10,
                          O26 = df$O11 + df$O14,
                          O28 = df$O12 + df$O14,
                          O30 = df$O11 + df$O14,
                          O34 = df$O18,
                          O38 = df$O17,
                          O40 = df$O15,
                          O41 = df$O16)
  
  # Reemplazar puntajes de preguntas por puntajes de indicadores y devolver el
  # data frame modificado.
  df <- df %>% select(-starts_with("O"))
  df <- cbind(df, resultado)
  return(df)
}

puntuarResultadosAprendizajeCInfO <- function(df) {
  # Calcula los puntajes obtenidos para cada resultado de aprendizaje de CInfO.
  # Entrada:
  # - df: dataframe con los puntajes obtenidos para cada dimensión de CInfA y
  #   cada indicador del instrumento para medir CInfO.
  # Salida: dataframe con los puntajes obtenidos para cada dimensión de CInfA y
  # cada resultado de aprendizaje del instrumento para medir CInfO.
  
  # Puntuar resultados de aprendizaje.
  resultado <- data.frame(OB1 = df$O01 + df$O02,
                          OB2 = df$O05 + df$O06 + df$O07 + df$O08,
                          OE1 = df$O14, OE2 = df$O17 + df$O19,
                          OP1 = df$O21 + df$O22,
                          OP2 = df$O23 + df$O24 + df$O25,
                          OP3 = df$O26 + df$O28, OP4 = df$O30, OC2 = df$O34,
                          OC3 = df$O38, OC4 = df$O40 + df$O41)
  
  # Reemplazar puntajes de indicadores por puntajes de resultados de
  # aprendizaje y devolver el data frame modificado.
  df <- df %>% select(-starts_with("O"))
  df <- cbind(df, resultado)
  return(df)
}

puntuarCInfO <- function(df) {
  # Calcula los puntajes obtenidos para cada dimensión de CInfO.
  # Entrada:
  # - df: dataframe con los puntajes obtenidos para cada dimensión de CInfA y
  #   cada resultado de aprendizaje del instrumento para medir CInfO.
  # Salida: dataframe con los puntajes obtenidos para cada dimensión de CInfA y
  # CInfO.
  
  # Puntuar dimensiones.
  resultado <- data.frame(O_BUS = df$OB1 + df$OB2,
                          O_EVAL = df$OE1 + df$OE2,
                          O_PROC = df$OP1 + df$OP2 + df$OP3 + df$OP4,
                          O_COM = df$OC2 + df$OC3 + df$OC4)
  
  # Normalizar puntajes y agregar total.
  resultado$O_BUS <- (resultado$O_BUS / 14) * 10
  resultado$O_EVAL <- (resultado$O_EVAL / 6) * 10
  resultado$O_PROC <- (resultado$O_PROC / 15) * 10
  resultado$O_COM <- (resultado$O_COM / 4) * 10
  
  resultado$O_TOT <- resultado$O_BUS + resultado$O_EVAL + resultado$O_PROC + 
    resultado$O_COM
  
  # Reemplazar puntajes de resultados de aprendizaje por puntajes de
  # dimensiones y devolver el data frame modificado.
  df <- df %>% select(-starts_with("O"))
  df <- cbind(df, resultado)
  return(df)
}

procesarEncuesta <- function() {
  # Lee el archivo con las respuestas a la encuesta aplicada mediante LimeSurvey
  # con el consentimiento informado y los instrumentos para medir CInfA y CInfO,
  # y genera un dataframe anonimizado con los puntajes obtenidos solo por
  # estudiantes que aceptaron el consentimiento informado y autorizaron el uso de
  # sus calificaciones.
  # Entrada: ninguna.
  # Salida: dataframe con los puntajes obtenidos para cada dimensión de CInfA y
  # CInfO.
  
  df <- leerEncuesta()
  df <- puntuarCInfA(df)
  df <- puntuarPreguntasCInfO(df)
  df <- puntuarIndicadoresCInfO(df)
  df <- puntuarResultadosAprendizajeCInfO(df)
  df <- puntuarCInfO(df)
  return(df)
}



################################################################################
# Funciones para cargar y ordenar antecedentes de ingreso y calificaciones de
# primer semestre.
################################################################################

cargarPDT <- function() {
  # Lee el archivo con los puntajes obtenidos en la Prueba de Transición
  # Universitaria y da formato a las columnas.
  # Entrada: ninguna.
  # Salida: dataframe con puntajes de la PDT.

  # Leer archivo y dar formato a valores faltantes.
  df <- read.csv2(ARCHIVOS$pdt)
  df[df == ""] <- NA

  # Filtrar, renombrar y ordenar columnas de interés.
  df <- df %>% rename(NAC = NACIONALIDAD, PREF = PREFERENCIA,
                      P_PON = PTJ_PONDERADO, P_NEM = PTJE_NEM,
                      P_RAN = RANKING, P_LEN = PTJE_LENGUAJE,
                      P_MAT = PTJE_MATEMATICA, P_CIE = PTJE_CIENCIAS)

  df <- df %>% select(c(ID, NAC, SEXO, PREF, NEM, P_LEN, P_MAT, P_CIE, P_NEM,
                        P_RAN, P_PON))

  return(df)
}

cargarAdmision <- function() {
  # Lee el archivo con los datos de admisión y da formato a las columnas.
  # Entrada: ninguna.
  # Salida: dataframe con datos de admisión.

  # Leer archivo y dar formato a valores faltantes.
  df <- read.csv2(ARCHIVOS$admision, stringsAsFactors = TRUE)
  df[df == ""] <- NA

  # Formatear gratuidad.
  df$GRAT <- rep("SI", nrow(df))
  df$GRAT[df$GRATUIDAD == 0] <- "NO"

  # Formatear beca.
  BECA <- rep("SI", nrow(df))
  BECA[df$BECA == 0] <- "NO"
  df$BECA <- BECA

  # Formatrar dependencia.
  df <- df %>% rename(DEP = DEPENDENCIA)

  df$DEP <- recode_factor(
    df$DEP, Municipal = "MUNICIPAL", Part_Pag = "PART. PAGADO",
    Part_Subv = "PART. SUBV.")

  # Seleccionar columnas de interés.
  df <- df %>% select(c("ID", "GRAT", "BECA", "DEP"))
  return(df)
}

cargarCalificaciones <- function() {
  # Lee el archivo con los datos de carrera y calificaciones, y da formato a las
  # columnas.
  # Entrada: ninguna.
  # Salida: dataframe con datos de carrera y calificaciones.

  # Leer archivo y dar formato a valores faltantes.
  df <- read.csv2(ARCHIVOS$notas, stringsAsFactors = TRUE,
                  fileEncoding = "latin1")
  
  df[df == ""] <- NA

  # Renombrar y seleccionar columnas de interés.
  df <- df %>% rename(CODIGO = cal_ccarr, AGNO_ING = cal_agno_ing,
                      SEM_ING = cal_sem_ing, CURSO = asi_nom,
                      AGNO_CURSA = cal_agno, SEM_CURSA = cal_sem,
                      NOTA = cal_nota_teo, NOTA_PPA = PPA.del.Semestre,
                      SIT = cal_sit_alu)

  # Seleccionar columnas de interés.
  df <- df %>% select(ID, CODIGO, AGNO_ING, SEM_ING, CURSO, AGNO_CURSA,
                      SEM_CURSA, NOTA, NOTA_PPA, SIT)

  # Renombrar y filtrar asignaturas de interés.
  df$CURSO <- recode_factor(
    df$CURSO, !!!c(
      "ÁLGEBRA I PARA INGENIERÍA" = "ALG",
      "CÁLCULO I PARA INGENIERÍA" = "CAL",
      "FISICA I PARA INGENIERÍA" = "FIS",
      "FÍSICA I PARA INGENIERÍA" = "FIS"))
  
  df <- df %>% filter(CURSO %in% c("ALG", "CAL", "FIS"))
  df <- droplevels(df)

  # Descartar alumnos con ingreso anterior a 2021 y de Bachillerato.
  df <- df %>% filter(AGNO_ING >= 2021)
  df <- df %>% filter(CODIGO < 5000)

  # Descartar asignaturas no rendidas en el semestre de ingreso.
  df <- df[which(df$AGNO_ING == df$AGNO_CURSA & df$SEM_ING == df$SEM_CURSA), ]
  df <- df %>% select(-c(AGNO_ING, AGNO_CURSA, SEM_ING, SEM_CURSA))
  
  # Formatear situación final.
  df$SIT <- recode_factor(df$SIT, !!!c("A" = "APRUEBA", "R" = "REPRUEBA"))

  # Formatear calificaciones y llevar a formato ancho.
  df$NOTA <- as.numeric(gsub(",", ".", df$NOTA))
  df <- df %>% pivot_wider(names_from = CURSO, values_from = c(NOTA, SIT))

  # Agregar PPA discretizado.
  df$SIT_PPA <- rep("APRUEBA", nrow(df))
  df$SIT_PPA[df$NOTA_PPA < 4] <- "REPRUEBA"

  # Agregar datos de carrera.
  carreras <- read.csv2(ARCHIVOS$carreras, stringsAsFactors = TRUE)
  df <- merge(df, carreras, by = "CODIGO", all.x = TRUE)
  df <- df %>% select(-CODIGO)
  return(df)
}



#############################################################################
# Main
##############################################################################

# Cargar y preparar datos de las distintas fuentes.
cinf <- procesarEncuesta()
pdt <- cargarPDT()
admision <- cargarAdmision()
rendimiento <- cargarCalificaciones()

# Combinar datos de las distintas fuentes.
df <- merge(cinf, pdt, by = "ID", all.x = TRUE)
df <- merge(df, admision, by = "ID", all.x = TRUE)
df <- merge(df, rendimiento, by = "ID", all.x = TRUE)

# Descartar observaciones incompletas.
df <- df %>% filter(complete.cases(df))

# Ordenar columnas.
df <- df %>% select(c(
  "A_BUS", "A_EVAL", "A_TOT", "O_BUS", "O_EVAL", "O_PROC", "O_COM", "O_TOT",
  "NAC", "SEXO", "DEP", "GRAT", "PREF", "BECA", "CARR", "T_CARR", "DPTO",
  "NEM", "P_LEN", "P_MAT", "P_CIE", "P_NEM", "P_RAN", "P_PON", "NOTA_ALG",
  "NOTA_CAL", "NOTA_FIS", "NOTA_PPA", "SIT_ALG", "SIT_CAL", "SIT_FIS",
  "SIT_PPA"))

# Guardar data frame de datos brutos.
guardarDataframe(df, DATASET)
