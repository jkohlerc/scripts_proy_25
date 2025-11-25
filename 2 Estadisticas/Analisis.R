################################################################################
# Configuración
################################################################################

setwd("D:/Dropbox/Tesis/Estudio")
source("Scripts/0 Comun/Utilidades.R")

importarPaquetes(c(
  "combinat", "doParallel", "doRNG", "foreach", "ggpubr", "patchwork", "pROC",
  "PRROC", "rstatix", "tidyverse"))

source("Scripts/0 Comun/Estadisticas.R")

# Configuración de paralelización.
NUCLEOS <-  detectCores() - 2
CONSTANTES <- c("ALFA", "N_BOOT", "SEMILLA")



################################################################################
# Constantes
################################################################################

RUTA_RESULTADOS <- "Resultados/1 Analisis"
DATASET_BRUTO <- "Datos/2 Brutos/Dataset bruto.csv"
DATASET_LIMPIO <- "Datos/3 Limpio/Dataset limpio.csv"



################################################################################
# Funciones generales.
################################################################################

activarParalelismo <- function() {
  # Crea el cluster y carga los elementos necesarios en los nodos.
  # Entrada: ninguna.
  # Salida: cluster.
  
  # Fijar semilla para paralelización.
  RNGkind("L'Ecuyer-CMRG")
  set.seed(SEMILLA)
  
  # Crear cluster y activar paralelización.
  cluster <- makePSOCKcluster(NUCLEOS)
  registerDoParallel(cluster)

  # Inicializar RNG reproducible en los nodos
  parallel::clusterSetRNGStream(cluster, iseed = SEMILLA)
  
  # Exportar entorno de trabajo necesario a los nodos.
  parallel::clusterExport(cluster, varlist = NULL, envir = .GlobalEnv)
  
  # Exportar paquetes y scripts.
  clusterEvalQ(cluster, {
    library(combinat)
    library(doParallel)
    library(doRNG)
    library(foreach)
    library(rstatix)
    library(pROC)
    library(PRROC)
    library(tidyverse)
    
    source("Scripts/0 Comun/Utilidades.R")
    source("Scripts/0 Comun/Estadisticas.R")
  })
  
  return(cluster)
}

cargarDatos <- function() {
  # Lee el archivo con el conjunto de datos brutos, descarta variables que,
  # por definición, no aportan información y da formato a variables categóricas.
  # Entrada: ninguna.
  # Salida: dataframe con los datos brutos.
  
  # Cargar datos brutos y formatear variables categóricas.
  df <- read.csv2(DATASET_BRUTO, stringsAsFactors = TRUE)
  df$PREF <- factor(df$PREF)
  
  # Descartar variables que, por definición, se correlacionan con otras.
  df <- df %>% select(-c(A_TOT, O_TOT, NEM, CARR))
  return(df)
}

desactivarParalelismo <- function(cluster) {
  # Detiene el clúster de paralelización y vuelve al procesamiento secuencial.
  # Entrada:
  # - cluster: cluster.
  # Salida: ninguna.

  if(!is.null(cluster)) {
    # Detener el cluster.
    stopCluster(cluster)

    # Volver a procesamiento secuencial.
    if(getDoParRegistered()) {
      registerDoSEQ()
    }
  }
}



################################################################################
# Funciones para estadísticas descriptivas generales
################################################################################

crearGraficoCajas <- function(df, prefijo, titulo, nombreX, nombreY) {
  # Genera un gráfico de cajas con las variables del dataframe que comienzan con
  # un prefijo dado.
  # Entrada:
  # - df: dataframe con el conjunto de datos.
  # - prefijo: prefijo común para los nombres de las columnas de interés.
  # - titulo: titulo del gráfico.
  # - nombreX: nombre del eje X.
  # - nombreY: nombre del eje Y.
  # Salida: gráfico generado.
  
  # Filtrar columnas con el prefijo.
  filtrado <- df %>% select(starts_with(prefijo))
  
  # Convertir a formato largo.
  filtrado <- filtrado %>%
    pivot_longer(cols = everything(),
                 names_to = nombreX,
                 values_to = nombreY)
  
  # Crear el gráfico.
  g <- ggboxplot(filtrado, x = nombreX, y = nombreY, fill = nombreX,
                 title = titulo) +
    rremove("legend") +
    rotate_x_text(45) +
    scale_y_continuous(labels = formatearEjeY(titulo))
  
  return(g)
}

combinarGraficos <- function(
    gCinfA, gCinfO, gPDT, gNotas, titulo, nombreArchivo, subtitulo = NULL) {
  # Recibe cuatro gráficos y los combina en una única figura, como una matriz
  # de 2x2.
  # Entrada:
  # - gCinfA: gráfico de cajas con la variabilidad de CInfA.
  # - gCinfO: gráfico de cajas con la variabilidad de CInfO.
  # - gPDT: gráfico de cajas con la variabilidad de puntajes PDT.
  # - gNotas: gráfico de cajas con la variabilidad de calificaciones.
  # - titulo: título del gráfico.
  # - nombreArchivo: nombre del archivo  donde se guarda el gráfico.
  # - subtitulo: subtítulo (opcional) del gráfico.
  # Salida: archivo .pdf con el gráfico generado.
  
  panel <- ggarrange(gCinfA, gCinfO, gPDT, gNotas, ncol = 2, nrow = 2)
  
  if(!is.null(subtitulo)) {
    texto <- paste0(titulo, "\n", subtitulo)
  } else {
    texto <- titulo
  }
  
  anotacion <- text_grob(texto, face = "bold", size = 14, vjust = 0.5,
                         hjust = 0.5)
  
  g <- annotate_figure(panel, top = anotacion)
  archivo <- paste0(RUTA_RESULTADOS, "/", nombreArchivo)
  crearRuta(archivo)
  ggexport(g, filename = archivo, verbose = FALSE, width = 10, height = 8)
}

formatearEjeY <- function(titulo, decimales = 1) {
  # Define formato numérico para marcas del eje Y de un gráfico.
  # Entrada:
  # - titulo: título del gráfico.
  # - decimales: cantidad de decimales.
  # Salida: definición de la función de formato.
  
  formatoY <- if(startsWith(titulo, "Prueba")) {
    function(x) as.integer(x)
  } else {
    function(x) sapply(x, function(v) formatearFlotante(v, decimales))
  }

    
  return(formatoY)  
}

resumirCategoricas <- function(df) {
  # Reporta las frecuencias para todas las variables categóricas de un
  # dataframe.
  # Entrada:
  # -df: dataframe con el conjunto de datos.
  # Salida: archivo csv con el resumen.
  
  df <- df %>% select_if(~class(.) == 'factor')
  resumenes <- list()
  
  for(nombre in colnames(df)) {
    columna <- as.factor(df[[nombre]])
    frecuencias <- table(columna)
    
    resumen <- data.frame(
      Variable = nombre,
      Nivel = names(frecuencias),
      Frecuencia = as.numeric(frecuencias),
      stringsAsFactors = FALSE
    )
    
    resumenes[[nombre]] <- resumen
  }
  
  resultado <- do.call(rbind, resumenes)
  archivo <- paste0(RUTA_RESULTADOS, "/Resumen categoricas.csv")
  guardarDataframe(resultado, archivo)
}

resumirNumericas <- function(df) {
  # Reporta las frecuencias para todas las variables numéricas del dataframe.
  # Entrada:
  # -df: dataframe con el conjunto de datos.
  # Salida: archivo .csv con el resumen.
  
  # Filtrar variables numéricas.
  df <- df %>% select_if(~class(.) != 'factor')
  
  # Generar reporte de resumen.
  resultado <- NULL
  
  for(nombre in colnames(df)) {
    # Calcular cuartiles.
    cuartiles <- quantile(df[[nombre]])
    
    actual <- data.frame(Variable = nombre,
                         Minimo = round(min(df[[nombre]]), 3),
                         Q1 = round(cuartiles[2], 3),
                         Media = round(mean(df[[nombre]]), 3),
                         Q3 = round(cuartiles[4], 3),
                         Maximo = round(max(df[[nombre]]), 3),
                         Desv_est = round(sd(df[[nombre]]), 3),
                         Varianza = round(var(df[[nombre]]), 3))
    
    resultado <- rbind(resultado, actual)
  }
  
  archivo <- paste0(RUTA_RESULTADOS, "/Resumen numericas.csv")
  guardarDataframe(resultado, archivo)
  
}

reportarDescriptivas <- function(df) {
  # Reporta estadísticas descriptivas para el conjunto de datos (frecuencia para
  # variables categóricas y medidas de tendencia central y dispersión para
  # variables numéricas) y grafica variabilidad de las variables numéricas
  # mediante gráficos de caja.
  # Entrada:
  # -df: dataframe con el conjunto de datos.
  # Salida: archivos .csv con los resúmenes y .pdf con los gráficos.
  
  resumirCategoricas(df)
  resumirNumericas(df)
  
  gCinfA <- crearGraficoCajas(
    df, "A_", "Competencias informacionales autopercibidas", "Dimensión",
    "Puntaje")
  
  gCinfO <- crearGraficoCajas(
    df, "O_", "Competencias informacionales observadas", "Dimensión", "Puntaje")
  
  gPDT <- crearGraficoCajas(
    df, "P_", "Prueba de Transición Universitaria", "Componente", "Puntaje")
  
  gNotas <- crearGraficoCajas(
    df, "NOTA_", "Rendimiento académico", "Asignatura", "Promedio")
  
  combinarGraficos(
    gCinfA, gCinfO, gPDT, gNotas, "Distribución de variables numéricas",
    "Descriptivas_gral.pdf")
}



################################################################################
# Funciones para estadísticas descriptivas según situación final en asignaturas
################################################################################

crearGraficoCajasSituacion <- function(
    df, prefijo, titulo, nombreX, nombreY, nombreZ) {
  # Genera un gráfico de cajas con facetas según la situación final de una
  # asignatura, utilizando las variables del dataframe que comienzan con un
  # prefijo dado y una variable dicotómica dada.
  # Entrada:
  # - df: dataframe con el conjunto de datos.
  # - prefijo: prefijo común para los nombres de las columnas de interés.
  # - titulo: título del gráfico.
  # - nombreX: nombre del eje X (variable categórica del boxplot).
  # - nombreY: nombre del eje Y (variable numérica del boxplot).
  # - nombreZ: nombre de la variable con la situación final en una asignatura.
  # Salida: gráfico generado.
  
  # Filtrar columnas con el prefijo y la situación final.
  columnas <- c(nombreZ, names(df)[startsWith(names(df), prefijo)])
  filtrado <- df %>% select(all_of(columnas))
  
  # Convertir a formato largo.
  filtrado <- filtrado %>%
    pivot_longer(cols = -all_of(nombreZ), names_to = nombreX,
                 values_to = nombreY)
  
  # Crear el gráfico.
  g <- ggboxplot(filtrado, x = nombreX, y = nombreY, fill = nombreX,
                 title = titulo) +
    facet_wrap(as.formula(paste("~", nombreZ))) +
    rremove("legend") + rotate_x_text(45) +
    scale_y_continuous(labels = formatearEjeY(titulo))
  
  return(g)
}

resumirCategoricasSituacion <- function(df) {
  # Reporta las frecuencias por situación final para todas las variables
  # categóricas de un dataframe.
  # Entrada:
  # - df: dataframe con el conjunto de datos.
  # Salida: archivo csv con el resumen.
  
  # Filtrar columnas categóricas y separar las de situación final de las de
  # ingreso.
  df <- df %>% select_if(is.factor)
  cursos <- names(df)[startsWith(names(df), "SIT_")]
  variables <- names(df)[!startsWith(names(df), "SIT_")]
  
  # Inicializar lista principal para almacenar resúmenes finales.
  resumenes <- list()
  
  for(variable in variables) {
    columnasFrecuencias <- list()
    
    for(curso in cursos) {
      # Formateo de variables.
      variableFormateada <- df[[variable]]
      cursoFormateado <- factor(df[[curso]], levels = c("APRUEBA", "REPRUEBA"))
      
      # Calcular frecuencias y llevar a formato ancho.
      tabla <- as.data.frame(table(variableFormateada, cursoFormateado))
      names(tabla) <- c("Nivel", "SIT_Nivel", "Frecuencia")
      
      tabla <- tabla %>% pivot_wider(
        names_from = SIT_Nivel, values_from = Frecuencia,
        names_prefix = paste0(curso, "."))
      
      # Renombrar columnas.
      nombres <- names(tabla)
      
      nombres <- gsub(
        paste0("^", curso, "\\.APRUEBA$"), paste0(curso, ".APRUEBA"), nombres)
      
      nombres <- gsub(
        paste0("^", curso, "\\.REPRUEBA$"), paste0(curso, ".REPRUEBA"), nombres)
      
      names(tabla) <- nombres
      columnasFrecuencias[[curso]] <- tabla
    }
    
    # Combinar los resultados.
    resultado <- columnasFrecuencias[[1]]
    
    if(length(columnasFrecuencias) > 1) {
      for(i in 2:length(columnasFrecuencias)) {
        resultado <- full_join(
          resultado, columnasFrecuencias[[i]], by = "Nivel")
      }
    }
    
    resultado <- resultado %>% mutate(Variable = variable, .before = 1)
    resumenes[[variable]] <- resultado
  }
  
  # Combinar resultados de todas las variables.
  final <- bind_rows(resumenes)
  archivo <- paste0(RUTA_RESULTADOS, "/Resumen_Categoricas_Situacion.csv")
  guardarDataframe(final, archivo)
}

resumirNumericasSituacion <- function(df, situacion) {
  # Genera un resumen de estadísticas descriptivas para variables numéricas,
  # agrupado según la situación final en una asignatura.
  # Entrada:
  # - df: dataframe con el conjunto de datos.
  # - situacion: Nombre de la variable con la situación final.
  # Salida: archivo .csv con el resumen.
  
  # Filtrar variables numéricas y situación final de la asignatura.
  columnas <- colnames(df %>% select_if(~class(.) != 'factor'))
  df <- df %>% select(all_of(c(columnas, situacion)))
  
  # Separar observaciones por situación final.
  aprobados <- df %>% filter(df[[situacion]] == "APRUEBA")
  reprobados <- df %>% filter(df[[situacion]] == "REPRUEBA")
  
  # Generar reporte de resumen.
  resultado <- NULL
  
  for(nombre in columnas) {
    # Calcular cuartiles.
    cuartilesA <- quantile(aprobados[[nombre]])
    cuartilesR <- quantile(reprobados[[nombre]])
    
    actual <- data.frame(Variable = nombre,
                         Minimo_A = round(min(aprobados[[nombre]]), 3),
                         Q1_A = round(cuartilesA[2], 3),
                         Media_A = round(mean(aprobados[[nombre]]), 3),
                         Q3_A = round(cuartilesA[4], 3),
                         Maximo_A = round(max(aprobados[[nombre]]), 3),
                         Desv_est_A = round(sd(aprobados[[nombre]]), 3),
                         Varianza_A = round(var(aprobados[[nombre]]), 3),
                         Minimo_R = round(min(reprobados[[nombre]]), 3),
                         Q1_R = round(cuartilesR[2], 3),
                         Media_R = round(mean(reprobados[[nombre]]), 3),
                         Q3_R = round(cuartilesR[4], 3),
                         Maximo_R = round(max(reprobados[[nombre]]), 3),
                         Desv_est_R = round(sd(reprobados[[nombre]]), 3),
                         Varianza_R = round(var(reprobados[[nombre]]), 3))
    
    resultado <- rbind(resultado, actual)
  }
  
  archivo <- paste0(RUTA_RESULTADOS, "/Resumen numericas ", situacion, ".csv")
  guardarDataframe(resultado, archivo)
}

reportarDescriptivasSituacion <- function(df) {
  # Reporta estadísticas descriptivas por situación final en asignaturas de
  # interés para el conjunto de datos (frecuencia para variables categóricas
  # y medidas de tendencia central y dispersión para variables numéricas) y
  # grafica variabilidad de las variables numéricas mediante gráficos de caja.
  # Entrada:
  # -df: dataframe con el conjunto de datos.
  # Salida: archivos .csv con los resúmenes y .pdf con los gráficos.
  
  cursos <- colnames(df %>% select(starts_with("SIT_")))
  resumirCategoricasSituacion(df)
  
  for(curso in cursos) {
    resumirNumericasSituacion(df, curso)
    
    subtitulos <- list(SIT_ALG = "(según situación final en Álgebra I)",
                       SIT_CAL = "(según situación final en Cálculo I)",
                       SIT_FIS = "(según situación final en Física I)",
                       SIT_PPA = "(según PPA discretizado de primer semestre)")
    
    gCinfA <- crearGraficoCajasSituacion(
      df, "A_", "Competencias informacionales autopercibidas", "Dimensión",
      "Puntaje", curso)
    
    gCinfO <- crearGraficoCajasSituacion(
      df, "O_", "Competencias informacionales observadas", "Dimensión",
      "Puntaje", curso)
    
    gPDT <- crearGraficoCajasSituacion(
      df, "P_", "Prueba de Transición Universitaria", "Componente", "Puntaje",
      curso)
    
    gNotas <- crearGraficoCajasSituacion(
      df, "NOTA_", "Rendimiento académico", "Asignatura", "Promedio", curso)
    
    combinarGraficos(
      gCinfA, gCinfO, gPDT, gNotas, "Distribución de variables numéricas",
      paste0("Descriptivas_", curso, ".pdf"), subtitulo = subtitulos[[curso]])
  }
}



################################################################################
# Funciones para análisis inferencial
################################################################################

verificarNormalidad <- function(df) {
  # Verifica normalidad de variables continuas mediante prueba de Shapiro-Wilk.
  # Entrada:
  # - df: dataframe con el conjunto de datos.
  # Salida: archivo csv con el resumen.
  
  # Filtrar variables continuas.
  df <- df %>% select_if(~class(.) != 'factor')
  df <- df %>% select(-starts_with("A_"))
  
  # Aplicar pruebas de normalidad.
  resultado <- data.frame()
  
  for(nombre in colnames(df)) {
    prueba <- shapiroWilk(df[[nombre]])
    resultado <- rbind(resultado, prueba)
  }
  
  resultado <- formatearReporte(resultado)
  archivo <- paste0(RUTA_RESULTADOS, "/Normalidad numericas.csv")
  guardarDataframe(resultado, archivo)
}

obtenerCorrelacionPredictoresNumericos <- function(df) {
  # Hace pruebas de correlación de Spearman entre predictores numéricos (puesto
  # que la mayoría no sigue una distribución normal).
  # Entrada:
  # - df: dataframe con el conjunto de datos.
  # Salida: archivo .csv con los resultados de las pruebas.
  
  df <- df %>% dplyr::select(-starts_with("NOTA_"))
  df <- df %>% dplyr::select_if(is.numeric)
  
  resultado <- data.frame()
  n <- ncol(df)
  nombres <- colnames(df)
  
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      nombre1 <- nombres[i]
      nombre2 <- nombres[j]
      
      actual <- data.frame(Variable1 = nombre1, Variable2 = nombre2)
      prueba <- spearman(df[[nombre1]], df[[nombre2]])
      actual <- cbind(actual, prueba)
      resultado <- rbind(resultado, actual)
    }
  }
  
  resultado <- formatearReporte(resultado)
  archivo <- paste0(RUTA_RESULTADOS, "/Asociacion numericas.csv")
  guardarDataframe(resultado, archivo)
}

obtenerAsociacionPredictoresCategoricos <- function(df) {
  # Hace pruebas de asociación entre predictores categóricos: Fisher si ambos
  # son dicotómicos, chi-cuadrado con bootstrapping en otro caso.
  # Entrada:
  # - df: dataframe con el conjunto de datos.
  # Salida: archivo .csv con los resultados de las pruebas.
  
  df <- df %>% select(-starts_with("SIT_"))
  df <- df %>% select_if(is.factor)
  
  resultado <- data.frame()
  n <- ncol(df)
  nombres <- colnames(df)
  
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      nombre1 <- nombres[i]
      nombre2 <- nombres[j]
      actual <- data.frame(Variable1 = nombre1, Variable2 = nombre2)
      
      if(nlevels(df[[nombre1]]) == 2 & nlevels(df[[nombre2]]) == 2) {
        prueba <- fisher(df[[nombre1]], df[[nombre2]])
        actual <- cbind(actual, prueba)
        resultado <- rbind(resultado, actual)
      } else {
        prueba <- chiCuadrado(df[[nombre1]], df[[nombre2]])
        actual <- cbind(actual, prueba)
        resultado <- rbind(resultado, actual)
      }
    }
  }
  
  resultado <- formatearReporte(resultado)
  archivo <- paste0(RUTA_RESULTADOS, "/Asociacion categoricas.csv")
  guardarDataframe(resultado, archivo)
}

obtenerAsociacionPredictoresMixtos <- function(df) {
  # Hace pruebas de asociación entre predictores numéricos y categóricos:
  # - categórica dicotómica, numérica de intervalos iguales: t-test con
  #   permutaciones para 2 muestras independientes.
  # - categórica dicotómica, numérica de intervalos desiguales: suma de rangos
  #   de Wilcoxon con permutaciones para 2 muestras independientes.
  # - categórica con más de 2 niveles: Kruskal-Wallis con permutaciones para
  #   muestras independientes.
  # Entrada: dataframe con el conjunto de datos.
  # Salida: archivo .csv con los resultados de las pruebas.
  
  df <- df %>% select(-c(starts_with("NOTA_"), starts_with("SIT")))
  numericas <- colnames(df %>% select_if(is.numeric))
  categoricas <- colnames(df %>% select_if(is.factor))
  resultado <- NULL
  
  for(nombreNum in numericas) {
    for(nombreCat in categoricas) {
      if(nlevels(df[[nombreCat]]) == 2) {
        niveles <- levels(df[[nombreCat]])
        grupo1 <- df[df[[nombreCat]] == niveles[1], nombreNum]
        grupo2 <- df[df[[nombreCat]] == niveles[2], nombreNum]
        
        if(startsWith(nombreNum, "A_")) {
          actual <- data.frame(Variable1 = nombreCat, Variable2 = nombreNum)
          prueba <- wilcoxIndep(grupo1, grupo2)
          actual <- cbind(actual, prueba)
          resultado <- rbind(resultado, actual)
        } else {
          actual <- data.frame(Variable1 = nombreCat, Variable2 = nombreNum)
          prueba <- tTestIndep(grupo1, grupo2)
          actual <- cbind(actual, prueba)
          resultado <- rbind(resultado, actual)
        }
      } else {
        actual <- data.frame(Variable1 = nombreCat, Variable2 = nombreNum)
        prueba <- kruskal(df[[nombreNum]], df[[nombreCat]])
        actual <- cbind(actual, prueba)
        resultado <- rbind(resultado, actual)
      }
    }
  }
  
  resultado <- formatearReporte(resultado)
  archivo <- paste0(RUTA_RESULTADOS, "/Asociacion mixtas.csv")
  guardarDataframe(resultado, archivo)
}

obtenerAsociacionRespuestasCategoricas <- function(df) {
  # Hace pruebas de asociación entre respuestas categóricas y predictores:
  # - predictor categórico dicotómico: Fisher.
  # - predictor categórico con más de 2 niveles: chi-cuadrado con bootstrapping.
  # - predictor numérico de intervalos desiguales: suma de rangos de Wilcoxon
  #   con permutaciones para 2 muestras independientes.
  # - predictor numérico de intervalos iguales: t-test con permutaciones para 2
  #   muestras independientes.
  # Entrada:
  # - df: dataframe con el conjunto de datos.
  # Salida: archivos .csv con los resultados de las pruebas.
  
  predictores <- colnames(df %>% select(-c(starts_with("SIT"),
                                           starts_with("NOTA"))))
  
  respuestas <- colnames(df %>% select(starts_with("SIT")))
  
  for(respuesta in respuestas) {
    resultado <- data.frame()
    
    for(predictor in predictores) {
      nombres <- data.frame(Variable1 = respuesta, Variable2 = predictor)
      
      if(is.factor(df[[predictor]])) {
        # Predictor categórico.
        if(nlevels(df[[predictor]]) == 2) {
          # Predictor dicotómico.
          prueba <- fisher(df[[respuesta]], df[[predictor]])
          actual <- cbind(nombres, prueba)
          resultado <- rbind(resultado, actual)
        } else {
          # Predictor con más de dos niveles.
          prueba <- chiCuadrado(df[[respuesta]], df[[predictor]])
          actual <- cbind(nombres, prueba)
          resultado <- rbind(resultado, actual)
        }
      } else {
        # Predictor numérico.
        niveles <- levels(df[[respuesta]])
        grupo1 <- df[df[[respuesta]] == niveles[1], predictor]
        grupo2 <- df[df[[respuesta]] == niveles[2], predictor]
        
        if(startsWith(predictor, "A_")) {
          # Escala continua de intervalos desiguales.
          prueba <- wilcoxIndep(grupo1, grupo2)
          actual <- cbind(nombres, prueba)
          resultado <- rbind(resultado, actual)
        } else {
          # Escala continua de intervalos iguales.
          prueba <- tTestIndep(grupo1, grupo2)
          actual <- cbind(nombres, prueba)
          resultado <- rbind(resultado, actual)
        }
      }
    }
    
    resultado <- formatearReporte(resultado)
    archivo <- paste0(RUTA_RESULTADOS, "/Asociacion ", respuesta, ".csv")
    guardarDataframe(resultado, archivo)
  }
}

obtenerAsociacionesRespuestasNumericas <- function(df) {
  # Hace pruebas de asociación entre respuestas numéricas y predictores:
  # - predictor categórico dicotómico: prueba t de Student con permutaciones
  #   para dos muestras independientes.
  # - predictor categórico con más de 2 niveles: Kruskal-Wallis con
  #   permutaciones para muestras independientes.
  # - predictor numérico: prueba de correlación de Spearman.
  # Entrada:
  # - df: dataframe con el conjunto de datos.
  # Salida: archivos .csv con los resultados de las pruebas.
  
  predictores <- colnames(df %>% select(-c(starts_with("SIT"),
                                           starts_with("NOTA"))))
  
  respuestas <- colnames(df %>% select(starts_with("NOTA")))
  
  for(respuesta in respuestas) {
    resultado <- data.frame()
    
    for(predictor in predictores) {
      nombres <- data.frame(Variable1 = respuesta, Variable2 = predictor)
      
      if(is.factor(df[[predictor]])) {
        # Predictor categórico.
        if(nlevels(df[[predictor]]) == 2) {
          # Predictor dicotómico.
          niveles <- levels(df[[predictor]])
          grupo1 <- df[df[[predictor]] == niveles[1], respuesta]
          grupo2 <- df[df[[predictor]] == niveles[2], respuesta]
          prueba <- tTestIndep(grupo1, grupo2)
          actual <- cbind(nombres, prueba)
          resultado <- rbind(resultado, actual)
        } else {
          # Predictor con más de dos niveles.
          prueba <- kruskal(df[[respuesta]], df[[predictor]])
          actual <- cbind(nombres, prueba)
          resultado <- rbind(resultado, actual)
        }
      } else {
        # Predictor numérico.
        prueba <- spearman(df[[respuesta]], df[[predictor]])
        actual <- cbind(nombres, prueba)
        resultado <- rbind(resultado, actual)
      }
    }
    
    resultado <- formatearReporte(resultado)
    archivo <- paste0(RUTA_RESULTADOS, "/Asociacion ", respuesta, ".csv")
    guardarDataframe(resultado, archivo)
  }
}



################################################################################
# Funciones para comparar competencias informacionales
################################################################################

compararCINF <- function(df) {
  # Hace comparaciones intra-sujeto para competencias informacionales:
  # - dimensiones de CInfA: suma de rangos con signo con permutaciones.
  # - dimensiones de CInfO: ANOVA de una vía para muestras correlacionadas con
  #   permutaciones.
  # - Comparación de una dimensión autopercibida y observada: suma de rangos con
  #   signo con permutaciones.
  # Entrada:
  # - df: data frame con el conjunto de datos.
  # Salida: archivo .csv con los resultados.
  
  # Comparar dimensiones de CInfA.
  actual <- data.frame(Variable1 = "A_BUS", Variable2 = "A_EVAL")
  prueba <- wilcoxPar(df$A_BUS, df$A_EVAL)
  resultado <- cbind(actual, prueba)

  # Comparar dimensiones de CInfO.
  filtrado <- df %>% select(starts_with("O_"))
  actual <- data.frame(Variable1 = "CInfO", Variable2 = "")
  prueba <- anovaPar(filtrado)
  actual <- cbind(actual, prueba)
  resultado <- rbind(resultado, actual)

  # Comparar CInfA y CInfO por dimensión.
  actual <- data.frame(Variable1 = "A_BUS", Variable2 = "O_BUS")
  prueba <- wilcoxPar(df$A_BUS, df$O_BUS)
  actual <- cbind(actual, prueba)
  resultado <- rbind(resultado, actual)

  actual <- data.frame(Variable1 = "A_EVAL", Variable2 = "O_EVAL")
  prueba <- wilcoxPar(df$A_BUS, df$O_BUS)
  actual <- cbind(actual, prueba)
  resultado <- rbind(resultado, actual)
  
  # Guardar el resultado.
  resultado <- formatearReporte(resultado)
  archivo <- paste0(RUTA_RESULTADOS, "/Comparaciones CInf.csv")
  guardarDataframe(resultado, archivo)
}



################################################################################
# Main
################################################################################

cluster <- activarParalelismo()

# Cargar datos.
datos <- cargarDatos()

# Obtener estadísticas descriptivas.
reportarDescriptivas(datos)

# Descartar variables con poca variabilidad (identificadas mediante
# estadísticas descriptivas).
datos <- datos %>% select(-c(NAC, GRAT, BECA, T_CARR))

# Recodificar preferencia por poca variabilidad (según se identifica en
# estadísticas descriptivas).
preferencias <- rep("SI", nrow(datos))
preferencias[as.numeric(datos$PREF) > 1] <- "NO"
datos$PREF <- as.factor(preferencias)
datos <- datos %>% rename(PREF_1 = PREF)

# Verificar normalidad de variables continuas con escala de intervalos iguales.
verificarNormalidad(datos)

# Identificar asociaciones entre predictores y descartar variables fuertemente
# asociadas.
obtenerCorrelacionPredictoresNumericos(datos)
datos <- datos %>% dplyr::select(-c(P_NEM, P_PON))
obtenerAsociacionPredictoresCategoricos(datos)
obtenerAsociacionPredictoresMixtos(datos)
datos <- datos %>% select(-DPTO)

# Identificar asociaciones entre predictores y respuestas.
reportarDescriptivasSituacion(datos)
obtenerAsociacionRespuestasCategoricas(datos)
obtenerAsociacionesRespuestasNumericas(datos)

# Guardar dataset limpio.
crearRuta(DATASET_LIMPIO)
write.csv2(datos, DATASET_LIMPIO, row.names = FALSE)

# Hacer comparaciones por competencias informacionales.
compararCINF(datos)

if(!is.null(cluster)) desactivarParalelismo(cluster)
