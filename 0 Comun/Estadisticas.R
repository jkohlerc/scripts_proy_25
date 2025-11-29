################################################################################
# Configuración
################################################################################

# Cargar funciones de apoyo.
source("Scripts/0 Comun/Utilidades.R")

# Importar paquetes.
importarPaquetes(c("combinat", "rstatix", "tidyverse"))


################################################################################
# Constantes
################################################################################

SEMILLA <- 5317
N_BOOT <- 4999
ALFA <- 0.05




################################################################################
# Funciones de apoyo.
################################################################################

reportarPrueba <- function(
    prueba, p, nombreEstadistico = NULL, valorEstadistico = NULL, grados = NULL,
    nombreEfecto = NULL, valorEfecto = NULL, inferior = NULL,
    superior = NULL) {
  # Construye un dataframe de una fila para reportar los resultados de una
  # prueba estadística.
  # Entrada:
  # - prueba: string con el nombre de la prueba.
  # - p: valor p.
  # - nombreEstadistico: string con el nombre del estadístico.
  # - valorEstadistico: valor del estadístico.
  # - grados: string formateado con los grados de libertad.
  # - nombreEfecto: string con el nombre del tamaño del efecto.
  # - valorEfecto: valor del tamaño del efecto.
  # - inferior: límite inferior del intervalo de confianza.
  # - superior: límite superior del intervalo de confianza.
  # Salida: dataframe con el reporte:
  # - Prueba: string con el nombre de la prueba.
  # - nombreEstadistico: string con el nombre del estadístico.
  # - valorEstadistico: valor del estadístico.
  # - p: valor p.
  # - inferior: límite inferior del intervalo de confianza.
  # - superior: límite superior del intervalo de confianza.
  # - Grados: grados de libertad.
  # - nombreEfecto: string con el nombre del tamaño del efecto.
  # - valorEfecto: valor del tamaño del efecto.

  # Formatear valores para reporte.
  nombreEstadistico <- if(is.null(nombreEstadistico)) {
    NA_character_
  } else {
    nombreEstadistico
  }
  
  valorEstadistico <- if(is.null(valorEstadistico)) {
    NA_real_
  } else {
    valorEstadistico
  }
  
  inferior <- if(is.null(inferior)) NA_real_ else inferior
  superior <- if(is.null(superior)) NA_real_ else superior
  grados <- if(is.null(grados)) NA_character_ else grados
  
  nombreEfecto <- if(is.null(nombreEfecto)) {
    NA_character_
  } else {
    nombreEfecto
  }
  
  valorEfecto <- if(is.null(valorEfecto)) {
    NA_real_
  } else {
    valorEfecto
  }
  
  # Armar dataframe.
  resultado <- data.frame(
    Prueba = prueba, nombreEstadistico = nombreEstadistico,
    valorEstadistico = valorEstadistico, p = p, inferior = inferior,
    superior = superior, Grados = grados, nombreEfecto = nombreEfecto,
    valorEfecto = valorEfecto)
  
  return(as.data.frame(resultado))
}

formatearReporte <- function(reporte, alfa = ALFA) {
  # Da el formato definitivo a una tabla de reporte de pruebas estadísticas.
  # Entrada:
  # - reporte: dataframe con la tabla.
  # - alfa: nivel de significación.
  # Salida: reporte modificado.
  
  # Formatear valor p.
  reporte <- reporte %>%
    mutate(p = ifelse(p < alfa,
                      paste0(formatearFlotante(p), "*"),
                      formatearFlotante(p)))
  
  # Formatear columnas con valores reales.
  reporte <- reporte %>%
    mutate(across(where(is.numeric), formatearFlotante))
  
  # Formatear estadístico.
  reporte$Estadistico <- paste(
    reporte$nombreEstadistico,
    formatearFlotante(reporte$valorEstadistico),
    sep = "="
  )
  reporte <- reporte %>% relocate(Estadistico, .after = Prueba)
  reporte <- reporte %>% select(-c(nombreEstadistico, valorEstadistico))
  
  # Formatear intervalo de confianza.
  reporte <- reporte %>% rename(IC_inf = inferior, IC_sup = superior)
  
  # Formatear tamaño del efecto.
  reporte$Efecto <- paste(reporte$nombreEfecto, reporte$valorEfecto, sep = "=")
  reporte <- reporte %>% select(-c(nombreEfecto, valorEfecto))
  
  # Reemplazar valores faltantes por strings vacíos.
  reporte <- reporte %>%
    mutate(across(everything(), ~ replace(.x, is.na(.x), "")))
  
  # Ordenar de manera ascendente por valor p, respetando la asociación entre
  # ómnibus y post-hoc.
  reporte <- reporte %>%
    mutate(
      esPH = grepl("^PH", Prueba),
      bloque = cumsum(!grepl("^PH", Prueba))
    )
  
  reporte <- reporte %>%
    group_by(bloque) %>%
    arrange(esPH, as.numeric(
      str_replace_all(p, "[^0-9\\.]", "")), .by_group = TRUE) %>%
    ungroup()
  
  pOmnibus <- reporte %>%
    filter(!esPH) %>%
    mutate(pOrden = as.numeric(str_replace_all(p, "[^0-9\\.]", ""))) %>%
    select(bloque, pOrden)
  
  reporte <- reporte %>%
    left_join(pOmnibus, by = "bloque") %>%
    arrange(pOrden, bloque, esPH) %>%
    select(-pOrden, -bloque, -esPH)
  
  # Limpiar el reporte.
  reporte <- reporte %>%
    mutate(
      Estadistico = str_replace_all(Estadistico, "NA=|\\=NA", ""),
      Efecto = str_replace_all(Efecto, "NA=|\\=NA", ""),
      Estadistico = str_replace_all(Estadistico, "\\=$|^=", ""),
      Efecto = str_replace_all(Efecto, "\\=$|^=", "")
    )
  
  conservar <- sapply(reporte, function(columna) {!all(columna == "")})
  reporte <- reporte[, conservar, drop = FALSE]
  
  rownames(reporte) <- NULL
  return(as.data.frame(reporte))
}



################################################################################
# Máscaras para uniformar reporte de resultados.
################################################################################

chiCuadrado <- function(
    categorica1, categorica2, R = N_BOOT, semilla = SEMILLA) {
  # Hace una prueba chi-cuadrado con simulación de montecarlo y calcula el
  # tamaño del efecto mediante V de Cramer.
  # Entrada:
  # - categorica1, categorica2: vectores correspondientes a variables
  # - R: cantidad de repeticiones.
  # - semilla: semilla.
  # Salida: dataframe con el resultado.
  
  options(warn = -1)
  
  # Obtener grados de libertad.
  prueba <- chisq.test(categorica1, categorica2, simulate.p.value = FALSE)
  grados <- as.character(prueba$parameter)
  
  # Calcular el tamaño del efecto.
  tabla <- table(categorica1, categorica2)
  efecto <- rstatix::cramer_v(tabla)
  
  # Hacer la prueba con bootstrapping.
  set.seed(semilla)
  prueba <- chisq.test(categorica1, categorica2, simulate.p.value = TRUE, B = R)
  
  resultado <- reportarPrueba(
    prueba = "x2 (M)", p = prueba$p.value, nombreEstadistico = "x2",
    valorEstadistico = prueba$statistic, grados = grados,
    nombreEfecto = "V", valorEfecto = efecto)
  
  options(warn = 0)
  return(resultado)
}

durbinWatson <- function(modeloLineal, R = N_BOOT, semilla = SEMILLA) {
  # Hace una prueba autocorrelación (de primer orden) de Durbin-Watson.
  # Entrada:
  # - modeloLineal: modelo de regresión lineal.
  # - R: cantidad de repeticiones.
  # - semilla: semilla.
  # Salida: dataframe con el resultado.
  
  set.seed(semilla)
  prueba <- durbinWatsonTest(modeloLineal, simulate = TRUE, reps = N_BOOT)
  
  # Reportar resultado.
  resultado <- reportarPrueba(
    prueba = "Durbin-Watson (b)", p = prueba$p, nombreEstadistico = "D-W",
    valorEstadistico = prueba$dw, valorEfecto = prueba$r, nombreEfecto = "rho")
  
  return(resultado)
}

fisher <- function(categorica1, categorica2, alfa = ALFA) {
  # Hace una prueba exacta de Fisher.
  # Entrada:
  # - categorica1, categorica2: vectores correspondientes a variables
  #   dicotómicas.
  # - alfa: nivel de significación.
  # Salida: dataframe con el resultado.
  
  prueba <- fisher.test(
    categorica1, categorica2, conf.level = 1 - alfa, alternative = "two.sided")
  
  resultado <- reportarPrueba(
    "Fisher", p = prueba$p.value, inferior = prueba$conf.int[1],
    superior = prueba$conf.int[2], nombreEfecto = "Odds ratio",
    valorEfecto = prueba$estimate)
  
  return(resultado)
}

ncv <- function(modeloLineal) {
  # Hace una prueba de puntaje de varianza inconstante.
  # Entrada:
  # - modeloLineal: modelo de regresión lineal.
  # Salida: dataframe con el resultado.
  
  prueba <- ncvTest(modeloLineal)
  
  # Reportar resultado.
  resultado <- reportarPrueba(
    prueba = "NCV", p = prueba$p, nombreEstadistico = "x2",
    valorEstadistico = prueba$ChiSquare, grados = prueba$Df)
  
  return(resultado)
}

rocTest <- function(
    curvaBase, curvaFinal, B = N_BOOT, alfa = ALFA, semilla = SEMILLA) {
  # Compara dos curvas ROC pareadas con el método de Venkatraman.
  # Entrada:
  # - curvaBase, curvaFinal: curvas ROC de los modelos base y final,
  #   construidas con los mismos pliegues.
  # - B: cantidad de repeticiones.
  # - alfa: nivel de significación.
  # - semilla: semilla.
  # Salida: dataframe con el resultado.
  
  set.seed(semilla)
  
  prueba <- roc.test(
    curvaBase, curvaFinal, method = "venkatraman", paired = TRUE, boot.n = B,
    conf.level = 1 - alfa)
  
  resultado <- reportarPrueba(
    "Venkatraman (b)", p = prueba$p.value, nombreEstadistico = "E",
    valorEstadistico = prueba$statistic, nombreEfecto = "delta AUC-ROC",
    valorEfecto = auc(curvaFinal) - auc(curvaBase))
  
  return(resultado)
}

shapiroWilk <- function(vector) {
  # Hace una prueba de Shapiro-Wilk.
  # Entrada:
  # - vector: vector numérico.
  # Salida: dataframe con el resultado.
  
  prueba <- shapiro.test(vector)
  
  resultado <- reportarPrueba(
    "Shapiro-Wilk", p = prueba$p.value, nombreEstadistico = "W",
    valorEstadistico = prueba$statistic)
  
  return(resultado)
}

spearman <- function(numerica1, numerica2, alfa = ALFA) {
  # Hace una prueba de correlación de Spearman.
  # Entrada:
  # - numerica1, numerica2: vectores correspondientes a variables numéricas.
  # - alfa: nivel de significación.
  # Salida: dataframe con el resultado. 
  
  options(warn = -1)
  
  prueba <- cor.test(
    numerica1, numerica2, method = "spearman", conf.level = 1 - alfa,
    alternative = "two.sided")
  
  resultado <- reportarPrueba(
    prueba = "Spearman", p = prueba$p.value, nombreEstadistico = "S",
    valorEstadistico = prueba$statistic, nombreEfecto = "rho",
    valorEfecto = prueba$estimate)
  
  options(warn = 0)
  return(resultado)
}



################################################################################
# Funciones con remuestreo para pruebas paramétricas.
################################################################################

anovaPar <- function(
    valores, R = N_BOOT, alfa = ALFA, semilla = SEMILLA) {
  # Hace la prueba ANOVA para muestras pareadas con permutaciones y reporta sus
  # resultados, incluyendo el intervalo de confianza en el procedimiento
  # post-hoc (si corresponde).
  # Entrada:
  # - valores: dataframe con las columnas correspondientes a las muestras
  #   pareadas.
  # - R: cantidad de permutaciones.
  # - alfa: nivel de significación.
  # - semilla: semilla.
  # Salida: dataframe con el resultado de la prueba (incluyendo procedimiento
  # post-hoc si corresponde).
  
  ##############################################################################
  # Funciones de apoyo.
  ##############################################################################
  
  permutar <- function(df) {
    # Obtiene una permutación de los valores observados intra-sujeto.
    # Entrada:
    # - df: dataframe en formato largo.
    # Salida: vector de valores permutado.
    
    permutacion <- do.call(rbind, lapply(split(df, df$id), function(id) {
      id$valor <- sample(id$valor)
      id
    }))

    permutacion <- permutacion[order(as.numeric(permutacion$id)), ]
    return(permutacion$valor)
  }
  
  
  
  ##############################################################################
  # Función principal.
  ##############################################################################
  
  # Llevar los datos a formato largo.
  niveles <- colnames(valores)
  df <- data.frame(id = as.factor(1:nrow(valores)), valores)
  df <- tidyr::pivot_longer(df, -id, names_to = "grupo", values_to = "valor")
  df$grupo <- factor(df$grupo)
  df$id <- factor(df$id)
  
  # Obtener estadístico observado, tamaño del efecto y grados de libertad.
  prueba <- rstatix::anova_test(
    df, dv = valor, wid = id, within = grupo, effect.size = "pes")
  
  observado <- prueba$ANOVA$F
  efecto <- prueba$ANOVA$pes
  grados <- paste0("Grupos: ", prueba$ANOVA$DFn, ", Error: ", prueba$ANOVA$DFd)
  
  # Obtener permutaciones intrasujeto.
  set.seed(semilla)
  
  permutaciones <- vector("list", R)
  for (i in 1:R) {
    permutaciones[[i]] <- permutar(df)
  }
  
  # Obtener distribución del estadístico.
  distribucion <- foreach::foreach(actual = permutaciones, .combine = c) %do% { 
    dfPermutado <- df
    dfPermutado$valor <- actual
    
    # Calcular F para la permutación.
    prueba <- rstatix::anova_test(
      dfPermutado, dv = valor, wid = id, within = grupo, detailed = FALSE)
    
    prueba$ANOVA$F
  }
  
  # Calcular valor p.
  p <- (sum(distribucion >= observado) + 1) / (R + 1)
  
  # Empaquetar resultado.
  omnibus <- reportarPrueba(
    prueba = "ANOVA par. (M)", p = p, nombreEstadistico = "F",
    valorEstadistico = observado, grados = grados, nombreEfecto = "eta2_p", 
    valorEfecto = efecto)

  # Hacer procedimiento post-hoc si la prueba ómnibus es significativa.
  if(p < alfa) {
    postHoc <- data.frame()
    pares <- combn(niveles, 2, simplify = FALSE)
    
    for(par in pares) {
      # Obtener los grupos a comparar.
      nombre1 <- par[1]
      nombre2 <- par[2]
      grupo1 <- valores[[nombre1]]
      grupo2 <- valores[[nombre2]]
      
      # Calcular estadístico observado, intervalo de confianza y tamaño del
      # efecto.
      prueba <- t.test(
        grupo1, grupo2, paired = TRUE, conf.int = TRUE, conf.level = 1 - alfa)
      
      observado <- prueba$statistic
      efecto <- mean(grupo1 - grupo2) / sd(grupo1 - grupo2)
      intervalo <- prueba$conf.int
      
      # Calcular el valor p.
      distribucion <- foreach::foreach(
        actual = permutaciones, .combine = c) %do% { 
          dfActual <- df
          dfActual$valor <- actual
          grupo1Permutado <- actual[df$grupo == nombre1]
          grupo2Permutado <- actual[df$grupo == nombre2]
          
          t.test(
            grupo1Permutado, grupo2Permutado, paired = TRUE,
            conf.level = 1 - alfa)$statistic
        }
      
      mediana <- median(distribucion, na.rm = TRUE)
      
      p <- (sum(abs(distribucion - mediana) >= abs(observado - mediana),
                na.rm = TRUE) + 1) / (R + 1)
      
      # Empaquetar el resultado.
      resultado <- reportarPrueba(
        prueba = paste0("PH: ", nombre1, "-", nombre2, " t par. (M)"),
        p = p, nombreEstadistico = "t", valorEstadistico = observado,
        inferior = intervalo[1], superior = intervalo[2],
        nombreEfecto = "Diff. Media", valorEfecto = efecto)
      
      postHoc <- rbind(postHoc, resultado)
    }
    
    # Ajustar valores p.
    postHoc$p <- p.adjust(postHoc$p, method = "BH")
    reporte <- rbind(omnibus, postHoc)
  } else {
    reporte <- omnibus
  }
  
  return(reporte)
}

tTestIndep <- function(
    grupo1, grupo2, R = N_BOOT, alfa = ALFA, semilla = SEMILLA) {
  # Realiza una prueba t de Student para dos muestras independientes con
  # permutaciones,
  # reportando el valor p y el Intervalo de Confianza (IC) por inversión.
  # Entrada:
  # - grupo1, grupo2: vectores de datos.
  # - R: cantidad de permutaciones.
  # - alfa: nivel de significación (para el IC: 1 - alfa).
  # - semilla: semilla.
  # Salida: dataframe con el resultado de la prueba.
  
  ################################################################################
  # Funciones de apoyo.
  ################################################################################
  
  calcularEfecto <- function(grupo1, grupo2) {
    # Calcula el tamaño del efecto usando d de Cohen.
    # Entrada:
    # - grupo1, grupo2: vectores de datos.
    # Salida: tamaño del efecto.
    
    n1 <- length(grupo1)
    n2 <- length(grupo2)
    var1 <- var(grupo1)
    var2 <- var(grupo2)
    varAgrupada <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
    dCohen <- (mean(grupo1) - mean(grupo2)) / varAgrupada
    return(dCohen)
  }
  
  calcularIntervalo <- function(
    funcionP, grupo1, grupo2, alfa = ALFA, R = N_BOOT, semilla = SEMILLA) {
    # Calcula el intervalo de confianza para la diferencia de medias.
    # Entrada:
    # - funcionP: función que calcula el valor p con desplazamiento
    # - grupo1: vector con los valores de la primera muestra.
    # - grupo2: vector con los valores de la segunda muestra.
    # - alfa: nivel de significación.
    # - R: cantidad de permutaciones.
    # - semilla: semilla.
    # Salida: vector con los límites del intervalo de confianza.
    
    # Calcular diferencia de medias.
    diferenciaObservada <- mean(grupo1) - mean(grupo2)
    
    # Función objetivo para uniroot: busca la raíz donde p(delta) - ALPHA = 0.
    funcionLimite <- function(delta) {
      p <- funcionP(grupo1, grupo2, delta, R = R, semilla = semilla)
      
      return(p - alfa)
    }
    
    # Estimación del rango de búsqueda (para estabilidad de uniroot).
    error <- sqrt(var(grupo1)/length(grupo1) + var(grupo2)/length(grupo2))
    
    rangoBusqueda <- c(
      diferenciaObservada - 3.5 * error, diferenciaObservada + 3.5 * error)
    
    intervalo <- c(NA, NA)
    
    # Búsqueda de raíces con manejo de errores.
    tryCatch({
      # Límite inferior.
      intervalo[1] <- uniroot(
        funcionLimite, interval = c(rangoBusqueda[1], diferenciaObservada),
        tol = 1e-4)$root
      
      # Límite superior.
      intervalo[2] <- uniroot(
        funcionLimite, interval = c(diferenciaObservada, rangoBusqueda[2]),
        tol = 1e-4)$root
    }, error = function(e) {
      warning(paste("IC de Inversión fallido (uniroot). Razón:", e$message))
    })
    
    return(intervalo)
  }
  
  calcularP <- function(grupo1, grupo2, delta, R, semilla) {
    # Calcula el valor p a partir de las permutaciones.
    # Entrada:
    # - grupo1, grupo2: vectores de datos.
    # - delta: desplazamiento de la media para inversión de la prueba.
    # - R: número de permutaciones (solo independiente).
    # - semilla: semilla para números aleatorios.
    # Salida: valor p.
    
    diferenciaObservada <- mean(grupo1) - mean(grupo2)
    diferenciaDesplazada <- diferenciaObservada - delta
    combinada <- c(grupo1, grupo2)
    
    grupos <- factor(
      c(rep("A", length(grupo1)), rep("B", length(grupo2))))
    
    diferencias <- foreach::foreach(
      i = 1:R, .combine = 'c', .options.RNG = list(seed = semilla)) %dorng% {
        permutaciones <- sample(grupos)
        grupo1Permutado <- combinada[permutaciones == "A"]
        grupo2Permutado <- combinada[permutaciones == "B"]
        mean(grupo1Permutado) - mean(grupo2Permutado)
      }
    
    p <- (sum(abs(diferencias) >= abs(diferenciaDesplazada)) + 1) / (R + 1)
    return(p)
  }
  
  
  
  ################################################################################
  # Función principal.
  ################################################################################
  
  # Obtener valor p (H0: delta = 0).
  p <- calcularP(grupo1, grupo2, delta = 0, R = R, semilla = semilla)
  
  # Calcular intervalo de confianza por inversión (Good, 2004).
  intervalo <- calcularIntervalo(
    calcularP, grupo1, grupo2, alfa = alfa, R = R, semilla = semilla)
  
  # Obtener estadístico observado y los grados de libertad.
  prueba <- t.test(grupo1, grupo2, paired = FALSE, var.equal = TRUE)
  observado <- prueba$statistic
  grados <- formatearFlotante(prueba$parameter)
  
  # Calcular y formatear tamaño del efecto.
  dCohen <- calcularEfecto(grupo1, grupo2)
  
  # Empaquetar resultado.
  prueba <- "t indep. (M)"
  
  resultado <- reportarPrueba(
    prueba = "t indep. (M)", p = p, nombreEstadistico = "t",
    valorEstadistico = observado, grados = grados,
    nombreEfecto = "d", valorEfecto = dCohen, inferior = intervalo[1],
    superior = intervalo[2])
  
  return(resultado)
}

tTestPar <- function(
    grupo1, grupo2, R = N_BOOT, alfa = ALFA, semilla = SEMILLA) {
  # Realiza una prueba t de Student para dos muestras pareadas con
  # permutación de signos, reportando el valor p y el IC por inversión.
  # Entrada:
  # - grupo1, grupo2: vectores de datos.
  # - R: cantidad de remuestreos.
  # - alfa: nivel de significación (para el IC).
  # - semilla: semilla.
  # Salida: dataframe con el resultado de la prueba.
  
  ################################################################################
  # Funciones de apoyo.
  ################################################################################
  
  calcularEfecto <- function(grupo1, grupo2) {
    # Calcula el tamaño del efecto usando d de Cohen.
    # Entrada:
    # - grupo1, grupo2: vectores de datos.
    # Salida: tamaño del efecto.
    
    diferencias <- grupo1 - grupo2
    media <- mean(diferencias)
    desviacion <- sd(diferencias)
    dCohen <- media / desviacion
    return(dCohen)
  }
  
  calcularIntervalo <- function(
    funcionP, grupo1, grupo2, alfa = ALFA, R = N_BOOT, semilla = SEMILLA) {
    # Calcula el intervalo de confianza para la media de diferencias.
    # Entrada:
    # - funcionP: función que calcula el valor p con desplazamiento
    # - grupo1: vector con los valores de la primera muestra.
    # - grupo2: vector con los valores de la segunda muestra.
    # - alfa: nivel de significación.
    # - R: cantidad de permutaciones.
    # - semilla: semilla.
    # Salida: vector con los límites del intervalo de confianza.
    
    # Calcular diferencias.
    diferencias <- grupo1 - grupo2
    diferenciaObservada <- mean(diferencias)
    
    # Función objetivo para uniroot: busca la raíz donde p(delta) - ALPHA = 0.
    funcionLimite <- function(delta) {
      p <- funcionP(grupo1, grupo2, delta, R = R, semilla = semilla)
      
      return(p - alfa)
    }
    
    # Estimación del rango de búsqueda (para estabilidad de uniroot).
    error <- sd(diferencias) / sqrt(length(diferencias))
    
    rangoBusqueda <- c(
      diferenciaObservada - 3.5 * error, diferenciaObservada + 3.5 * error)
    
    intervalo <- c(NA, NA)
    
    # Búsqueda de raíces con manejo de errores.
    tryCatch({
      # Límite inferior.
      intervalo[1] <- uniroot(
        funcionLimite, interval = c(rangoBusqueda[1], diferenciaObservada),
        tol = 1e-4)$root
      
      # Límite superior.
      intervalo[2] <- uniroot(
        funcionLimite, interval = c(diferenciaObservada, rangoBusqueda[2]),
        tol = 1e-4)$root
    }, error = function(e) {
      warning(paste("IC de Inversión fallido (uniroot). Razón:", e$message))
    })
    
    return(intervalo)
  }
  
  calcularP <- function(grupo1, grupo2, delta, R, semilla) {
    # Calcula el valor p a partir de las permutaciones.
    # Entrada:
    # - grupo1, grupo2: vectores de datos.
    # - delta: desplazamiento de la media para inversión de la prueba.
    # - R: número de permutaciones (solo independiente).
    # - semilla: semilla para números aleatorios.
    # Salida: valor p.
    
    diferencias <- grupo1 - grupo2
    diferenciasDesplazadas <- diferencias - delta 
    observado <- mean(diferenciasDesplazadas) 
    
    medias <- foreach::foreach(
      i = 1:R, .combine = 'c', .options.RNG = list(seed = semilla)) %dorng% { 
        signos <- sample(
          c(-1, 1), size = length(diferenciasDesplazadas), replace = TRUE)
        
        mean(signos * diferenciasDesplazadas)
      }
    
    p <- (sum(abs(medias) >= abs(observado)) + 1) / (R + 1)
    return(p)
  }
  
  
  
  ################################################################################
  # Función principal.
  ################################################################################
  
  # Eliminar pares incompletos.
  dfTest <- data.frame(g1 = grupo1, g2 = grupo2)
  dfTest <- na.omit(dfTest)
  grupo1 <- dfTest$g1
  grupo2 <- dfTest$g2
  
  # Obtener valor p (H0: delta = 0).
  p <- calcularP(grupo1, grupo2, delta = 0, R = R, semilla = semilla)
  
  # Calcular intervalo de confianza por inversión (Good, 2004).
  intervalo <- calcularIntervalo(
    calcularP, grupo1, grupo2, alfa = alfa, R = R, semilla = semilla)
  
  # Obtener estadístico observado y los grados de libertad.
  prueba <- t.test(grupo1, grupo2, paired = TRUE)
  observado <- prueba$statistic
  grados <- as.character(as.integer(prueba$parameter))
  
  # Calcular y formatear tamaño del efecto.
  dCohen <- calcularEfecto(grupo1, grupo2)
  
  # Empaquetar resultado.
  resultado <- reportarPrueba(
    prueba = "t par. (M)", p = p, nombreEstadistico = "t",
    valorEstadistico = observado, grados = grados,
    nombreEfecto = "d", valorEfecto = dCohen, inferior = intervalo[1],
    superior = intervalo[2])
  
  return(resultado)
}


################################################################################
# Funciones con remuestreo para pruebas no paramétricas.
################################################################################

kruskal <- function(
    valores, grupos, R = N_BOOT, alfa = ALFA, semilla = SEMILLA) {
  # Realiza una prueba de Kruskal-Wallis con permutaciones y, si es
  # significativa, un procedimiento posthoc correspondiente con pruebas de suma
  # de rangos con signo (sobre las mismas permutaciones) y ajuste de
  # Benjamini-Hochsberg.
  # Entrada:
  # - valores: vector numérico.
  # - grupos: vector categórico.
  # - R: número de permutaciones.
  # - alfa: nivel de significación.
  # - semilla: semilla para reproducibilidad.
  # Salida: dataframe con el resultado de la prueba.
  
  # Preparar dataframe base.
  df <- data.frame(valor = valores, grupo = factor(grupos))
  
  # Obtener estadístico observado y grados de libertad.
  prueba <- rstatix::kruskal_test(df, valor ~ grupo)
  observado <- as.numeric(prueba$statistic)
  grados <- as.numeric(prueba$df)
  
  # Calcular intervalo de confianza y tamaño del efecto.
  prueba <- rstatix::kruskal_effsize(
    df, valor ~ grupo, ci = TRUE, conf.level = 1 - alfa, ci.type = "perc",
    nboot = R)
  
  efecto <- as.numeric(prueba$effsize)
  intervalo <- c(as.numeric(prueba$conf.low), as.numeric(prueba$conf.high))
  
  # Obtener permutaciones.
  set.seed(semilla)
  permutaciones <- replicate(R, sample(grupos), simplify = FALSE)
  
  # Obtener distribución del estadístico.
  distribucion <- foreach(p = permutaciones, .combine = c) %dorng% {
    permutacion <- data.frame(valor = valores, grupo = factor(p))
    rstatix::kruskal_test(permutacion, valor ~ grupo)$statistic
  }
  
  # Calcular valor p.
  p <- (sum(distribucion >= observado) + 1) / (R + 1)
  
  # Empaquetar resultado.
  omnibus <- reportarPrueba(
    prueba = "Kruskal-Wallis (M)", p = p, nombreEstadistico = "x2",
    valorEstadistico = observado, inferior = intervalo[1],
    superior = intervalo[2], grados = grados, nombreEfecto = "eta2", 
    valorEfecto = efecto)
  
  # Hacer post-hoc si prueba ómnibus es significativa.
  if(p < alfa) {
    niveles <- levels(grupos)
    postHoc <- data.frame()
    pares <- combn(niveles, 2, simplify = FALSE)
    
    # Obtener valor observado, tamaño del efecto e intervalo de confianza.
    for(par in pares) {
      # Obtener los grupos a comparar.
      nombre1 <- par[1]
      nombre2 <- par[2]
      grupo1 <- valores[which(grupos == nombre1)]
      grupo2 <- valores[which(grupos == nombre2)]
      
      # Calcular estadístico observado, intervalo de confianza y tamaño del
      # efecto.
      prueba <- wilcox.test(
        grupo1, grupo2, paired = FALSE, exact = FALSE, conf.int = TRUE,
        conf.level = 1 - alfa, correct = FALSE)
      
      observado <- prueba$statistic
      efecto <- prueba$estimate
      intervalo <- prueba$conf.int
      
      # Calcular el valor p.
      distribucion <- foreach::foreach(
        actual = permutaciones, .combine = c) %dopar% {
          grupo1Permutado <- valores[actual == nombre1]
          grupo2Permutado <- valores[actual == nombre2]
          
          wilcox.test(
            grupo1Permutado, grupo2Permutado, paired = FALSE, exact = FALSE,
            correct = FALSE)$statistic
        }
      
      mediana <- median(distribucion, na.rm = TRUE)
      p <- (sum(abs(distribucion - mediana) >= abs(observado - mediana),
                na.rm = TRUE) + 1) / (R + 1)
      
      # Empaquetar el resultado.
      resultado <- reportarPrueba(
        prueba = paste0("PH: ", nombre1, "-", nombre2, " Wilcoxon indep. (M)"),
        p = p, nombreEstadistico = "W", valorEstadistico = observado,
        inferior = intervalo[1], superior = intervalo[2],
        nombreEfecto = "Dif. HL", valorEfecto = efecto)
      
      postHoc <- rbind(postHoc, resultado)
    }
    
    # Ajustar valores p.
    postHoc$p <- p.adjust(postHoc$p, method = "BH")
    reporte <- rbind(omnibus, postHoc)
  } else {
    reporte <- omnibus
  }
  
  return(reporte)
}

wilcoxIndep <- function(
    grupo1, grupo2, R = N_BOOT, alfa = ALFA, semilla = SEMILLA) {
  # Realiza una prueba de suma de rangos (dos muestras independientes) con
  # permutaciones.
  # Entrada:
  # - grupo1, grupo2: vectores de datos.
  # - R: cantidad de remuestreos.
  # - alfa: nivel de significación.
  # - semilla: semilla.
  # Salida: dataframe con el resultado de la prueba.
  
  ##############################################################################
  # Funciones de apoyo
  ##############################################################################
  
  calcularP <- function(
    combinada, observado, n1, R = N_BOOT, semilla = SEMILLA) {
    # Calcula el valor p a partir de las permutaciones.
    # Entrada:
    # - combinada: vector con la muestra combinada.
    # - observado: valor observado para el estadístico.
    # - n1: tamaño de la primera muestra.
    # - R: número de permutaciones.
    # - semilla: semilla para números aleatorios.
    # Salida: valor p.
    
    # Generar distribución nula.
    distribucion <- foreach::foreach(
      i = 1:R, .combine = 'c', .options.RNG = list(seed = semilla),
      .packages = "stats") %dorng% {
        # Samplear índices para mantener tamaño de grupo
        indices <- sample(length(combinada), n1, replace = FALSE)
        grupo1Permutado <- combinada[indices]
        grupo2Permutado <- combinada[-indices]
        
        W <- wilcox.test(grupo1Permutado, grupo2Permutado,
                         paired = FALSE, exact = FALSE,
                         correct = FALSE)$statistic
        W
      }
    
    # Calcular valor p.
    mediana <- median(distribucion)
    
    p <- (sum(abs(distribucion - mediana) >= abs(observado - mediana)) + 1) /
      (R + 1)
    
    return(p)
  }
  
  
  
  ##############################################################################
  # Función principal
  ##############################################################################
  
  # Obtener estadístico observado, tamaño del efecto de Hodges-Lehmann e
  # intervalo de confianza de Hodges-Lehmann.
  prueba <- wilcox.test(
    grupo1, grupo2, paired = FALSE, exact = FALSE, conf.int = TRUE,
    conf.level = 1 - alfa, correct = FALSE)
  
  observado <- as.numeric(prueba$statistic)
  efecto <- as.numeric(prueba$estimate)
  intervalo <- as.numeric(prueba$conf.int)
  
  # Calcular el valor p.
  n1 <- length(grupo1)
  n2 <- length(grupo2)
  combinada <- c(grupo1, grupo2)
  p <- calcularP(combinada, observado, n1, R, semilla)
  
  # Empaquetar resultado.
  resultado <- reportarPrueba(
    prueba = "Wilcoxon indep. (M)", p = as.numeric(p), nombreEstadistico = "W", 
    valorEstadistico = observado, nombreEfecto = "Dif. HL",
    valorEfecto = efecto, inferior = intervalo[1], superior = intervalo[2])
  
  return(resultado)
}

wilcoxPar <- function(
    grupo1, grupo2, R = N_BOOT, alfa = ALFA, semilla = SEMILLA) {
  # Realiza una prueba de suma de rangos con signo (dos muestras pareadas) con
  # permutaciones.
  # Entrada:
  # - grupo1, grupo2: vectores de datos emparejados.
  # - R: cantidad de remuestreos (permuta signos).
  # - alfa: nivel de significación.
  # - semilla: semilla.
  # Salida: dataframe con el resultado de la prueba.
  
  ##############################################################################
  # Funciones de apoyo
  ##############################################################################
  
  calcularP <- function(
    grupo1, grupo2, observado, delta = 0, R = N_BOOT, semilla = SEMILLA) {
    # Calcula el valor p para la prueba pareada mediante permutación de signos.
    # Entrada:
    # - grupo1, grupo2: vectores de datos emparejados.
    # - observado: valor observado del estadístico.
    # - delta: desplazamiento.
    # - R: número de permutaciones.
    # - semilla: semilla para números aleatorios.
    # Salida: valor p.
    
    # Calcular diferencias.
    diferencias <- grupo2 - grupo1 - delta
    n <- length(diferencias)
    
    # Generar distribución nula por permutación de signos.
    distribucion <- foreach::foreach(
      i = 1:R, .combine = 'c', .options.RNG = list(seed = semilla),
      .packages = "stats") %dorng% {
        signos <- sample(c(-1, 1), n, replace = TRUE)
        diferenciasPermutadas <- diferencias * signos
        
        rangos <- rank(abs(diferenciasPermutadas))
        W <- sum(rangos[diferenciasPermutadas > 0])
        W
      }
    
    # Calcular valor p.
    mediana <- median(distribucion, na.rm = TRUE)
    
    p <- (sum(abs(distribucion - mediana) >= abs(observado - mediana),
              na.rm = TRUE) + 1) / (R + 1)
    
    p <- max(min(p, 1), 0)
    return(p)
  }
  
  
  
  ##############################################################################
  # Función principal
  ##############################################################################
  
  # Obtener estadístico observado, tamaño del efecto de Hodges-Lehmann e
  # intervalo de confianza de Hodges-Lehmann.
  prueba <- wilcox.test(
    grupo1, grupo2, paired = TRUE, exact = FALSE, conf.int = TRUE,
    conf.level = 1 - alfa, correct = FALSE)
  
  observado <- as.numeric(prueba$statistic)
  efecto <- as.numeric(prueba$estimate)
  intervalo <- as.numeric(prueba$conf.int)
  
  # Calcular valor p.
  p <- calcularP(grupo1, grupo2, observado, delta = 0, R = R, semilla = semilla)
  
  # Empaquetar resultado.
  resultado <- reportarPrueba(
    prueba = "Wilcoxon par. (M)", p = as.numeric(p), nombreEstadistico = "W",
    valorEstadistico = observado, nombreEfecto = "Dif. HL",
    valorEfecto = efecto, inferior = intervalo[1], superior = intervalo[2])
  
  return(resultado)
}



################################################################################
# Función con remuestreo para comparar curvas PR.
################################################################################

prTest <- function(base, final, B = N_BOOT, semilla = SEMILLA) {
  # Compara dos curvas PR mediante una prueba Z bilateral pareada con
  # bootstrapping.
  # Entrada:
  # - base: modelo base (solo variables de ingreso).
  # - final: modelo final (con competencias informacionales).
  # - B: cantidad de repeticiones.
  # - alfa: nivel de significación.
  # - semilla: semilla.
  # Salida: dataframe con el resultado.
  
  # Extraer vectores originales.
  observados <- ifelse(base$predicciones$Observado == "REPRUEBA", 1, 0)
  puntajesBase <- base$predicciones$Prob_REPRUEBA
  puntajesFinal <- final$predicciones$Prob_REPRUEBA
  
  # Calcular diferencia observada (tamaño del efecto).
  aucBase  <- PRROC::pr.curve(
    scores.class0 = 1 - puntajesBase, scores.class1 = puntajesBase,
    curve = FALSE)$auc.davis.goadrich
  
  aucFinal <- PRROC::pr.curve(
    scores.class0 = 1 - puntajesFinal, scores.class1 = puntajesFinal,
    curve = FALSE)$auc.davis.goadrich
  
  efecto <- aucFinal - aucBase
  
  # Obtener la distribución bootstrap estratificada.
  set.seed(semilla)
  
  diffs <- foreach::foreach(
    i = 1:B, .combine = c, .options.RNG = list(seed = semilla),
    .packages = "PRROC"
  ) %dorng% {
    ind0 <- which(observados == 0)
    ind1 <- which(observados == 1)
    indx <- c(sample(ind0, replace = TRUE), sample(ind1, replace = TRUE))
    
    baseRemuestreado <- puntajesBase[indx]
    finalRemuestreado <- puntajesFinal[indx]
    
    aucBase <- PRROC::pr.curve(
      scores.class0 = 1 - baseRemuestreado, scores.class1 = baseRemuestreado,
      curve = FALSE)$auc.davis.goadrich
    
    aucFinal <- PRROC::pr.curve(
      scores.class0 = 1 - finalRemuestreado,
      scores.class1 = finalRemuestreado, curve = FALSE)$auc.davis.goadrich
    
    aucFinal - aucBase
  }
  
  # Calcular el estadístico y el valor p.
  estadistico <- efecto / sd(diffs)
  p <- 2 * stats::pnorm(-abs(estadistico))
  
  # Reportar el resultado.
  resultado <- reportarPrueba(
    prueba = "PR AUC par. (b)", p = p, nombreEstadistico = "Z",
    valorEstadistico = estadistico, nombreEfecto = "delta AUC-PR",
    valorEfecto = efecto)
  
  return(resultado)
}  
