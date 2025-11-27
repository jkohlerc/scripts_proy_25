ajustarParametros <- function(
    datosModelo, tipoModelo, semilla = SEMILLA,
    configuracion = EXPERIMENTOS_IRACE, limites = LIMITES_PARAMETROS) {
  # Usa optimización bayesiana para ajustar los hiperparámetros de un modelo
  # y la tasa de remuestreo.
  # Entrada:
  # - datosModelo: descriptor con los datos generales del modelo.
  # - tipoModelo: string con el tipo de modelo.
  # - semilla: semilla.
  # - configuracion: lista con valores de configuración para irace.
  # - limites: lista con los límites del espacio de hiperparámetros por tipo de
  #   modelo.
  # Salida:
  # - Lista con los hiperparámetros óptimos.
  # - Archivo .csv con el dataframe de iteraciones del proceso de optimización.
  # - Archivo .pdf con el gráfico de convergencia.
  
  ##############################################################################
  # Funciones de apoyo: optimización de random forest.
  ##############################################################################
  
  ajustarParametrosRF <- function(datosModelo, tipoModelo) {
    # Ajustar ntree.
    ntree <- analizarConvergenciaNtree(
      datosModelo, tipoModelo, limites = NTREE, semilla = SEMILLA)
    
    # Definir rango para mtry.
    nPredictores <- ncol(datosModelo$df) - 1
    
    mtrySugerido <- if(datosModelo$clasificacion) {
      sqrt(nPredictores)
    } else {
      nPredictores / 3
    }
    
    mtrySugerido <- floor(mtrySugerido)
    minMtry <- as.integer(max(2, floor(mtrySugerido / 2)))
    maxMtry <- as.integer(min(nPredictores, mtrySugerido * 2))
    
    # Si hay un único valor, devolver los parámetros ajustados.
    if(minMtry == maxMtry) {
      parametros <- list(ntree = ntree, mtry = minMtry)
      return(parametros)
    }
    
    # Definir la grilla.
    listaGrilla <- list(ntree = ntree, mtry = seq(minMtry, maxMtry))
    grilla <- expand.grid(listaGrilla, KEEP.OUT.ATTRS = FALSE)
    
    # Evaluar las configuraciones.
    reporte <- data.frame(
      Iteracion = seq_len(nrow(grilla)), Puntaje = NA_real_)
    
    for(i in seq_len(nrow(grilla))) {
      parametros <- as.list(grilla[i,] %>% select(c("ntree", "mtry")))
      
      reporte$Puntaje[i] <- evaluarParametros(
        parametros, datosModelo, tipoModelo)
      
      for(parametro in names(grilla)) {
        reporte[i, parametro] <- grilla[i, parametro]
      }
    }
    
    # Seleccionar como óptimo la fila con el mayor puntaje (la primera en caso
    # de empate).
    optimo <- reporte %>%
      dplyr::filter(Puntaje == max(Puntaje, na.rm = TRUE)) %>%
      dplyr::slice(1)
    
    reporte$Seleccionado <- rep("", nrow(reporte))
    reporte$Seleccionado[reporte$Iteracion == optimo$Iteracion[1]] <- "*"
    
    # Agregar columna con la métrica original.
    nombreMetrica <- if(datosModelo$clasificacion) METRICA_CLAS else METRICA_REG
    
    reporte[[nombreMetrica]] <- restaurarMetrica(
      reporte$Puntaje, datosModelo$clasificacion)
    
    # Ordenar dataframe de reporte.
    columnas <- c(
      "Iteracion", "ntree", "mtry", "Puntaje", nombreMetrica, "Seleccionado")
    
    reporte <- reporte %>% select(any_of(columnas))
    reporte <- reporte %>% mutate(across(where(is.numeric), ~ round(., 3)))
    
    # Guardar el reporte.
    archivo <- paste0("Resumen optimizacion grilla ", tipoModelo)
    guardarReporte(datosModelo, reporte, archivo, usarNombre = TRUE)
    
    # Extraer los parámetros optimizados.
    parametros <- reporte %>% filter(Seleccionado == "*")
    parametros <- as.list(parametros %>% select(c("ntree", "mtry")))
    return(parametros)
  }
  
  analizarConvergenciaNtree <- function(
    datosModelo, tipoModelo, limites = NTREE, semilla = SEMILLA) {
    # Determina la cantidad de árboles a utilizar en un modelo random forest
    # a partir de la cantidad de variables del conjunto de datos e identificando
    # convergencia tras una cantidad de iteraciones sin mejora.
    # Entrada:
    # - datosModelo: descriptor con los datos generales del modelo.
    # - tipoModelo: tipo de modelo.
    # - limites: lista con valores para máximo, salto y tolerancia.
    # - semilla: semilla.
    # Salida:
    # - Entero con la cantidad de árboles seleccionada.
    # - Archivo .pdf con el gráfico de convergencia.
    
    optimo <- obtenerOptimoNtree(datosModelo$df, limites, semilla)
    graficarConvergenciaNtree(optimo, datosModelo, tipoModelo)
    return(as.integer(optimo$ntree))
  }
  
  graficarConvergenciaNtree <- function(optimo, datosModelo, tipoModelo) {
    # Genera el gráfico de convergencia.
    # Entrada:
    # - optimo: lista con el dataframe y la cantidad óptima de árboles.
    # - datosModelo: descriptor con los datos generales del modelo.
    # - tipoModelo: tipo de modelo.
    # Salida: archivo .pdf con el gráfico generado.
    
    # Definir títulos y etiquetas.
    nombreMetrica <- if(datosModelo$clasificacion) "Error OOB" else "RMSE OOB"
    titulo <- "Convergencia de random forest"
    
    subtitulo <- if(tipoModelo == "RFE") {
      paste0(datosModelo$respuesta, ": selección de características")
    } else {
      datosModelo$respuesta
    }
    
    anotacion <- sprintf(
      "ntree: %d (%s = %s)", optimo$ntree, nombreMetrica,
      formatearFlotante(
        optimo$reporte$Error[which.min(
          abs(optimo$reporte$ntree - optimo$ntree))]))
    
    # Escalar eje y.
    rango <- max(optimo$reporte$Error, na.rm = TRUE) -
      min(optimo$reporte$Error, na.rm = TRUE)
    
    ajusteEjeY <- max(rango * 0.1, 0.005)
    minY <- min(optimo$reporte$Error, na.rm = TRUE) - ajusteEjeY
    maxY <- max(optimo$reporte$Error, na.rm = TRUE) + ajusteEjeY
    
    # Generar gráfico.
    g <- ggplot(optimo$reporte, aes(x = ntree, y = Error)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue", size = 2) +
      geom_point(
        data = filter(
          optimo$reporte, ntree == optimo$ntree), aes(x = ntree, y = Error),
        color = "red", size = 4, shape = 21, fill = "red") +
      annotate(
        "text", x = max(optimo$reporte$ntree, na.rm = TRUE),
        y = maxY + (rango * 0.05),
        label = anotacion, hjust = 1, vjust = 0, size = 4.5) +
      scale_y_continuous(labels = formatearFlotante) +
      coord_cartesian(ylim = c(minY, maxY + rango * 0.1)) +
      labs(
        title = titulo, subtitle = subtitulo, x = "Cantidad de árboles",
        y = nombreMetrica) +
      ggpubr::theme_pubr()
    
    # Guardar gráfico.
    archivo <- paste0("Convergencia ntree ", tipoModelo)
    
    guardarGrafico(
      datosModelo, g, archivo, usarNombre = TRUE, width = 8, height = 5)
  }
  
  obtenerOptimoNtree <- function(df, limites, semilla) {
    # Evalúa el error para una grilla de cantidades de árboles para modelos
    # random forest.
    # Entrada:
    # - df: dataframe para el entrenamiento.
    # - limites: lista con valores para máximo, salto y tolerancia.
    # - semilla: semilla.
    # Salida: lista con los siguientes elementos:
    # - reporte: dataframe con los resultados de la evaluación.
    # - ntree: valor seleccionado para ntree.
    
    # Determinar la cantidad mínima de árboles.
    minimo <- max(10 * ncol(df) - 1, 50L)
    
    # Inicialización.
    clasificacion <- is.factor(df$respuesta)
    valoresNtree <- seq(minimo, limites$maximo, by = limites$salto)
    error <- numeric(length(valoresNtree))
    
    # Calcular el error por cada cantidad de árboles con detención temprana.
    for(i in seq_along(valoresNtree)) {
      set.seed(semilla)
      modelo <- ranger(
        respuesta ~ ., df, num.trees = valoresNtree[i],
        classification = clasificacion, seed = semilla, verbose = FALSE)
      
      error[i] <- if(clasificacion) {
        modelo$prediction.error
      } else {
        sqrt(modelo$prediction.error)
      }
      
      # Evaluar si hay mejora significativa.
      if(i >= limites$sinMejora) {
        ultimas_k <- error[(i - limites$sinMejora + 1):i]
        rango_k <- max(ultimas_k, na.rm = TRUE) - min(ultimas_k, na.rm = TRUE)
        if(rango_k < limites$tolerancia) {
          break
        }
      }
    }
    
    # Construir dataframe para el gráfico.
    reporte <- data.frame(
      Iteracion = seq_len(i), ntree = valoresNtree[1:i], Error = error[1:i])
    
    reporte$ntree <- as.integer(reporte$ntree)
    
    # Filtrar la región de convergencia.
    inicio <- max(1, nrow(reporte) - limites$sinMejora + 1)
    region <- reporte[inicio:nrow(reporte), ]
    
    # Suavizar el error de la región de convergencia.
    region$ErrorSuavizado <- zoo::rollmean(
      region$Error, k = limites$ventana, fill = NA, align = "right")
    
    region$ErrorSuavizado[is.na(region$ErrorSuavizado)] <-
      region$Error[is.na(region$ErrorSuavizado)]
    
    # Identificar el segmento más estable (mínima varianza) de la región.
    varianzaVentana <- sapply(
      1:(nrow(region) - limites$ventana + 1), function(i) {
        var(region$ErrorSuavizado[i:(i + limites$ventana - 1)])
      })
    
    mejorVentana <- which.min(varianzaVentana)
    
    # Definir la cantidad de árboles como un 10% más de la cantidad donde inicia
    # la ventana óptima de convergencia (redondeado al punto evaluado más
    # cercano).
    ntree <- region$ntree[mejorVentana]
    optimo <- round(ntree * 1.1 / limites$salto) * limites$salto
    optimo <- valoresNtree[which.min(abs(valoresNtree - optimo))]
    resultado <- list(reporte = reporte, ntree = as.integer(optimo))
    return(resultado)
  }
  
  
  
  ##############################################################################
  # Funciones de apoyo: optimización otros modelos.
  ##############################################################################
  
  definirEspacio <- function(
    tipoModelo, clasificacion, limites = LIMITES_PARAMETROS) {
    # Establece el espacio de búsqueda de los hiperparámetros según el tipo de
    # modelo.
    # Entrada:
    # - tipoModelo: string con el tipo de modelo.
    # - clasificacion: TRUE para clasificación; FALSE para regresión.
    # - limites: lista con los límites del espacio de hiperparámetros por tipo
    #   de modelo.
    # Salida: lista con rangos de valores para hiperparámetros de un modelo.
    
    espacioBase <- limites[[tipoModelo]]
    lineas <- list()
    logaritmicos <- list("cost", "epsilon", "sigma", "eta")
    
    for(nombre in names(espacioBase)) {
      if(nombre == "epsilon" &&
         (!startsWith(tipoModelo, "SVM") || clasificacion)) {
        next
      }
      
      rango <- espacioBase[[nombre]]
      if(length(rango) != 2) next
      tipo <- if(is.integer(rango)) "i" else "r"
      if(nombre %in% logaritmicos) tipo <- paste(tipo, "log", sep = ",")
      minimo <- as.character(rango[1])
      maximo <- as.character(rango[2])
      linea <- paste0(nombre, " \"\" ", tipo, " (", minimo, ", ", maximo, ")")
      lineas <- c(lineas, linea)
    }
    
    texto <- paste(lineas, collapse = "\n")
    espacio <- irace::readParameters(text = texto, digits = 10)
    return(espacio)
  }
  
  ejecutarIrace <- function(datosModelo, tipoModelo, semilla = SEMILLA) {
    # Ejecuta irace.
    # Entrada:
    # - datosModelo: descriptor con los datos generales del modelo.
    # - tipoModelo: string con el tipo de modelo.
    # - semilla: semilla.
    # Salida:
    # - Lista con los hiperparámetros ajustados.
    # - Archivo .csv con el reporte.
    
    # Crear archivos temporales.
    instanciaDummy <- "dummy.txt"
    writeLines("1", instanciaDummy)
    
    espacio <- definirEspacio(tipoModelo, datosModelo$clasificacion)
    
    escenario <- list(
      maxExperiments = 100 * length(espacio$.params), logFile = LOG_IRACE,
      targetRunner = funcionObjetivo(datosModelo, tipoModelo),
      parameters = espacio, trainInstancesFile = instanciaDummy, seed = semilla)
    
    irace(escenario)
    file.remove(instanciaDummy)
    parametros <- reportarIrace(datosModelo, tipoModelo)
    return(parametros)
  }
  
  evaluarParametros <- function(
    parametros, datosModelo, tipoModelo, semilla = SEMILLA,
    rangos = RANGO_METRICAS) {
    # Ajusta un modelo con los hiperparámetros dados y lo evalúa.
    # Entrada:
    # - parametros: lista de hiperparámetros en el formato requerido por el
    #   modelo.
    # - datosModelo: descriptor con los datos generales del modelo.
    # - tipoModelo: string con el tipo de modelo.
    # - semilla: semilla.
    # - rangos: rango de valores para las métricas.
    # Salida: valor obtenido para la métrica de entrenamiento.
    
    puntaje <- tryCatch({
      suppressWarnings(entrenarModelo(
        datosModelo, tipoModelo, parametros, completo = FALSE,
        optimizacion = TRUE, semilla))
    }, error = function(e) {
      return(NA_real_)
    })
    
    if(is.na(puntaje)) return(1)
    puntaje <- normalizarMetrica(puntaje, datosModelo$clasificacion)
    return(puntaje)
  }
  
  funcionObjetivo <- function(
    datosModelo, tipoModelo, semilla = SEMILLA, limites = LIMITES_PARAMETROS) {
    # Crea la configuración para poder ejecutar irace.
    # Entrada:
    # - datosModelo: descriptor con los datos generales del modelo.
    # - tipoModelo: string con el tipo de modelo.
    # - semilla: semilla.
    # - limites: lista con los límites del espacio de hiperparámetros por tipo
    #   de modelo.
    # Salida: runner configurado.
    
    function(experiment, scenario) {
      parametros <- as.list(experiment$configuration)
      
      if(tipoModelo == "XGB") {
        parametros$gamma <- 0
        parametros$subsample <- 0.8
        parametros$colsample_bytree <- 0.8
        parametros$nrounds <- ifelse(parametros$eta <= 0.1, 300L, 200L)
      }
      
      puntaje <- evaluarParametros(parametros, datosModelo, tipoModelo, semilla)
      return(list(cost = puntaje))
    }
  }
  
  normalizarMetrica <- function(valor, clasificacion, rangos = RANGO_METRICAS) {
    # Normaliza la métrica (MCC o RMSE) usando normalización min-max para que
    # quede en una escala de 0 a 1 maximizable.
    # Entrada:
    # - valor: valor obtenido para la métrica.
    # - clasificacion: TRUE para clasificación; FALSE para regresión.
    # - rangos: rango de valores para las métricas.
    # Salida: métrica normalizada.
    
    if(clasificacion) {
      valor <- 1 - (valor - rangos$minClas) / (rangos$maxClas - rangos$minClas)
    } else {
      valor <- (valor - rangos$minReg) / (rangos$maxReg - rangos$minReg)
    }
    
    return(pmin(pmax(valor, 0), 1))
  }
  
  reportarIrace <- function(
    datosModelo, tipoModelo, semilla = SEMILLA, limites = LIMITES_PARAMETROS) {
    # Reporta los resultados del ajuste de hiperparámetros.
    # Entrada:
    # - datosModelo: descriptor con los datos generales del modelo.
    # - tipoModelo: string con el tipo de modelo.
    # - semilla: semilla.
    # - limites: lista con los límites del espacio de hiperparámetros por tipo
    #   de modelo.
    # Salida:
    # - Lista con los hiperparámetros ajustados.
    # - Archivo .csv con el reporte.
    
    # Construir el reporte.  
    load(LOG_IRACE)
    configuraciones <- iraceResults$allConfigurations
    experimentos <- iraceResults$experiments
    colnames(experimentos) <- as.character(configuraciones$.ID.)
    filas <- list()
    
    for(i in seq_len(nrow(configuraciones))) {
      fila <- configuraciones[i, ]
      id <- as.character(fila$.ID.)
      
      if(id %in% colnames(experimentos)) {
        valores <- experimentos[, id]
        fila$Puntaje <- if(all(is.na(valores))) 1 else min(valores, na.rm = TRUE)
      } else {
        fila$Puntaje <- 1
      }
      
      filas <- append(filas, list(fila))
    }
    
    reporte <- do.call(rbind, filas)
    
    # Ordenar reporte.
    reporte <- reporte[!duplicated(reporte$.ID.), ]
    
    reporte <- reporte %>% rename(Configuracion = .ID., Padre = .PARENT.) %>%
      relocate(Padre, .after = Configuracion)
    
    # Seleccionar la mejor configuración. En caso de empate, escoger la 
    # configuración con mejor métrica secundaria (AUC_PR para clasificación, R2
    # para regresión).
    mejorPuntaje <- min(reporte$Puntaje)
    candidatos <- reporte %>% filter(Puntaje == mejorPuntaje)
    
    print(candidatos)
    
    if(nrow(candidatos) == 1) {
      reporte$Seleccionado <- ifelse(reporte$Puntaje == mejorPuntaje, "*", "")
    } else {
      puntajeDesempate <- sapply(seq_len(nrow(candidatos)), function(i) {
        fila <- candidatos[i, ]
        
        parametros <- as.list(fila %>% select(
          -c("Configuracion", "Padre", "Puntaje")))
        
        if(tipoModelo == "XGB") {
          parametros$gamma = limites$XGB$gamma
          parametros$subsample = limites$XGB$subsample
          parametros$colsample_bytree = limites$XGB$colsample_bytree
          parametros$nrounds <- ifelse(parametros$eta <= 0.1, 300L, 200L)
        }
        
        modelo <- entrenarModelo(
          datosModelo, tipoModelo, parametros = parametros, completo = TRUE,
          optimizacion = TRUE)
        
        if(datosModelo$clasificacion) {
          return(modelo$resumen$media$AUC_PR)
        } else {
          return(modelo$resumen$media$R2)
        }
      })
      
      idMejor <- which.max(puntajeDesempate)
      mejor <- candidatos$Configuracion[idMejor]
      reporte$Seleccionado <- ifelse(reporte$Configuracion == mejor, "*", "")
    }
    
    # Añadir columna con la métrica original.
    nombreMetrica <- if(datosModelo$clasificacion) METRICA_CLAS else METRICA_REG
    
    reporte[[nombreMetrica]] <- restaurarMetrica(
      reporte$Puntaje, datosModelo$clasificacion)
    
    reporte <- reporte %>% relocate(Seleccionado, .after = last_col())
    
    # Guardar el reporte y eliminar elementos inútiles.
    archivo <- paste0("Resumen optimizacion irace ", tipoModelo)
    guardarReporte(datosModelo, reporte, archivo, usarNombre = TRUE)
    
    file.remove(LOG_IRACE)
    rm(iraceResults)
    
    # Extraer los hiperparámetros óptimos.
    optimo <- reporte %>% filter(Seleccionado == "*")
    
    parametros <- as.list(optimo %>% select(
      -any_of(
        c("Configuracion", "Padre", "Puntaje", "MCC", "RMSE", "Seleccionado"))))
    
    if(tipoModelo == "XGB") {
      parametros$gamma = limites$XGB$gamma
      parametros$subsample = limites$XGB$subsample
      parametros$colsample_bytree = limites$XGB$colsample_bytree
      parametros$nrounds <- ifelse(parametros$eta <= 0.1, 300L, 200L)
    }
    
    return(parametros)
  }
  
  restaurarMetrica <- function(valor, clasificacion, rangos = RANGO_METRICAS) {
    # Restaura una métrica normalizada a su valor original.
    # - valor: valor de la métrica normalizada.
    # - clasificacion: TRUE para clasificación; FALSE para regresión.
    # - rangos: rango de valores para las métricas.
    # Salida: métrica restaurada.
    
    valor <- pmin(pmax(valor, 0), 1)
    
    if(clasificacion) {
      return((1 - valor) * (rangos$maxClas - rangos$minClas) + rangos$minClas)
    } else {
      return(valor * (rangos$maxReg - rangos$minReg) + rangos$minReg)
    }
  }
  
  
  
  ##############################################################################
  # Función principal.
  ##############################################################################
  
  if(tipoModelo %in% c("RLin", "RLog")) return(NULL)
  
  if(tipoModelo %in% c("RFE", "RF")) {
    return(ajustarParametrosRF(datosModelo, tipoModelo))
  }
  
  return(ejecutarIrace(datosModelo, tipoModelo))
}