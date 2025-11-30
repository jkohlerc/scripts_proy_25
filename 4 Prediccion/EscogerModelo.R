escogerModelo <- function(
    datosModelo, tipos = TIPOS_MODELO, semilla = SEMILLA) {
  # Ajusta diferentes modelos para un conjunto de datos y escoge el mejor.
  # Entrada:
  # - datosModelo: descriptor con los datos generales del modelo.
  # - tipos: vector con los tipos de modelos a ajustar.
  # - semilla: semilla.
  # Salida:
  # - Objeto con el modelo seleccionado.
  # - Archivo .rds con la lista de modelos ajustados.
  # - Archivo .csv con el reporte de los modelos.
  
  ##############################################################################
  # Funciones de apoyo: evaluación de modelos de regresión lineal y logística.
  ##############################################################################
  
  graficarApalancamientoRL <- function(
    influencia, limiteApalancamiento, limiteCook, datosModelo) {
    # Grafica la la influencia de las observaciones de un modelo de regresión
    # lineal o logística.
    # Entrada:
    # - influencia: dataframe con el reporte de influencia de cada observación.
    # - limiteApalancamiento: valor límite para el apalancamiento.
    # - limiteCook: valor límite para la distancia de Cook.
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida: archivo .pdf con el gráfico generado.
    
    # Definir formato para marcas de los ejes.
    formatoEjeX <- function(x) sapply(x, function(v) formatearFlotante(v, 3))
    formatoEjeY <- function(x) sapply(x, function(v) formatearFlotante(v, 1))
    
    # Generar los títulos.
    titulo = "Apalancamiento e influencia de las observaciones"
    nombre <- tolower(datosModelo$nombre)
    tipo <- if(datosModelo$clasificacion) "RLog" else "RLin"
    subtitulo = paste0(datosModelo$respuesta, ": ", tipo, " modelo ", nombre)
    
    # Crear gráfico base.
    g <- ggplot(influencia, aes(x = Apalancamiento, y = Residuo)) +
      geom_point(
        aes(size = Cook, color = Cook), alpha = 0.5,
        show.legend = c(size = FALSE)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_vline(
        xintercept = limiteApalancamiento, linetype = "dashed", color = "red") +
      geom_text_repel(
        data = subset(influencia, Cook > limiteCook), aes(label = Id), size = 3,
        color = "blue") +
      scale_size_continuous(range = c(1, 10)) +
      scale_color_gradient(
        low = "lightblue", high = "blue", labels = formatoEjeX) +
      labs(
        title = titulo, subtitle = subtitulo, x = "Apalancamiento",
        y = "Residuos estandarizados", color = "Distancia de Cook",
        size = "Distancia de Cook") +
      theme_pubr() + scale_x_continuous(labels = formatoEjeX) +
      scale_y_continuous(labels = formatoEjeY) +
      theme(legend.text = element_text(angle = 45, hjust = 1))
    
    # Guardar el gráfico.
    archivo = paste0("Apalancamiento RL ", tolower(datosModelo$nombre))
    
    guardarGrafico(
      datosModelo, g, archivo, usarNombre = TRUE, width = 8, height = 5)
  }
  
  graficarResiduosRL <- function(modelo, datosModelo) {
    # Grafica la curvatura de los residuos por cada variable independiente de un
    # modelo de regresión lineal.
    # Entrada:
    # - modelo: objeto con el modelo.
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida: archivo .pdf con el gráfico generado.
    
    # Calcular residuos estandarizados.
    predicciones <- modelo$predicciones
    residuos <- NULL
    
    if(datosModelo$clasificacion) {
      predicciones$Observado <- ifelse(predicciones$Observado == "R", 1, 0)
      residuos <- predicciones$Observado - predicciones$Prob_R
    } else {
      residuos <- predicciones$Observado - predicciones$Predicho
    }
    
    residuosEstandar <- (residuos - mean(residuos)) / sd(residuos)
    
    # Construir dataframe para graficar.
    predictores <- strsplit(modelo$predictores, ", ")[[1]]
    
    df <- data.frame(
      residuos = residuosEstandar, modelo$datos[, predictores, drop = FALSE])
    
    # Definir formato para marcas de los ejes.
    formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 1))
    
    # Crear lista de gráficos individuales.
    graficos <- lapply(predictores, function(var) {
      g <- ggplot(df, aes_string(x = var, y = "residuos")) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(x = var, y = "Residuos estandarizados")
      
      if (is.factor(df[[var]])) {
        g <- g + geom_boxplot()
      } else {
        g <- g + geom_point(alpha = 0.7) +
          geom_smooth(method = "loess", color = "blue", se = FALSE)
      }
      
      g <- g + theme_pubr() + scale_y_continuous(labels = formatoEjes)
      
      if(!startsWith(var, "P_")) {
        g <- g + scale_x_continuous(labels = formatoEjes)
      }
      
      return(g)
    })
    
    # Generar los títulos del gráfico combinado.
    titulo = "Prueba de curvatura"
    nombre <- tolower(modelo$nombre)
    subtitulo = paste0(modelo$respuesta, ": ", modelo$tipo, " modelo ", nombre)
    
    # Crear gráfico combinado.
    filas <- ceiling(length(predictores) / 2)
    columnas <- if(length(graficos) == 1) 1 else 2
    alto <- 3 * filas
    ancho <- if(length(graficos) == 1) 4 else 8
    
    g <- wrap_plots(graficos, ncol = columnas) +
      plot_annotation(title = titulo, subtitle = subtitulo)
    
    # Guardar el gráfico combinado.
    archivo <- paste0("Residuos RL ", tolower(datosModelo$nombre))
    
    guardarGrafico(
      datosModelo, g, archivo, usarNombre = TRUE, width = ancho, height = alto)
  }
  
  evaluarSobreinfluencia <- function(modeloLineal, datosModelo) {
    # Evalúa la presencia de obervaciones sobreinfluyentes considerando
    # distancia de Cook, apalancamiento y residuos estandarizados.
    # Entrada:
    # - modeloLineal: objeto con el modelo lineal.
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida:
    # - Archivo .csv con la evaluación de sobreinfluencia.
    # - Archivo .pdf con el gráfico de apalancamiento.
    
    # Inicializar dataframe.
    n <- nobs(modeloLineal)
    p <- length(coef(modeloLineal))
    influencia <- data.frame(Id = 1:n)
    
    # Evaluar apalancamiento.
    influencia$Apalancamiento <- hatvalues(modeloLineal)
    limiteHat <- 2 * p / n
    influencia$sigApalancamiento <- influencia$Apalancamiento > limiteHat
    
    # Evaluar distancia de Cook.
    influencia$Cook <- cooks.distance(modeloLineal)
    limiteCook <- 4 / n
    influencia$sigCook <- influencia$Cook > limiteCook
    
    # Evaluar residuos estandarizados.
    influencia$Residuo <- rstudent(modeloLineal)
    limiteResiduo <- 2 * sd(residuals(modeloLineal))
    influencia$sigResiduo <- abs(influencia$Residuo) > limiteResiduo
    
    # Añadir columna de resumen identificando observaciones sobreinfluyentes.
    influencia$Sobreinfluencia <- influencia$sigApalancamiento |
      influencia$sigCook | influencia$sigResiduo
    
    # Dar formato a las columnas.
    influencia <- influencia %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      mutate(across(where(is.logical), ~ ifelse(.x, "*", "")))
    
    # Guardar el dataframe.
    archivo <- paste0("Sobreinfluencia RL ", tolower(datosModelo$nombre))
    guardarReporte(datosModelo, influencia, archivo, usarNombre = TRUE)
    
    # Generar gráfico de apalancamiento.
    graficarApalancamientoRL(influencia, limiteHat, limiteCook, datosModelo)  
  }
  
  verificarCondicionesResiduosRL <- function(modeloLineal, datosModelo) {
    # Realiza las pruebas correspondientes para verificar condiciones asociadas
    # a los residuos: independencia, homocedasticidad (solo regresión lineal) y
    # normalidad (solo regresión lineal).
    # Entrada:
    # - modeloLineal: objeto con el modelo lineal.
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida: archivo .csv con los resultados de las pruebas.
    
    reporte <- data.frame()
    
    if(!datosModelo$clasificacion) {
      # Comprobar normalidad de los residuos para regresión lineal.
      prueba <- shapiroWilk(residuals(modeloLineal))
      reporte <- rbind(reporte, prueba)
      
      # Comprobar homocedasticidad de los residuos para regresión lineal.
      prueba <- ncv(modeloLineal)
      reporte <- rbind(reporte, prueba)
    }
    
    # Comprobar independencia de los residuos.
    prueba <- durbinWatson(modeloLineal)
    reporte <- rbind(reporte, prueba)
    
    # Guardar dataframe con resultados de las pruebas.
    archivo <- paste0("Pruebas condiciones RL ", tolower(datosModelo$nombre))
    
    guardarReporte(
      datosModelo, formatearReporte(reporte), archivo, usarNombre = TRUE)
  }
  
  verificarRL <- function(modelo, datosModelo) {
    # Verifica el cumplimiento de condiciones para modelos lineales.
    # Entrada:
    # - modelo: objeto con el modelo.
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida:
    # - Archivo .csv con los resultados de las pruebas.
    # - Archivo .csv con la evaluación de sobreinfluencia.
    # - Archivo .pdf con el gráfico de residuos.
    # - Archivo .pdf con el gráfico de apalancamiento.
    
    # Ajustar el modelo lineal.
    modeloLineal <- if(modelo$clasificador) {
      glm(
        as.formula("respuesta ~ ."), data = modelo$datos,
        family = binomial(link = "logit"))
    } else {
      lm(as.formula("respuesta ~ ."), data = modelo$datos)
    }
    
    # Verificar condiciones.
    verificarCondicionesResiduosRL(modeloLineal, datosModelo)
    graficarResiduosRL(modelo, datosModelo)
    evaluarSobreinfluencia(modeloLineal, datosModelo)
  }
  
  
  
  ##############################################################################
  # Funciones de apoyo: reporte de clasificadores.
  ##############################################################################
  
  graficarCalibracion <- function(modelo) {
    # Grafica la curva de calibración para un clasificador.
    # Entrada:
    # - modelo: objeto con el modelo.
    # Salida: objeto con el gráfico.
    
    # Convertir valores observados a formato 0/1 y calcular la media de las
    # probabilidades obtenidas para cada observación.
    df <- modelo$predicciones %>%
      mutate(obs = ifelse(Observado == "REPRUEBA", 1, 0)) %>%
      group_by(Id) %>%  summarise(obs = first(obs), prob = mean(Prob_REPRUEBA))
    
    # Agrupar dataframe por bins.
    df <- df %>% mutate(
      bin = cut(
        prob, breaks = seq(0, 1, length.out = 11), include.lowest = TRUE)) %>%
      group_by(bin) %>% summarise(
        mediaProbabilidad = mean(prob), mediaObservado = mean(obs))
    
    # Definir formato para marcas de los ejes.
    formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 3))
    
    # Generar gráfico
    g <- ggplot(df, aes(x = mediaProbabilidad, y = mediaObservado)) +
      geom_point(size = 3, color = "navy") +
      geom_line(color = "navy", size = 1.2) +
      geom_abline(
        slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Gráfico de calibración", x = "Probabilidad estimada",
        y = "Frecuencia observada") +
      theme_pubr() + scale_x_continuous(labels = formatoEjes) +
      scale_y_continuous(labels = formatoEjes)
    
    return(g)
  }
  
  graficarClasificador <- function(modelo, datosModelo) {
    # Grafica aspectos para evaluar la calidad de un clasificador.
    # Entrada:
    # - modelo: objeto con el modelo.
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida: archivo .pdf con un gráfico que contiene:
    # - Curva ROC.
    # - Curva PR.
    # - Gráfico de calibración.
    # - Importancia de variables.
    
    # Generar cada subgráfico.
    roc <- graficarROC(modelo)
    pr <- graficarPR(modelo)
    calibracion <- graficarCalibracion(modelo)
    importancia <- graficarImportancia(modelo)
    
    # Generar los títulos del gráfico combinado.
    titulo = "Resumen de desempeño del clasificador"
    
    subtitulo = paste0(
      modelo$respuesta, ": ", modelo$tipo, " modelo ", tolower(modelo$nombre))
    
    # Generar y guardar el gráfico combinado.
    g <- (roc | pr) / (calibracion | importancia) +
      plot_annotation(title = titulo, subtitle = subtitulo)
    
    archivo <- paste0("Evaluacion modelo ", datosModelo$nombre)
    
    guardarGrafico(
      datosModelo, g, archivo, usarNombre = TRUE, width = 12, height = 10)
    
    return(g)
  }
  
  graficarPR <- function(modelo) {
    # Grafica la curva PR para un clasificador.
    # Entrada:
    # - modelo: objeto con el modelo.
    # Salida: objeto con el gráfico.
    
    # Generar la curva PR.
    observado <- ifelse(modelo$predicciones$Observado == "REPRUEBA", 1, 0)
    curva <- generarCurvaPR(modelo)
    
    # Construir dataframe para el gráfico.
    dfCurva <- data.frame(
      Recall = curva$curve[,1], Precision = curva$curve[,2])
    
    # Definir formato para marcas de los ejes.
    formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 3))
    
    # Definir subtítulo con el valor de la métrica.
    subtitulo <- paste0("AUC-PR = ", modelo$resumen$media$AUC_PR)

    # Generar el gráfico.
    g <- ggplot(dfCurva, aes(x = Recall, y = Precision)) +
      geom_line(color = "navy", size = 1.2) +
      geom_hline(
        yintercept = mean(observado), linetype = "dashed", color = "red") +
      labs(
        title = "Curva PR", subtitle = subtitulo, x = "Recall", y = "Precisión") +
      theme_pubr() + scale_x_continuous(labels = formatoEjes) +
      scale_y_continuous(labels = formatoEjes)
    
    return(g)
  }
  
  graficarROC <- function(modelo) {
    # Grafica la curva ROC para un clasificador.
    # Entrada:
    # - modelo: objeto con el modelo.
    # Salida: objeto con el gráfico.
    
    # Generar la curva ROC.
    curva <- generarCurvaROC(modelo)
    
    # Construir dataframe para el gráfico.
    df <- data.frame(
      verdaderosPositivos = curva$sensitivities,
      falsosPositivos = 1 - curva$specificities)
    
    # Definir formato para marcas de los ejes.
    formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 3))
    
    # Definir subtítulo con el valor de la métrica.
    subtitulo <- paste0("AUC-ROC = ", modelo$resumen$media$AUC_ROC)
    
    # Generar el gráfico.
    g <- ggplot(df, aes(x = falsosPositivos, y = verdaderosPositivos)) +
      geom_line(color = "navy", size = 1.2) +
      geom_abline(
        slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Curva ROC", subtitle = subtitulo, x = "1 - especificidad",
        y = "Sensibilidad") +
      theme_pubr() + scale_x_continuous(labels = formatoEjes) +
      scale_y_continuous(labels = formatoEjes)
    
    return(g)
  }
  
  
  
  ##############################################################################
  # Funciones de apoyo: reporte de regresores.
  ##############################################################################
  
  graficarDensidadResiduos <- function(modelo) {
    # Genera un gráfico de la distribución de los residuos.
    # Entrada:
    # - modelo: objeto con el modelo.
    # Salida: objeto con el gráfico.
    
    # Construir dataframe para el gráfico.
    df <- modelo$predicciones
    df$residual <- df$Observado - df$Predicho
    
    # Definir formato para marcas de los ejes.
    formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 3))
    
    # Generar el gráfico.
    g <- ggplot(df, aes(x = residual)) +
      geom_density(fill = "navy", color = "black", alpha = 0.6)+
      geom_vline(
        xintercept = mean(df$residual, na.rm = TRUE), linetype = "dashed",
        color = "red") +
      labs(title = "Distribución de residuos", x = "Residuo", y = "Densidad") +
      theme_pubr() + scale_x_continuous(labels = formatoEjes) +
      scale_y_continuous(labels = formatoEjes)
    
    return(g)
  }
  
  graficarPredicciones <- function(modelo) {
    # Genera un gráfico de dispersión de valores predichos frente a observados.
    # Entrada:
    # - modelo: objeto con el modelo.
    # Salida: objeto con el gráfico.
    
    # Definir formato para marcas de los ejes.
    formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 1))
    
    # Generar el gráfico.
    g <- ggplot(modelo$predicciones, aes(x = Observado, y = Predicho)) +
      geom_point(color = "navy") +
      geom_abline(
        slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Dispersión de predicciones", x = "Observado", y = "Estimado") +
      theme_pubr() + scale_x_continuous(labels = formatoEjes) +
      scale_y_continuous(labels = formatoEjes)
    
    return(g)
  }
  
  graficarRegresor <- function(modelo, datosModelo) {
    # Grafica aspectos para evaluar la calidad de un regresor.
    # Entrada:
    # - modelo: objeto con el modelo.
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida: archivo .pdf con un gráfico que contiene:
    # - Curva ROC.
    # - Curva PR.
    # - Gráfico de calibración.
    # - Importancia de variables.
    
    # Generar cada subgráfico.
    predicciones <- graficarPredicciones(modelo)
    residuos <- graficarResiduos(modelo)
    densidad <- graficarDensidadResiduos(modelo)
    importancia <- graficarImportancia(modelo)
    
    # Generar los títulos del gráfico combinado.
    titulo = "Resumen de desempeño del regresor"
    
    subtitulo = paste0(
      modelo$respuesta, ": ", modelo$tipo, " modelo ", tolower(modelo$nombre))
    
    # Generar y guardar el gráfico combinado.
    g <- (predicciones | residuos) / (densidad | importancia) +
      plot_annotation(title = titulo, subtitle = subtitulo)
    
    archivo <- paste0("Evaluacion modelo ", datosModelo$nombre)
    
    guardarGrafico(
      datosModelo, g, archivo, usarNombre = TRUE, width = 12, height = 10)
    
    return(g)
  }
  
  graficarResiduos <- function(modelo) {
    # Genera un gráfico de dispersión de residuos frente a valores predichos.
    # Entrada:
    # - modelo: objeto con el modelo.
    # Salida: objeto con el gráfico.
    
    # Construir dataframe para el gráfico.
    df <- modelo$predicciones
    df$residual <- df$Observado - df$Predicho
    
    # Definir formato para marcas de los ejes.
    formatoEjeX <- function(x) sapply(x, function(v) formatearFlotante(v, 1))
    formatoEjeY <- function(x) sapply(x, function(v) formatearFlotante(v, 3))
    
    # Generar el gráfico.
    g <- ggplot(df, aes(x = Predicho, y = residual)) +
      geom_point(color = "navy") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Dispersión de residuos", x = "Estimado", y = "Residuo") +
      theme_pubr() + scale_x_continuous(labels = formatoEjeX) +
      scale_y_continuous(labels = formatoEjeY)
    
    return(g)
  }
  
  
  
  ##############################################################################
  # Funciones de apoyo: reporte de modelos.
  ##############################################################################
  
  escogerMejor <- function(reporte, clasificacion) {
    # Añade una columna con el modelo seleccionado al dataframe con el reporte
    # de los modelos ajustados.
    # Entrada:
    # - reporte: dataframe con el reporte.
    # - clasificacion: TRUE para clasificación; FALSE para regresión.
    # Salida:
    # - Dataframe modificado con el reporte.
    
    # Escoger el mejor modelo.
    mejor <- reporte
    
    if(clasificacion) {
      mejorValor <- max(mejor$MCC, na.rm = TRUE)
      mejor <- mejor %>% filter(MCC == mejorValor)
      mejorValor <- max(mejor$AUC_PR, na.rm = TRUE)
      mejor <- mejor %>% filter(AUC_PR == mejorValor)
      mejorValor <- max(mejor$AUC_ROC, na.rm = TRUE)
      mejor <- mejor %>% filter(AUC_ROC == mejorValor)
    } else {
      mejorValor <- min(mejor$RMSE, na.rm = TRUE)
      mejor <- mejor %>% filter(RMSE == mejorValor)
      mejorValor <- max(mejor$R2, na.rm = TRUE)
      mejor <- mejor %>% filter(R2 == mejorValor)
    }
    
    mejor <- mejor[1, ]
    
    # Añadir columna con la selección.
    reporte$Seleccionado <- ifelse(reporte$Modelo == mejor$Modelo[1], "*", "")
    return(reporte)
  }
  
  extraerMejor <- function(modelos, reporte, datosModelo) {
    # Extrae el mejor modelo de la lista de modelos.
    # Entrada:
    # - modelos: lista de modelos.
    # - reporte: dataframe con el reporte.
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida:
    # - Objeto con el modelo seleccionado.
    # - Archivo .pdf con gráficos de evaluación del modelo.
    # - Archivo .txt con verificación de condiciones (solo regresión lineal o
    #   logística).
    # - Archivo .pdf con gráfico de curvatura de los residuos (solo regresión
    #   lineal o logística).
    # - Archivo .pdf con gráfico de apalancamiento (solo regresión lineal o
    #   logística).
    
    # Extraer el mejor modelo.
    filtrado <- reporte %>% filter(Seleccionado == "*")
    tipo <- filtrado$Modelo
    mejor <- modelos[[tipo]]
    
    # Reportar el modelo.
    if(tipo %in% c("RLin", "RLog")) verificarRL(mejor, datosModelo)
    
    if(datosModelo$clasificacion) {
      graficarClasificador(mejor, datosModelo)
    } else {
      graficarRegresor(mejor, datosModelo)
    }
    
    return(mejor)
  }
  
  reportarModelos <- function(modelos, datosModelo) {
    # Crea un dataframe con el reporte de los modelos ajustados.
    # Entrada:
    # - modelos: lista de modelos ajustados.
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida:
    # - Dataframe con el reporte.
    # - Archivo .csv con el reporte.
    
    # Reportar las métricas de los modelos.
    reporte <- NULL
    
    for(tipo in names(modelos)) {
      actual <- NULL
      modelo <- modelos[[tipo]]
      
      actual <- data.frame(
        Respuesta = datosModelo$respuesta, Modelo = modelo$tipo,
        Predictores = modelo$predictores)
      
      actual <- cbind(actual, modelo$resumen$media)
      
      if(datosModelo$clasificacion) {
        actual <- cbind(actual, modelo$matriz)
      }
      
      params <- reportarParametros(modelo$parametros)
      actual <- cbind(actual, parametros = params)
      reporte <- if(is.null(reporte)) actual else rbind(reporte, actual)
    }
    
    # Añadir selección del mejor modelo.
    reporte <- escogerMejor(reporte, datosModelo$clasificacion)
    
    # Guardar el reporte.
    guardarReporte(datosModelo, reporte, "Resumen modelos", usarNombre = TRUE)
    return(reporte)
  }
  
  
  
  ##############################################################################
  # Función principal.
  ##############################################################################
  
  # Inicialización.
  nPredictores <- ncol(datosModelo$df) - 1
  modelos <- list()
  
  # Determinar la cantidad de variables independientes.
  for(tipo in tipos) {
    # Configurar el tipo de modelo.
    if(tipo == "RL") {
      tipo <- if(datosModelo$clasificacion) "RLog" else "RLin"
    }
    
    # Ajustar los modelos. Si solo hay una variable independiente, omitir random
    # forest y máquina de vectores soporte con kernel lineal.
    if(nPredictores > 1 || !tipo %in% c("SVM_L", "RF", "XGB")) {
      cat(paste0("\t", tipo, "\n"))
      
      modelos[[tipo]] <- entrenarModelo(
        datosModelo, tipo, completo = TRUE, optimizacion = FALSE,
        semilla = semilla)
    }
  }
  
  # Guardar la lista de modelos.
  guardarModelos(datosModelo, modelos, "Modelos")
  
  # Reportar modelos y escoger el mejor.
  reporte <- reportarModelos(modelos, datosModelo)
  modelo <- extraerMejor(modelos, reporte, datosModelo)
  
  # Devolver el modelo seleccionado.
  return(modelo)
}
