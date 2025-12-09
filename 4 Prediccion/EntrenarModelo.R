entrenarModelo <- function(
    datosModelo, tipoModelo, parametros = NULL, optimizacion, semilla = SEMILLA,
    nombresMetricas = NOMBRES_METRICAS) {
  # Ajusta un modelo con un conjunto dado de hiperparámetros.
  # Entrada:
  # - datosModelo: descriptor con los datos generales del modelo.
  # - tipoModelo: string con el tipo de modelo.
  # - parametros: lista con los valores de los hiperparámetros (si es NULL, se
  #   optimizan o se usan parámetros por defecto).
  #   características).
  # - optimizacion: TRUE indica que se usa validación cruzada simple y solo se
  #   devuelve la métrica para optimización de hiperparámetros; FALSE indica que
  #   se usa validación cruzada con repeticiones y se devuelve el modelo
  #   completo.
  # - semilla: semilla.
  # - nombresMetricas: lista con los nombres de las métricas.
  # Salida: según opción optimizacion:
  # - FALSE: el objeto resultante incluye un modelo completo, con:
  #   - clasificador: TRUE si es clasificador, FALSE si es regresor.
  #   - respuesta: nombre de la variable de respuesta.
  #   - nombre: string con el nombre del modelo (Base o Final).
  #   - tipo: string con el tipo de modelo.
  #   - datos: dataframe con los datos de entrenamiento.
  #   - predictores: string con los nombres de las variables predictoras, por
  #     importancia decreciente.
  #   - parametros: lista con los parámetros empleados.
  #   - importancia: dataframe con la importancia de las variables ordenado en
  #     forma decreciente.
  #   - predicciones: dataframe con las predicciones y etiquetas de las
  #     observaciones no empleadas durante el entrenamiento.
  #   - metricas: reporte de las métricas por pliegue.
  #   - resumen: reporte con el resumen (media, error estándar) de las métricas.
  #   - final: objeto con el modelo final.
  # - TRUE: solo se devuelve el valor resumido de la métrica de interés.

  ##############################################################################
  # Funciones de apoyo.
  ##############################################################################
  
  calcularImportancia <- function(modeloFinal, tipoModelo, df = NULL) {
    # Determina la importancia de las variables para un modelo ajustado de
    # tidymodels.
    # Entrada:
    # - modeloFinal: modelo ajustado de tidymodels.
    # - tipoModelo: string con el tipo de modelo.
    # - df: dataframe con los datos empleados para ajustar el modelo (solo SVM).
    # Salida: dataframe con las variables ordenadas por importancia decreciente.
    
    ##############################################################################
    # Funciones de apoyo.
    ##############################################################################
    
    calcularImportanciaRL <- function(modeloFinal) {
      # Determina la importancia de las variables para un modelo de regresión
      # lineal o logística.
      # Entrada:
      # - modeloFinal: modelo ajustado de tidymodels.
      # Salida: dataframe con las variables ordenadas por importancia decreciente.
      
      # Extraer coeficientes.
      ajustado <- workflows::extract_fit_parsnip(modeloFinal)$fit
      coeficientes <- as.data.frame(coef(summary(ajustado)))
      
      # Calcular la importancia como complemento del valor p.
      valorP <- coeficientes[[grep(
        "^Pr", colnames(coeficientes), value = TRUE)]]
      
      importancia <- data.frame(
        Variable = rownames(coeficientes), Importancia = round(1 - valorP, 3))
      
      importancia <- importancia %>% filter(Variable != "(Intercept)")
      return(importancia)
    }
    
    calcularImportanciaRF <- function(modeloFinal) {
      # Determina la importancia de las variables para un modelo random forest.
      # Entrada:
      # - modeloFinal: modelo ajustado de tidymodels.
      # Salida: dataframe con las variables ordenadas por importancia decreciente.
      
      modeloAjustado <- hardhat::extract_fit_engine(modeloFinal)
      
      importanciaBruta <- modeloAjustado$variable.importance
      
      if(is.null(importanciaBruta)) {
        importanciaBruta <- modeloAjustado$fit$variable.importance
      }
      
      if(is.null(importanciaBruta)) {
        return(NULL)
      }
      
      importancia <- as.data.frame(importanciaBruta)
      names(importancia) <- "Importancia"
      
      importancia <- data.frame(
        Variable = row.names(importancia),
        Importancia = round(importancia$Importancia, 3)
      )
      
      return(importancia)
    }
    
    calcularImportanciaSVM <- function(modeloFinal, df) {
      # Determina la importancia de las variables para un modelo máquina de
      # vectores soporte.
      # Entrada:
      # - modeloFinal: modelo ajustado de tidymodels.
      # - df: dataframe con los datos empleados para ajustar el modelo.
      # Salida: dataframe con las variables ordenadas por importancia decreciente.
      
      # Determinar si se trata de clasificación o regresión.
      especificacion <- workflows::extract_spec_parsnip(modeloFinal)
      clasificacion <- especificacion$mode == "classification"
      
      # Inicializar dataframe de importancia.
      importancia <- data.frame(Variable = character(), Importancia = numeric())
      
      # Calcular importancia de las variables:
      # - Clasificación, AUC_ROC univariado por cada predictor.
      # - Regresión, |t| del modelo de regresión lineal univariado por cada
      #   predictor.
      for(nombreVariable in setdiff(names(df), "respuesta")) {
        variable <- df[[nombreVariable]]
        if(!is.numeric(variable) || length(unique(variable)) < 2) next
        
        if(clasificacion) {
          curvaROC <- try(
            pROC::roc(df$respuesta, variable, quiet = TRUE), silent = TRUE)
          
          if(inherits(curvaROC, "try-error") || is.null(curvaROC$auc)) next
          auc <- as.numeric(curvaROC$auc)
          
          importancia <- rbind(
            importancia, data.frame(
              Variable = nombreVariable, Importancia = round(auc, 3)))
        } else {
          modeloUnivariado <- lm(df$respuesta ~ variable)
          t <- summary(modeloUnivariado)$coefficients[2, "t value"]
          
          importancia <- rbind(
            importancia, data.frame(
              Variable = nombreVariable, Importancia = round(abs(t), 3)))
        }
      }
      
      return(importancia)
    }
    
    calcularImportanciaXGB <- function(modeloFinal) {
      # Determina la importancia de las variables para un modelo extreme gradient
      # boosting.
      # Entrada:
      # - modeloFinal: modelo ajustado de tidymodels.
      # Salida: dataframe con las variables ordenadas por importancia decreciente.
      
      modeloAjustado <- hardhat::extract_fit_engine(modeloFinal)
      importancia <- xgboost::xgb.importance(model = modeloAjustado)
      importancia <- importancia[, c("Feature", "Gain")]
      names(importancia) <- c("Variable", "Importancia")
      importancia$Importancia <- round(importancia$Importancia, 3)
      return(as.data.frame(importancia))
    }
    
    
    
    ##############################################################################
    # Funciones de apoyo.
    ##############################################################################
    
    importancia <- switch(
      tipoModelo,
      "RLin" = calcularImportanciaRL(modeloFinal),
      "RLog" = calcularImportanciaRL(modeloFinal),
      "RFE" = calcularImportanciaRF(modeloFinal),
      "RF" = calcularImportanciaRF(modeloFinal),
      "SVM_L" = calcularImportanciaSVM(modeloFinal, df),
      "SVM_R" = calcularImportanciaSVM(modeloFinal, df),
      "XGB" = calcularImportanciaXGB(modeloFinal),
      default = {NULL})
    
    if(is.null(importancia)) {
      return(NULL)
    }
    
    importancia <- dplyr::arrange(importancia, dplyr::desc(Importancia))
    return(importancia)
  }
  
  configurarModelo <- function(df, tipoModelo, parametros = NULL) {
    # Crea un un flujo de entrenamiento según el tipo de modelo a ajustar.
    # Entrada:
    # - df: dataframe para el entrenamiento.
    # - tipoModelo: string con el tipo de modelo.
    # - parametros: lista con los valores de los hiperparámetros (NULL para
    #   regresión lineal o logística).
    # Salida: flujo de entrenamiento.
    
    ##############################################################################
    # Funciones de apoyo.
    ##############################################################################
    
    configurarRLin <- function(receta, tarea) {
      # Define el flujo de trabajo para ajustar un modelo de regresión lineal.
      # Entrada:
      # - receta: receta para la fórmula del modelo.
      # - tarea: string con el nombre de la tarea para el motor.
      # Salida: flujo de trabajo.
      
      # Definir flujo de trabajo.
      flujo <- workflow() %>% add_recipe(receta) %>%
        add_model(linear_reg() %>% set_engine("lm") %>% set_mode(tarea))
      
      return(flujo)
    }
    
    configurarRLog <- function(receta, tarea) {
      # Define el flujo de trabajo para ajustar un modelo de regresión logística.
      # Entrada:
      # - receta: receta para la fórmula del modelo.
      # - tarea: string con el nombre de la tarea para el motor.
      # Salida: flujo de trabajo.
      
      # Definir flujo de trabajo.
      flujo <- workflow() %>% add_recipe(receta) %>%
        add_model(logistic_reg() %>% set_engine("glm") %>% set_mode(tarea))
      
      return(flujo)
    }
    
    configurarRF <- function(receta, tarea, parametros, nPredictores) {
      # Define el flujo de trabajo para ajustar un modelo random forest.
      # Entrada:
      # - receta: receta para la fórmula del modelo.
      # - tarea: string con el nombre de la tarea para el motor.
      # - parametros: lista con los valores de los hiperparámetros.
      # - nPredictores: entero con la cantidad de variables independientes.
      # Salida: flujo de trabajo.
      
      # Asegurar que mtry sea válido.
      if(nPredictores < parametros$mtry) parametros$mtry <- nPredictores
      
      # Configurar el motor de entrenamiento.
      reglaParticion <- if(tarea == "classification") "gini" else "variance"
      
      especificacionRF <- rand_forest(
        trees = parametros$ntree, mtry = parametros$mtry) %>%
        set_mode(tarea) %>% set_engine(
          "ranger", splitrule = reglaParticion, verbose = FALSE,
          importance = "permutation")
      
      # Definir flujo de trabajo.
      flujo <- workflow() %>% add_model(especificacionRF) %>%
        add_recipe(receta)
      
      return(flujo)
    }
    
    configurarSVM_L <- function(receta, tarea, parametros) {
      # Define el flujo de trabajo para ajustar un modelo máquina de vectores
      # soporte con kernel lineal.
      # Entrada:
      # - receta: receta para la fórmula del modelo.
      # - tarea: string con el nombre de la tarea para el motor.
      # - parametros: lista con los valores de los hiperparámetros.
      # Salida: flujo de trabajo.
      
      # Configurar el motor de entrenamiento.
      especificacionSVM <- NULL
      
      if(tarea == "classification") {
        especificacionSVM <- svm_linear(
          cost = parametros$cost) %>% set_mode(tarea) %>%
          set_engine("kernlab", prob.model = TRUE, verbose = FALSE)
      } else {
        especificacionSVM <- svm_linear(
          cost = parametros$cost, margin = parametros$epsilon) %>%
          set_mode(tarea) %>%
          set_engine("kernlab", verbose = FALSE)
      }
      
      # Definir receta base y el flujo de trabajo.
      receta <- receta %>%
        recipes::step_normalize(recipes::all_numeric_predictors())
      
      flujo <- workflow() %>% add_recipe(receta) %>%
        add_model(especificacionSVM)
      
      return(flujo)
    }
    
    configurarSVM_R <- function(receta, tarea, parametros) {
      # Define el flujo de trabajo para ajustar un modelo máquina de vectores
      # soporte con kernel radial.
      # Entrada:
      # - receta: receta para la fórmula del modelo.
      # - tarea: string con el nombre de la tarea para el motor.
      # - parametros: lista con los valores de los hiperparámetros.
      # Salida: flujo de trabajo.
      
      # Configurar el motor de entrenamiento.
      especificacionSVM <- NULL
      
      if(tarea == "classification") {
        especificacionSVM <- svm_rbf(
          cost = parametros$cost, rbf_sigma = parametros$sigma) %>%
          set_mode(tarea) %>% set_engine("kernlab", verbose = FALSE)
      } else {
        especificacionSVM <- svm_rbf(
          cost = parametros$cost, rbf_sigma = parametros$sigma,
          margin = parametros$epsilon) %>% set_mode(tarea) %>%
          set_engine("kernlab", verbose = FALSE)
      }
      
      # Definir receta base y el flujo de trabajo.
      receta <- receta %>%
        recipes::step_normalize(recipes::all_numeric_predictors())
      
      flujo <- workflow() %>% add_recipe(receta) %>%
        add_model(especificacionSVM)
      
      return(flujo)
    }
    
    configurarXGB <- function(receta, tarea, parametros) {
      # Define el flujo de trabajo para ajustar un modelo extreme gradient
      # boosting.
      # Entrada:
      # - receta: receta para la fórmula del modelo.
      # - tarea: string con el nombre de la tarea para el motor.
      # - parametros: lista con los valores de los hiperparámetros.
      # Salida: flujo de trabajo.
      
      # Configurar el motor de entrenamiento.
      especificacionXGB <- boost_tree(
        trees = parametros$nrounds, tree_depth = parametros$max_depth,
        min_n = parametros$min_child_weight, loss_reduction = parametros$gamma,
        sample_size = parametros$subsample, learn_rate = parametros$eta) %>%
        set_mode(tarea) %>% set_engine(
          "xgboost", colsample_bytree = parametros$colsample_bytree,
          counts = FALSE)
      
      # Definir flujo de trabajo.
      flujo <- workflow() %>% add_recipe(receta) %>%
        add_model(especificacionXGB)
      
      return(flujo)
    }
    
    
    
    ##############################################################################
    # Función principal.
    ##############################################################################
    
    # Definir receta y tarea.
    receta <- recipe(as.formula("respuesta ~ ."), data = df)
    tarea <- if(is.factor(df$respuesta)) "classification" else "regression"
    
    # Definir flujo de trabajo
    flujo <- switch(
      tipoModelo,
      "RFE" = configurarRF(receta, tarea, parametros, ncol(df) - 1),
      "RLin" = configurarRLin(receta, tarea),
      "RLog" = configurarRLog(receta, tarea),
      "RF" = configurarRF(receta, tarea, parametros, ncol(df) - 1),
      "SVM_L" = configurarSVM_L(receta, tarea, parametros),
      "SVM_R" = configurarSVM_R(receta, tarea, parametros),
      "XGB" = configurarXGB(receta, tarea, parametros),
      default = {NULL})
    
    return(flujo)
  }
  
  crearMetrica <- function(clasificacion, optimizacion) {
    # Define el conjunto de métricas para un modelo.
    # Entrada:
    # - clasificacion: TRUE para clasificación; FALSE para regresión.
    # - optimizacion: TRUE para métrica principal, FALSE para todas las
    #   métricas.
    # evaluación (irace) o todas las métricas para reporte.
    # Salida: objeto con las métricas.
    
    if(clasificacion) {
      if(optimizacion) return(yardstick::metric_set(yardstick::roc_auc))
      
      return(yardstick::metric_set(
        yardstick::roc_auc, yardstick::pr_auc, yardstick::brier_class,
        yardstick::mcc, yardstick::sens, yardstick::spec))
    } else {
      if(optimizacion) return(yardstick::metric_set(yardstick::rmse))
      return(yardstick::metric_set(yardstick::rmse, yardstick::rsq))
    }
  }
  
  extraerDetalleMetricas <- function(modeloCV, nombres = NOMBRES_METRICAS) {
    # Extrae las métricas de evaluación por pliegue.
    # Entrada:
    # - modeloCV: modelo ajustado con validación cruzada.
    # - nombres: lista con los nombres de las métricas con formato para reporte.
    # Salida: dataframe con las métricas por pliegue.
    
    # Extraer las métricas por pliegue.
    metricas <- collect_metrics(modeloCV, summarize = FALSE) %>%
      mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 3)))
    
    metricas <- renombrarPliegues(metricas)
    
    # Identificar nombres de los pliegues.
    nombresPliegues <- c("Repeticion", "Pliegue")
    nombresPliegues <- nombresPliegues[nombresPliegues %in% names(metricas)]
    
    # Llevar las métricas a formato ancho y con nombres para reporte.
    metricas <- metricas %>%
      select(all_of(nombresPliegues), .metric, .estimate) %>%
      pivot_wider(names_from = .metric, values_from = .estimate) %>%
      rename_with(
        ~ names(nombres)[match(.x, nombres)],
        .cols = setdiff(names(.), nombresPliegues)) %>%
      as.data.frame()
    return(metricas)
  }
  
  extraerPredicciones <- function(modeloCV) {
    # Extrae las predicciones para las observaciones no empleadas en el
    # entrenamiento.
    # Entrada:
    # - modeloCV: objeto de validación cruzada de tidymodels.
    # - threshold: umbral de decisión (solo regresión logística).
    # Salida: dataframe con las predicciones.
    
    # Convertir a dataframe y descartar columnas inútiles.
    predicciones <- collect_predictions(modeloCV) %>%
      mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 3))) %>%
      select(-".config")
    
    predicciones <- renombrarPliegues(predicciones)
    
    # Renombrar y ordenar columnas.
    predicciones <- predicciones %>% rename(
      Observado = "respuesta", Id = ".row")
    
    if(".pred_class" %in% names(predicciones)) {
      predicciones <- predicciones %>% rename(Predicho = ".pred_class")
      
      if(".pred_APRUEBA" %in% names(predicciones)) {
        predicciones <- predicciones %>% rename(
          Prob_APRUEBA = ".pred_APRUEBA", Prob_REPRUEBA = ".pred_REPRUEBA")
      }
    } else {
      predicciones <- predicciones %>% rename(Predicho = ".pred")
    }
    
    columnas <- c(
      "Repeticion", "Pliegue", "Id", "Observado", "Predicho", "Prob_APRUEBA",
      "Prob_REPRUEBA")
    
    predicciones <- predicciones %>% select(any_of(columnas)) %>% as.data.frame()
    return(predicciones)
  }
  
  extraerResumenMetricas <- function(
    datosModelo, modeloCV, modeloFinal = NULL, nombres = NOMBRES_METRICAS) {
    # Extrae las métricas de evaluación resumidas.
    # Entrada:
    # - datosModelo: descriptor con los datos generales del modelo.
    # - modeloCV: modelo ajustado con validación cruzada.
    # - modeloFinal: modelo ajustado de tidymodels.
    # - nombres: lista con los nombres de las métricas con formato para reporte.
    # Salida: dataframe con las métricas por pliegue.

    # Extraer métricas originales.
    metricas <- collect_metrics(modeloCV, summarize = TRUE, type = "wide") %>%
      select(-".config") %>% mutate(
        across(.cols = where(is.numeric), .fns = ~ round(.x, 3))) %>%
      as.data.frame()
    
    # Ordenar las métricas.
    columnas <- intersect(colnames(metricas), nombres)
    
    if(length(columnas) > 0) {
      metricas <- metricas %>% rename_with(
          ~ names(nombres)[match(.x, nombres)], .cols = columnas)
    }
    
    if(datosModelo$clasificacion) {
      metricas <- metricas %>% select(any_of(
        c("AUC_ROC", "AUC_PR", "Brier", "MCC", "Sens", "Espec")))
    } else {
      metricas <- metricas %>% select(any_of(c("RMSE", "R2")))
    }
    
    # Para clasificación, ajustar umbral y recalcular métricas dependientes de
    # de la clasificación.
    if(datosModelo$clasificacion && !is.null(modeloFinal)) {
      # Ajustar umbral.
      probabilidades <- predict(modeloFinal, datosModelo$df, type = "prob")
      
      curvaRoc <- pROC::roc(
        datosModelo$df$respuesta, probabilidades$.pred_REPRUEBA)
      
      umbral <- pROC::coords(
        curvaRoc, "best", best.method = "youden")["threshold"]
      
      umbral <- round(as.numeric(umbral), 3)
      
      # Obtener predicciones con el umbral ajustado.
      predicciones <- factor(ifelse(
        probabilidades$.pred_REPRUEBA >= umbral, "REPRUEBA", "APRUEBA"),
        levels = c("REPRUEBA", "APRUEBA"))
      
      # Recalcular las métricas dependientes de la clasificación.
      metricas$MCC <- round(
        yardstick::mcc_vec(
          truth = datosModelo$df$respuesta, estimate = predicciones), 3)
      
      metricas$Sens <- round(
        yardstick::sens_vec(
          truth = datosModelo$df$respuesta, estimate = predicciones), 3)
      
      metricas$Espec <- round(
        yardstick::spec_vec(
          truth = datosModelo$df$respuesta, estimate = predicciones), 3)
      
      # Agregar la matriz de confusión.
      matriz <- yardstick::conf_mat(data.frame(
        Observado = datosModelo$df$respuesta, Predicho = predicciones),
        truth = "Observado", estimate = "Predicho")
      
      matriz <- as.data.frame(matriz$table)
      
      metricas$VP <- matriz$Freq[matriz$Prediction == "REPRUEBA" &
                                   matriz$Truth == "REPRUEBA"]
      
      metricas$FP <- matriz$Freq[matriz$Prediction == "REPRUEBA" &
                                   matriz$Truth == "APRUEBA"]
      
      metricas$FN <- matriz$Freq[matriz$Prediction == "APRUEBA" &
                                   matriz$Truth == "REPRUEBA"]
      
      metricas$VN <- matriz$Freq[matriz$Prediction == "APRUEBA" &
                                   matriz$Truth == "APRUEBA"]
    }
    
    return(metricas)
  }
  
  renombrarPliegues <- function(df) {
    # Da formato a las columnas que identifican los pliegues.
    # Entrada:
    # - df: dataframe con reporte de resultados por pliegue.
    # Salida: dataframe formateado.
    
    # Formatear columnas con los pliegues.
    if("id2" %in% colnames(df)) {
      df <- df %>% rename(Repeticion = id, Pliegue = id2) %>%
        mutate(
          Repeticion = as.integer(gsub("^Repeat", "", Repeticion)),
          Pliegue    = as.integer(gsub("^Fold", "", Pliegue)))
    } else {
      df <- df %>% rename(Pliegue = id) %>%
        mutate(Pliegue = as.integer(gsub("^Fold", "", Pliegue)))
    }
    
    return(df)
  }
  
  
  
  ##############################################################################
  # Función principal.
  ##############################################################################
  
  # Configurar elementos para ajuste del modelo.
  df <- datosModelo$df

  # Si es para optimización:
  # - Se rebibe la configuración de hiperparámetros.
  # - Usar validación cruzada simple.
  # - Solo se devuelve la métrica para optimización.
  if(optimizacion) {
    flujo <- configurarModelo(df, tipoModelo, parametros)
    set.seed(semilla)
    
    cv <- fit_resamples(
      flujo, resamples = datosModelo$plieguesOptimizacion,
      control = crearControl(tipoModelo),
      metrics = crearMetrica(datosModelo$clasificacion, optimizacion))
    
    resumen <- extraerResumenMetricas(
      datosModelo, cv, nombres = nombresMetricas)
    
    nombreMetrica <- if(datosModelo$clasificacion) METRICA_CLAS else METRICA_REG
    return(resumen[[nombreMetrica]])
  }
  
  # Ajustar el modelo definitivo.
  # Ajustar hiperparámetros si no se entregan.
  if(!tipoModelo %in% c("RLin", "RLog") && is.null(parametros)) {
    parametros <- ajustarParametros(datosModelo, tipoModelo, semilla)
  }

  # Ajustar el modelo con validación cruzada.
  flujo <- configurarModelo(df, tipoModelo, parametros)
  set.seed(semilla)

  cv <- fit_resamples(
    flujo, resamples = datosModelo$plieguesFinal,
    control = crearControl(tipoModelo),
    metrics = crearMetrica(datosModelo$clasificacion, optimizacion))
  
  # Ajustar el modelo final.
  set.seed(semilla)
  final <- fit(flujo, data = df)
  
  # Reporte de resultados.
  predicciones <- extraerPredicciones(cv)
  metricas <- extraerDetalleMetricas(cv, nombresMetricas)
  resumen <- extraerResumenMetricas(datosModelo, cv, final, nombresMetricas)
  
  importancia <- if(startsWith(tipoModelo, "SVM")) {
    receta <- workflows::extract_recipe(final)
    normalizado <- bake(receta, new_data = datosModelo$df)
    normalizado$respuesta <- datosModelo$df$respuesta
    calcularImportancia(final, tipoModelo, normalizado)
  } else {
    calcularImportancia(final, tipoModelo)
  }

  # Empaquetar el resultado.
  modelo <- list(
    clasificador = datosModelo$clasificacion, respuesta = datosModelo$respuesta,
    nombre = datosModelo$nombre, tipo = tipoModelo, datos = df,
    predictores = paste(importancia$Variable, collapse = ", "),
    parametros = parametros, predicciones = predicciones,
    importancia = importancia, metricas = metricas, resumen = resumen,
    final = final)

  return(modelo)
}
