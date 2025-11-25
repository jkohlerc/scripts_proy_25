################################################################################
# Funciones para gestionar paralelización.
################################################################################

activarParalelismo <- function(nucleos = NUCLEOS, semilla = SEMILLA) {
  # Crea el cluster y carga los elementos necesarios en los nodos.
  # Entrada: ninguna.
  # Salida: cluster.
  
  # Fijar semilla para paralelización.
  RNGkind("L'Ecuyer-CMRG")
  set.seed(semilla)
  
  # Crear cluster y activar paralelización.
  cluster <- makePSOCKcluster(nucleos)
  registerDoParallel(cluster)
  
  # Inicializar RNG reproducible en los nodos
  parallel::clusterSetRNGStream(cluster, iseed = semilla)
  
  # Exportar entorno de trabajo necesario a los nodos.
  parallel::clusterExport(
    cluster, varlist = c("SEMILLA"), envir = environment())
  
  # Exportar paquetes y scripts.
  clusterEvalQ(cluster, {
    library(kernlab)
    library(ranger)
    library(themis)
    library(xgboost)
  })
  
  return(cluster)
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
# Funciones de apoyo para entrada y salida.
################################################################################

cargarDatos <- function(
    rutaDfLimpio = DATASET_LIMPIO, rutaDfAplanado = DATASET_APLANADO) {
  # Carga el conjunto de datos aplanado. Si no existe, lo crea.
  # Entrada:
  # - rutaDfLimpio: ruta del archivo con el dataset limpio.
  # - rutaDfAplanado: ruta del archivo con el dataset aplanado.
  # Salida: lista con los siguientes elementos:
  # - ingreso: dataframe con variables aplanadas de ingreso.
  # - cinf: dataframe con variables de competencias informacionales.
  # - respuestas: dataframe con variables de respuesta.
  
  # Cargar o construir el dataset aplanado.
  df <- NULL
  
  if(file.exists(rutaDfAplanado)) {
    df <- read.csv2(rutaDfAplanado, stringsAsFactors = TRUE)
  } else {
    df <- read.csv2(rutaDfLimpio, stringsAsFactors = TRUE)
    
    # Recodificar predictores dicotómicos.
    df <- df %>% mutate(SEXO = if_else(SEXO == "FEMENINO", 1, 0))
    df <- df %>% mutate(PREF_1 = if_else(PREF_1 == "S", 1, 0))
    
    # Aplanar dependencia (tipo de establecimiento secundario).
    df$DEP <- fct_recode(
      df$DEP, "MU" = "MUNICIPAL", "PP" = "PART. PAGADO", "PS" = "PART. SUBV.")
    
    df <- df  %>% dummy_cols(
      select_columns = "DEP", remove_first_dummy = TRUE,
      remove_selected_columns = TRUE)
    
    df <- df %>% relocate(starts_with("DEP_"), .after = PREF_1)
    
    # Guardar el conjunto de datos aplanado.
    crearRuta(rutaDfAplanado)
    write.csv2(df, rutaDfAplanado, row.names = FALSE)
  }
  
  # Separar predictores (por tipo) y respuestas.
  predictores <- df %>% select(-c(starts_with("SIT_"), starts_with("NOTA_")))
  predictores <- as.data.frame(lapply(predictores, as.numeric))
  
  ingreso <- predictores %>%
    select(-c(starts_with("A_"), starts_with("O_")))
  
  cinf <- predictores %>%
    select(c(starts_with("A_"), starts_with("O_")))
  
  respuestas <- df %>% select(c(starts_with("SIT_"), starts_with("NOTA_")))
  
  # Asegurar que REPRUEBA sea siempre el primer nivel en las respuestas
  # dicotómicas.
  respuestas <- respuestas %>%
    mutate(across(
      .cols = starts_with("SIT_"),
      .fns = ~ factor(.x, levels = c("REPRUEBA", "APRUEBA"))))
  
  # Construir lista con los subconjuntos de columnas.
  datos <- list(ingreso = ingreso, cinf = cinf, respuestas = respuestas)
  return(datos)
}

construirDataframe <- function(listaDatos, curso, clasificacion, dfBase) {
  # Crea el dataframe inicial para un problema (clasificación o regresión),
  # curso y subconjunto de predictores.
  # Entrada:
  # - listaDatos: lista de dataframes con los distintos subconjuntos de
  #   variables.
  # - curso: string con el nombre del curso.
  # - clasificacion: TRUE para clasificación; FALSE para regresión.
  # - dfBase: dataframe utilizado para el ajuste del modelo base (solo para el
  #   modelo final).
  # Salida: string con la ruta para guardar un archivo.
  
  if(is.null(dfBase)) {
    prefijo <- if(clasificacion) "SIT" else "NOTA"
    respuesta <- paste(prefijo, curso, sep = "_")
    df <- data.frame(respuesta = listaDatos$respuestas[[respuesta]])
    df <- cbind(df, listaDatos$ingreso)
  } else {
    df <- cbind(dfBase, datos$cinf)
  }
  
  return(df)
}

crearRutaSalida <- function(
    datosModelo, usarNombre, rutaBase = RUTA_RESULTADOS) {
  # Crea el string con la ruta de salida para un archivo y genera la ruta de
  # archivos correspondientes en disco.
  # Entrada:
  # - datosModelo: descriptor con los datos generales del modelo.
  # - usarNombre: TRUE indica que la ruta debe incluir el nombre del modelo;
  #   FALSE, que no.
  # - rutaBase: ruta de la carpeta principal de resultados.
  # Salida: string con la ruta para guardar un archivo.
  
  ruta <- rutaBase
  
  if(!is.null(datosModelo$parsimonia)) {
    experimento <- if(datosModelo$parsimonia) "Parsimonia" else "Absoluto"
    ruta <- paste(rutaBase, experimento, sep = "/")
  }
  
  ruta <- paste(ruta, datosModelo$respuesta, sep = "/")
  
  if(usarNombre) ruta <- paste(ruta, datosModelo$nombre, sep = "/")
  if(!dir.exists(ruta)) dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
  return(ruta)
}

generarCurvaPR <- function(modelo) {
  # Genera la curva PR de un clasificador.
  # Entrada:
  # - modelo: modelo clasificador.
  # Salida: objeto con la curva PR.
  
  observado <- ifelse(modelo$predicciones$Observado == "REPRUEBA", 1, 0)
  positivos <- modelo$predicciones$Prob_REPRUEBA
  negativos <- modelo$predicciones$Prob_APRUEBA
  
  curva <- PRROC::pr.curve(
    scores.class0 = negativos,
    scores.class1 = positivos, curve = TRUE)
  
  return(curva)
}

generarCurvaROC <- function(modelo) {
  # Genera la curva ROC de un clasificador.
  # Entrada:
  # - modelo: modelo clasificador.
  # Salida: objeto con la curva ROC.
  
  observado <- modelo$predicciones$Observado
  
  curva <- pROC::roc(
    observado, modelo$predicciones$Prob_REPRUEBA,
    levels = c("APRUEBA", "REPRUEBA"), direction = "<", quiet = TRUE)
  
  return(curva)
}

graficarImportancia <- function(
    modelo, titulo = "Importancia de las variables") {
  # Crea un gráfico con la importancia de las variables de un modelo.
  # Entrada:
  # - modelo: objeto con el modelo.
  # - titulo: título del gráfico.
  # Salida: objeto con el gráfico generado.
  
  # Si el modelo fue ajustado directamente con ranger, no tiene la estructura
  # definida en el pipeline general.
  if(is.null(modelo$tipo)) {
    importancia <- as.data.frame(modelo$variable.importance)
    names(importancia) <- "Importancia"
    
    importancia <- data.frame(
      Variable = row.names(importancia),
      Importancia = round(importancia$Importancia, 3))
    
    importancia <- dplyr::arrange(importancia, dplyr::desc(Importancia))
    modelo <- list(final = modelo, tipo = "RFE", importancia = importancia)
  }
  
  # Definir el tipo de importancia.
  tipoImportancia <- switch(
    modelo$tipo,
    "RLin" = "(|t|)",
    "RLog" = "(|Z|)",
    "RFE" = "(permutación)",
    "RF" = "(permutación)",
    "SVM_L" = if(modelo$clasificador) {
      "(AUC-ROC univariado)"
    } else {
      "(|t| univariado)"
    },
    "SVM_R" = if(modelo$clasificador) {
      "(AUC-ROC univariado)"
    } else {
      "(|t| univariado)"
    },
    "XGB" = "(ganancia)",
    ""
  )
  
  # Definir formato para marcas de los ejes.
  formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 3))
  
  # Generar el gráfico.
  g <- ggplot(
    modelo$importancia,
    # aes(x = Variable, y = Importancia, fill = Importancia)) +
    aes(
      x = reorder(
        Variable, -Importancia), y = Importancia, fill = Importancia)) +
    geom_col(show.legend = FALSE) +
    scale_fill_gradient(low = "skyblue", high = "navy") +
    labs(
      title = titulo, x = "Variable",
      y = paste("Importancia", tipoImportancia, sep = " ")) +
    theme_pubr() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = formatoEjes)
  
  return(g)
}

guardarGrafico <- function(datosModelo, grafico, archivo, usarNombre, ...) {
  # Genera la ruta, añade la extensión y guarda un gráfico.
  # Entrada:
  # - datosModelo: descriptor con los datos generales del modelo.
  # - grafico: objeto con el gráfico.
  # - archivo: string con el nombre del archivo sin ruta ni extensión.
  # - usarNombre: TRUE indica que la ruta debe incluir el nombre del modelo;
  # - ...: argumentos adicionales para ggsave().
  # Salida: archivo guardado.
  
  ruta <- crearRutaSalida(datosModelo, usarNombre)
  archivo <- paste0(ruta, "/", archivo, ".pdf")
  ggsave(archivo, grafico, device = "pdf", units = "in", ...)
}

guardarModelos <- function(datosModelo, modelos, archivo) {
  # Genera la ruta, añade la extensión y guarda una lista de modelos.
  # Entrada:
  # - datosModelo: descriptor con los datos generales del modelo.
  # - grafico: objeto con el gráfico.
  # - archivo: string con el nombre del archivo sin ruta ni extensión.
  # Salida: archivo guardado.
  
  ruta <- crearRutaSalida(datosModelo, usarNombre = TRUE)
  archivo <- paste0(ruta, "/", archivo, ".rds")
  saveRDS(modelos, archivo)
}

guardarReporte <- function(datosModelo, reporte, archivo, usarNombre) {
  # Genera la ruta, añade la extensión y guarda un dataframe.
  # Entrada:
  # - datosModelo: descriptor con los datos generales del modelo.
  # - reporte: dataframe con el reporte.
  # - archivo: string con el nombre del archivo sin ruta ni extensión.
  # - usarNombre: TRUE indica que la ruta debe incluir el nombre del modelo;
  # Salida: archivo guardado.
  
  ruta <- crearRutaSalida(datosModelo, usarNombre)
  archivo <- paste0(ruta, "/", archivo, ".csv")
  guardarDataframe(reporte, archivo)
}

inicializarDescriptor <- function(
    listaDatos, curso, clasificacion, dfBase = NULL, parsimonia = NULL) {
  # Crea el descriptor para ajustar un modelo dados un problema (clasificación o
  # regresión), curso y subconjunto de predictores.
  # Entrada:
  # - listaDatos: lista de dataframes con los distintos subconjuntos de
  #   variables.
  # - curso: string con el nombre del curso.
  # - clasificacion: TRUE para clasificación; FALSE para regresión.
  # - dfBase: dataframe utilizado para el ajuste del modelo base (solo para el
  #   modelo final).
  # - parsimonia: TRUE para usar parsimonia en el proceso de selección de
  #   características; FALSE si se quiere escoger el óptimo absoluto. NULL si
  #   solo se ejecutará un único experimento usando parsimonia.
  # Salida: lista con los elementos para el modelo.
  
  prefijo <- if(clasificacion) "SIT_" else "NOTA_"
  respuesta = paste0(prefijo, curso)
  df = construirDataframe(datos, curso, clasificacion, dfBase)
  
  datosModelo <- list(
    clasificacion = clasificacion, respuesta = respuesta, df = df,
    nombre = if(is.null(dfBase)) "Base" else "Final",
    plieguesOptimizacion = crearFolds(df, optimizacion = TRUE),
    plieguesFinal = crearFolds(df, optimizacion = FALSE))
  
  if(!is.null(parsimonia)) {
    datosModelo$parsimonia <- parsimonia
  }
  
  return(datosModelo)
}

reportarParametros <- function(parametros) {
  # Convierte una lista de parámetros a string.
  # Entrada:
  # - parametros: lista de parámetros.
  # Salida: string con los parámetros.
  
  nombres <- names(parametros)
  textos <- c()
  enteros <- c("ntree", "mtry", "max_depth", "min_child_weight")
  logaritmos <- c("cost", "sigma")
  
  for(nombre in nombres) {
    if(nombre %in% enteros) {
      actual <- paste(nombre, as.character(parametros[[nombre]]), sep = "=")
      textos <- c(textos, actual)
    } else if(nombre %in% logaritmos) {
      exponente <- log2(parametros[[nombre]])
      valor <- formatearFlotante(exponente, 3)
      actual <- sprintf("%s = 2^%s", nombre,valor)
      textos <- c(textos, actual)
    } else {
      actual <- paste(nombre, formatearFlotante(parametros[[nombre]], 3))
      textos <- c(textos, actual)
    }
  }
  
  reporte <- paste(textos, collapse = "; ")
  return(reporte)
}



################################################################################
# Funciones de apoyo: entrenamiento y evaluación de modelos.
################################################################################

crearControl <- function(tipoModelo) {
  # Crea un objeto de control para un tipo de modelo a ajustar.
  # Entrada:
  # - tipoModelo: string con el tipo de modelo.
  # Salida: objeto de control.
  
  paralelizar <- foreach::getDoParWorkers() > 1
  
  control <- if(tipoModelo %in% c("RF", "XGB")) {
    control_resamples(save_pred = TRUE, parallel_over = "resamples") 
  } else {
    control_resamples(save_pred = TRUE, allow_par = paralelizar) 
  }
  return(control)
}

crearFolds <- function(
    df, optimizacion, semilla = SEMILLA, configuracion = CV) {
  # Obtiene folds para validación cruzada.
  # Entrada:
  # - df: dataframe para el entrenamiento.
  # - optimizacion: TRUE para validacion cruzada simple; FALSE para validación
  #   cruzada con repeticiones.
  # - semilla: semilla.
  # - configuracion: lista con la cantidad de pliegues y repeticiones.
  # Salida: objeto con los pliegues.
  
  clasificacion <- is.factor(df$respuesta)
  estratificar <- if(clasificacion) "respuesta" else NULL
  set.seed(semilla)
  
  if(optimizacion) {
    pliegues <- vfold_cv(
      data = df, v = configuracion$folds, strata = all_of(estratificar))
  } else {
    pliegues <- vfold_cv(
      data = df, v = configuracion$folds, repeats = configuracion$repeticiones,
      strata = all_of(estratificar))
  }
  
  return(pliegues)
}

