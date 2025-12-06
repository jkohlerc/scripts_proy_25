################################################################################
# Configuración
################################################################################

# Cargar funciones de apoyo.
source("Scripts/0 Comun/Utilidades.R")
source("Scripts/0 Comun/Estadisticas.R")

# Cargar paquetes.
importarPaquetes(c(
  "caret", "clValid", "combinat", "doParallel", "doRNG", "foreach", "ggpubr",
  "patchwork", "pROC","PRROC", "reshape2", "rstatix", "tidyverse"))



################################################################################
# Constantes
################################################################################

NUCLEOS <-  detectCores() - 2
RUTA_RESULTADOS <- "Resultados/2 Caracterizacion"
DATASET_LIMPIO <- "Datos/3 Limpio/Dataset limpio.csv"
DATASET_NORMALIZADO <- "Datos/4 Caracterizacion/Dataset normalizado.csv"
DATASET_CLUSTERS <- "Datos/4 Caracterizacion/Dataset clusters.csv"
K_MIN <- 2
K_MAX <- 10
SUBCONJUNTOS <- c("CIA", "CIO", "CI")
SEXOS <- c("TODOS", "FEMENINO", "MASCULINO")
CURSOS <- c("ALG", "CAL", "FIS", "PPA")

NOMBRES_CURSOS <- list(
  ALG = "Álgebra I", CAL = "Cálculo I", FIS = "Física I", PPA = "PPA")



################################################################################
# Funciones de configuración.
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

desactivarParalelismo <- function(cluster) {
  # Detiene el clúster de paralelización y vuelve al procesamiento secuencial.
  # Entrada:
  # - cluster: cluster.
  # Salida: ninguna.
  
  if (!is.null(cluster)) {
    # Detener el cluster.
    stopCluster(cluster)
    
    # Volver a procesamiento secuencial.
    if (getDoParRegistered()) {
      registerDoSEQ()
    }
  }
}



################################################################################
# Funciones para determinar y evaluar agrupamientos.
################################################################################

evaluarClusters <- function(df, subconjunto) {
  # Realiza el agrupamiento para diferentes valores de k y obtiene métricas
  # para evaluar el resultado. Selecciona el mejor agrupamiento.
  # Entrada:
  # - df: dataframe con el conjunto de datos normalizado
  # - subconjunto: string con el nombre del subconjunto.
  # Salida: objeto con el agrupamiento.
  
  # Generar distintas cantidades de clusters y calcular conectividad, silueta
  # media e índice de Dunn.
  suppressWarnings({
    evaluacion <- clValid(df, nClust = K_MIN:K_MAX, clMethods = "kmeans",
                          validation = "internal", metric = "euclidean",
                          neighbSize = 10)
  })
  
  # Extraer las métricas como data frame.
  metricas <- melt(evaluacion@measures)
  colnames(metricas) <- c("Metrica", "K", "Algoritmo", "Valor")
  metricas$Algoritmo <- NULL
  metricas$Valor <- round(metricas$Valor, 3)
  
  metricas$Metrica <- fct_recode(
    metricas$Metrica, Conectividad = "Connectivity", Silueta = "Silhouette")
  
  metricas <- metricas %>% pivot_wider(
    names_from = "Metrica", values_from = "Valor")
  
  # Guardar las métricas como archivo .csv.
  archivo <- paste(RUTA_RESULTADOS, subconjunto, "Metricas.csv", sep = "/")
  crearRuta(archivo)
  write.csv2(metricas, archivo, row.names = FALSE)
  
  # Determinar la cantidad de clusters.
  k <- seleccionarK(metricas)
  clusters <- evaluacion@clusterObjs$kmeans[[as.character(k)]]
  
  # Guardar el objeto con los clusters generados en un archivo .rds.
  archivo <- paste(RUTA_RESULTADOS, subconjunto, "Clusters.RDS", sep = "/")
  saveRDS(clusters, archivo)
  return(clusters)
}

filtrarCompetenciasNormalizadas <- function(df, subconjunto) {
  # Filtra un subconjunto de competencias informacionales normalizadas.
  # Entrada:
  # - Dataframe con el conjunto de datos normalizado.
  # - String con el nombre del subconjunto.
  # Salida: Dataframe con competencias informacionales normalizadas del
  # subconjunto.
  
  filtrado <- df %>% select(ends_with("_N"))
  
  if(subconjunto == "CIA") {
    filtrado <- filtrado %>% select(starts_with("A_"))
  } else if(subconjunto == "CIO") {
    filtrado <- filtrado %>% select(starts_with("O_"))
  }
  
  return(filtrado)
}

normalizarCInf <- function() {
  # Carga el dataset limpio y normaliza variables correspondientes a
  # competencias informacionales.
  # Entrada: ninguna.
  # Salida: data frame con competencias informacionales normalizadas.
  
  if(file.exists(DATASET_NORMALIZADO)) {
    df <- read.csv2(DATASET_NORMALIZADO, stringsAsFactors = TRUE)
  } else {
    # Cargar datos y separar competencias informacionales.
    df <- read.csv2(DATASET_LIMPIO, stringsAsFactors = TRUE)
    cinf <- df %>% select(c(starts_with("A_"), starts_with("O_")))
    otras <- df %>% select(-all_of(colnames(cinf)))
    
    # Normalizar competencias informacionales.
    procesamiento <- preProcess(cinf, method = c("center", "scale"))
    cInfNorm <- data.frame(predict(procesamiento, cinf))
    cInfNorm <- cInfNorm %>% rename_all(~ paste0(., "_N"))
    
    # Armar dataset con predictores normalizados.
    df <- cbind(cinf, cInfNorm, otras)
    
    # Guardar dataset normalizado.
    crearRuta(DATASET_NORMALIZADO)
    write.csv2(df, DATASET_NORMALIZADO, row.names = FALSE)
  }
  
  return(df)
}

seleccionarK <- function(metricas) {
  # Selecciona la cantidad ideal de clusters para un subconjunto dado. Si los
  # métodos de conectividad y de silueta media coinciden, se entrega ese valor
  # de k. En caso contrario, se escoge el valor de k asociado al mayor índice de
  # Dunn.
  # Entrada: dataframe con las métricas de evaluación.
  # Salida: entero con el valor de k seleccionado.
  
  # Seleccionar mejores agrupamientos según conectividad.
  mejorConect <- min(metricas$Conectividad)
  kConectividad <- metricas[metricas$Conectividad == mejorConect, "K"][[1]]
  
  # Seleccionar mejores agrupamientos según silueta media.
  mejorSilueta <- max(metricas$Silueta)
  kSilueta <- metricas[metricas$Silueta == mejorSilueta, "K"][[1]]
  
  # Determinar si hay consenso entre ambas métricas.
  consenso <- intersect(kConectividad, kSilueta)
  
  # Seleccionar mejor agrupamiento según principio de parsimonia:
  # - Si hay consensos entre silueta y conectividad, se escoge el menor valor de
  #   K en que ambas métricas coincidan.
  # - En caso contrario, se desempata según índice de Dunn (el menor valor de K
  #   si hay más de uno en el desempate).
  if(length(consenso) > 0) {
    k <- min(consenso)
  } else {
    candidatos <- metricas %>%
      filter(K %in% unique(c(kConectividad, kSilueta)))
    
    mejorDunn <- max(candidatos$Dunn)
    kDunn <- candidatos[candidatos$Dunn == mejorDunn, "K"]
    k <- min(kDunn)
  }
  
  return(k)
}



################################################################################
# Funciones comunes para reportes.
################################################################################

combinarGraficos <- function(graficos, titulo, nombreArchivo) {
  # Crea un gráfico combinado de dos columnas con un título general.
  # Entrada:
  # - graficos: lista de objetos ggplot2.
  # - título: título del gráfico combinado.
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - nombreArchivo: string con el nombre del archivo (solo con subcarpeta,
  #   sin extensión).
  # Salida: archivo .pdf con el gráfico combinado.
  
  # Crear gráfico combinado.
  g <- patchwork::wrap_plots(graficos, ncol = 2)
  
  g <- g + patchwork::plot_annotation(
    title = titulo,
    theme = theme(plot.title = ggplot2::element_text(size = 14, face = "bold")),
    tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
    theme(plot.tag.position = "bottom")
  
  # Guardar el gráfico resultante.
  columnas <- 2
  filas <- ceiling(length(graficos) / columnas)
  ancho <- 6 * columnas
  alto <- 5 * filas
  crearRuta(nombreArchivo)
  ggsave(filename = nombreArchivo, plot = g, width = ancho, height = alto)
}

esSignificativa <- function(prueba) {
  # Extrae el valor p del reporte de una prueba estadística (solo ómnibus si
  # corresponde) y determina si es o no significativa en formato para reporte.
  # Entrada:
  # - prueba: dataframe con el reporte de una prueba estadística.
  # Salida: string (Sí/NO) con el resultado.
  
  p <- prueba[1, "p"]
  
  if (grepl("\\*", p)) {
    return("Sí")
  } else {
    return("No")
  }
}

filtrarDatos <- function(df, subconjunto, sexo, predictores, normal = FALSE,
                         cinf = FALSE) {
  # Filtra las variables y observaciones de interés desde el conjunto de datos.
  # Entrada:
  # - df: dataframe normalizado con los clusters añadidos.
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - sexo: sexo a considerar en el análisis.
  # - predictores: variables independientes a considerar en el análisis.
  # - normal: booleano que indica si se requieren las variables normalizadas de
  #   competencias informacionales.
  # - cinf: booleano que indica si se quiere la totalidad de competencias
  #   informacionales.
  # Salida: conjunto de datos filtrado.
  
  # Seleccionar variables a considerar.
  filtrado <- NULL
  
  if(predictores == "RENDIMIENTO") {
    filtrado <- datos %>% select(c(SEXO, starts_with("SIT_"),
                                   starts_with("NOTA_")))
  } else {
    filtrado <- datos %>% select(-c(starts_with("clusters"),
                                    starts_with("SIT_"), starts_with("NOTA_")))
    
    if(predictores == "INGRESO") {
      filtrado <- filtrado %>% select(-c(starts_with("A_"), starts_with("O_")))
      df <- cbind(df, filtrado)
    } else {
      filtrado <- filtrado %>% select(c(SEXO, starts_with("A_"),
                                        starts_with("O_")))
      
      if(normal) {
        filtrado <- filtrado %>% select(c(SEXO, ends_with("_N")))
      } else {
        filtrado <- filtrado %>% select(-ends_with("_N"))
      }
      
      if(subconjunto == "CIA" && !cinf) {
        filtrado <- filtrado %>% select(-starts_with("O_"))
      } else {
        if(subconjunto == "CIO" && !cinf) {
          filtrado <- filtrado %>% select(-starts_with("A_"))
        }
      }
    }
  }
  
  # Seleccionar los clusters.
  if(subconjunto == "CIA") {
    filtrado$cluster <- df$clusters_CIA
  } else {
    if(subconjunto == "CIO") {
      filtrado$cluster <- df$clusters_CIO
    } else {
      filtrado$cluster <- df$clusters_CI
    }
  }
  
  # Seleccionar solo el sexo de interés.
  if(sexo != "TODOS") {
    filtrado <- filtrado %>% filter(SEXO == sexo)
  }
  
  # Descartar la variable SEXO si no es necesaria.
  if(predictores != "INGRESO") {
    filtrado$SEXO <- NULL
  } else {
    if(sexo != "TODOS") {
      filtrado$SEXO <- NULL
    }
  }
  
  return(filtrado)
}

generarNombreArchivo <- function(sexo, subconjunto, identificador) {
  # Genera el nombre de un archivo de gráficos (con ruta).
  # Entrada:
  # - sexo: string con el sexo considerado.
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - identificador: string que distingue el gráfico específico.
  # Salida: string con el nombre del archivo.
  
  archivo <- paste(tolower(subconjunto), identificador, tolower(sexo), sep = "_")
  archivo <- paste(RUTA_RESULTADOS, subconjunto, archivo, sep = "/")
  archivo <- paste0(archivo, ".pdf")
  return(archivo)
}

graficarCategorica <- function(df, variable, prueba) {
  # Genera un gráfico de barras apiladas por grupos para una variable
  # categórica, incorporando el valor p en caso de diferencias significativas.
  # Entrada:
  # - df: conjunto de datos con las variables y observaciones de interés.
  # - variable: string con el nombre de la variable de interés.
  # - prueba: data frame con los resultados de la comparación.
  # Salida: objeto con el gráfico generado.
  
  #Definir paleta de colores.
  coloresBase <- c("#00BFC4", "#F8766D", "#BE55BF")
  cantidadNiveles <- length(levels(as.factor(df[[variable]])))
  colores <- coloresBase[1:cantidadNiveles]
  
  # Calcular proporciones por grupo.
  proporciones <- df %>%
    group_by(cluster, .data[[variable]]) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(cluster) %>%
    mutate(
      proporcion = count / sum(count),
      etiqueta = paste0(
        count, " (",
        formatearFlotante(proporcion * 100), "%)"))
  
  # Crear el gráfico.
  g <- ggbarplot(proporciones,  x = "cluster", y = "proporcion",
                 fill = variable, xlab = "Grupo",
                 ylab = paste("Proporciones", variable), legend = "top",
                 legend.title = "") + scale_fill_manual(values = colores)
  
  g <- g + scale_y_continuous(labels = scales::percent_format())
  
  g <- g + theme(legend.position = "top", legend.justification = "center",
                 legend.text = element_text(size = 10),
                 axis.text.x = element_text(size = 10))
  
  # Agregar etiquetas con frecuencias y proporciones.
  g <- g + geom_text(data = proporciones,
                     aes(x = cluster, y = proporcion, label = etiqueta,
                         group = .data[[variable]]),
                     position = position_stack(vjust = 0.5), size = 4,
                     color = "black")
  
  # Definir límite superior para fijar altura de las barras dejando espacio
  # para marcar diferencias significativas.
  maxY <- proporciones %>% group_by(cluster) %>%
    summarise(total = sum(proporcion)) %>% pull(total) %>% max()
  
  yP <- maxY * 1.05
  g <- g + coord_cartesian(ylim = c(0, yP))
  
  # Agregar el p-valor si es significativo.
  if(prueba$p < ALFA) {
    maxY <- proporciones %>% group_by(cluster) %>%
      summarise(total = sum(proporcion)) %>% pull(total) %>% max()
    
    yP <- maxY * 1.05
    
    dfP <- data.frame(
      group1 = "1", group2 = "2", y.position = yP,
      p = paste0("p = ", formatearFlotante(prueba$p))
    )
    
    g <- g + stat_pvalue_manual(
      dfP, label = "p", tip.length = 0.02, y.position = "y.position",
      inherit.aes = FALSE)
  }
  
  return(g)
}

graficarNumerica <- function(df, variable, prueba) {
  # Genera un gráfico de cajas por grupos para una variable numérica,
  # incorporando valores p en caso de diferencias significativas.
  # Entrada:
  # - df: conjunto de datos con las variables y observaciones de interés.
  # - variable: string con el nombre de la variable de interés.
  # - prueba: data frame con los resultados de la comparación.
  # Salida: objeto con el gráfico generado.

  # Construir data frame de valores p.
  n <- nlevels(df$cluster)
  columnaVariable <- if("Variable" %in% names(prueba)) "Variable" else "Curso"
  dfP <- NULL
  
  if(n == 2){
    if(prueba$p < ALFA) {
      dfP <- data.frame(group1 = "1", group2 = "2", p = prueba$p)
    }
  } else {
    omnibus <- prueba[1, "p"]
    
    if(omnibus < ALFA) {
      dfP <- prueba %>%
        dplyr::filter(grepl("^PH:", Prueba)) %>%
        dplyr::select(all_of(columnaVariable), Prueba, p) %>%
        dplyr::filter(p < ALFA) %>%
        dplyr::mutate(
          group1 = stringr::str_extract(Prueba, "(?<=PH: )\\d+"),
          group2 = stringr::str_extract(Prueba, "(?<=-)\\d+")
        ) %>%
        dplyr::select(group1, group2, p)
      
      dfP <- dfP %>% select(-any_of(c("Variable", "Ind", "Curso")))
    }
  }
  
  # Establecer límite visible para las etiquetas del eje Y.
  limiteY <- if(startsWith(variable, "A_") || startsWith(variable, "O_")) {
    10
  } else if(!startsWith(variable, "P_")) {
    7
  } else {
    max(df[[variable]])
  }
  
  # Definir formato para las etiquetas del eje Y.
  formatoY <- if(startsWith(variable, "P_")) {
    function(x) as.integer(x)
  } else {
    function(x) sapply(x, function(v) formatearFlotante(v, 1))
  }
  
  # Generar el gráfico.
  g <- ggboxplot(
    df, x = "cluster", y = variable, fill = "cluster", xlab = "Grupo",
    ylab = variable)
  
  g <- g + theme(legend.position = "none")
  
  # Añadir etiquetas con valores p si hay diferencia significativa.
  maxY <- max(df[[variable]], na.rm = TRUE)
  maxYAnotaciones <- maxY
  
  if(!is.null(dfP) && nrow(dfP) > 0) {
    salto <- maxY * 0.1
    dfP$y.position <- maxY + (1:nrow(dfP)) * salto
    dfP$p <- paste0("p = ", formatearFlotante(dfP$p))
    maxYAnotaciones <- max(dfP$y.position, na.rm = TRUE)
    
    g <- g + stat_pvalue_manual(
      dfP, label = "p", tip.length = 0.02, y.position = "y.position",
      inherit.aes = FALSE)
  }
  
  # Calcular margen dinámico.
  margenSuperior <- (maxYAnotaciones / limiteY) - 1
  if(margenSuperior < 0.1) margenSuperior <- 0.1
  
  # Dar formato a las etiquetas del eje Y.
  g <- g + scale_y_continuous(
      labels = formatoY, breaks = function(lims) {
        paso <- pretty(c(0, limiteY))
        paso[paso <= limiteY]
      }, expand = expansion(mult = c(0, margenSuperior))) +
    coord_cartesian(clip = "off")
  
  return(g)
}

ordenarComparaciones <- function(comparacion, resumen, columnas = "Variable") {
  # Reordena un data frame de comparaciones según el orden de las variables
  # en el resumen, manteniendo las filas post-hoc (si las hay) inmediatamente
  # después de su fila ómnibus.
  # Entrada:
  # - comparacion: dataframe con los resultados de las pruebas estadísticas.
  # - resumen: dataframe con las columnas de referencia ordenada.
  # - columnas: nombre de las columnas de referencia.
  # Salida: dataframe comparacion ordenado.
  
  ordenado <- data.frame()
  
  for(i in seq_len(nrow(resumen))) {
    valores <- resumen[i, columnas, drop = FALSE]
    
    mascara <- rep(TRUE, nrow(comparacion))
    for(columna in columnas) {
      mascara <- mascara &
        (as.character(comparacion[[columna]]) ==
           as.character(valores[[columna]]))
    }
    
    filas <- comparacion[mascara, , drop = FALSE]
    if(nrow(filas) > 0) ordenado <- rbind(ordenado, filas)
  }
  
  return(ordenado)
}



################################################################################
# Funciones para reportar clusters.
################################################################################

graficarClusters <- function(subconjunto, df, clusters) {
  # Grafica los k clusters para un subconjunto y las siluetas de las
  # observaciones.
  # Entrada:
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - df: conjunto de datos con las variables y observaciones de interés.
  # - clusters: objeto con el agrupamiento.
  # Salida: archivo .pdf con el gráfico.
  
  # Graficar clusters.
  df$cluster <- NULL
  titulo <- "Grupos de estudiantes"
  
  gCluster <- factoextra::fviz_cluster(
    grupos, data = df, geom = "point", main = titulo,
    xlab = FALSE, ylab = FALSE, ggtheme = theme_pubr()) +
    ggplot2::theme(legend.position = "top")  +
    scale_x_continuous(
      labels = function(x) sapply(x, function(v) formatearFlotante(v, 1)))
  
  # Graficar siluetas.
  siluetas <- cluster::silhouette(grupos$cluster, dist(df))
  media <- round(mean(siluetas[, 3]), 3)
  titulo <- "Siluetas de las observaciones"
  
  capture.output({
    gSilueta <- factoextra::fviz_silhouette(siluetas) +
      ggplot2::labs(
        title = titulo,
        subtitle = paste("Silueta media =", formatearFlotante(media)),
        x = "Clusters", y = "Ancho de silueta Si") +
      ggpubr::theme_pubr() +
      ggplot2::theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "top") +
      scale_y_continuous(
        labels = function(x) sapply(x, function(v) formatearFlotante(v, 1)))
  })
  
  # Combinar gráficos.
  titulo <- "Grupos de estudiantes según competencias informacionales"
  
  if(subconjunto == "CIA") {
    titulo <- paste(titulo, "autopercibidas")
  } else if(subconjunto == "CIO") {
    titulo <- paste(titulo, "observadas")
  } else {
    titulo <- paste(titulo, "autopercibidas y observadas")
  }
  
  graficos <- list(gCluster, gSilueta)
  archivo <- paste0(tolower(subconjunto), "_clusters.pdf")
  archivo <- paste(RUTA_RESULTADOS, subconjunto, archivo, sep = "/")
  combinarGraficos(graficos, titulo, archivo)
}

reportarCentroides <- function(subconjunto, df, clusters) {
  # Genera un archivo .csv con los centroides de los clusters.
  # Entrada:
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - df: conjunto de datos con las variables y observaciones de interés.
  # - clusters: objeto con el agrupamiento.
  # Salida: archivo .csv.
  
  # Obtener los centroides.
  centroides <- as.data.frame(round(grupos$centers, 3))
  
  # Obtener cantidad de grupos.
  n <- nlevels(df$cluster)
  
  # Agregar fila con el tamaño de los grupos.
  resultado <- data.frame(Subconjunto = "", Variable = "Frecuencia")
  
  for(grupo in 1:n) {
    nGrupo <- sum(df$cluster == grupo)
    nombre <- paste0("Grupo_", grupo)
    resultado[[nombre]] <- nGrupo
  }
  
  # Obtener nombres de las variables.
  nombres <- colnames(df %>% select(ends_with("_N")))
  
  # Agregar fila por variable.
  for(variable in nombres) {
    actual <- data.frame(
      Subconjunto = ifelse(startsWith(variable, "A_"), "CIA", "CIO"),
      Variable = variable)
    
    for(grupo in 1:n) {
      nombre <- paste0("Grupo_", grupo)
      actual[[nombre]] <- centroides[grupo, variable]
    }
    
    resultado <- rbind(resultado, actual)
  }
  
  # Guardar data frame con el reporte.
  archivo <- paste(RUTA_RESULTADOS, subconjunto, "Centroides.csv", sep = "/")
  crearRuta(archivo)
  write.csv2(resultado, archivo, row.names = FALSE)
}

reportarClusters <- function(subconjunto, df, clusters) {
  # Genera la tabla con el reporte de los centroides obtenidos a partir de un
  # subconjunto dado y genera los gráficos de los clusters y las siluetas.
  # Entrada:
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - df: dataframe normalizado con los clusters añadidos.
  # - clusters: objeto con el agrupamiento.
  # Salida:
  # - Archivo .csv con el reporte de los clusters.
  # - Archivo .pdf con el gráfico de los clusters.
  # - Archivo .pdf con el gráfico de las siluetas.
  
  # Filtrar datos de interés.
  filtrado <- filtrarDatos(df, subconjunto, "TODOS", "CI", normal = TRUE)
  
  # Generar tabla con centroides.
  reportarCentroides(subconjunto, filtrado, clusters)
  
  # Generar gráficos de clusters y siluetas.
  graficarClusters(subconjunto, filtrado, clusters)
}



################################################################################
# Funciones para caracterizar según competencias informacionales.
################################################################################

caracterizarCompetencias <- function(subconjunto, df) {
  # Crea las tablas de resumen de los clusters y de comparación entre clusters
  # según sus competencias informacionales.
  # Entrada:
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - df: dataframe normalizado con los clusters añadidos.
  # Salida:
  # - Archivo .csv con el resumen de los clusters.
  # - Archivo .csv con los resultados de las pruebas estadísticas.
  # - Archivo .pdf con los gráficos de variabilidad.
  
  # Hacer caracterización.
  for(sexo in SEXOS) {
    # Inicializar dataframes de resultados.
    resumen <- data.frame()
    comparacion <- data.frame()
    
    # Filtrar columnas y observaciones de interés.
    filtrado <- filtrarDatos(df, subconjunto, sexo, "CI", cinf = TRUE)
    
    # Determinar los grupos.
    grupos <- levels(filtrado$cluster)
    
    # Obtener nombres de las variables.
    variables <- colnames(filtrado %>% select(-cluster))
    
    # Inicializar lista de gráficos.
    graficos <- list()
    
    # Analizar variables.
    for(variable in variables) {
      # Obtener estadísticas de resumen.
      resumenActual <- resumirCompetencia(sexo, filtrado, variable)

      # Hacer comparaciones.
      prueba <- compararCompetencia(sexo, filtrado, variable)

      # Agregar resultados a los data frames correspondientes.
      resumen <- rbind(resumen, resumenActual)
      comparacion <- rbind(comparacion, prueba)
      
      # Generar el gráfico de variabilidad.
      graficos[[variable]] <- graficarNumerica(filtrado, variable, prueba)
    }
    
    # Generar gráfico combinado.
    titulo <- generarTituloCompetencias(sexo, subconjunto)
    archivo <- generarNombreArchivo(sexo, subconjunto, "cinf")
    combinarGraficos(graficos, titulo, archivo)
    
    # Guardar archivo .csv con las comparaciones.
    comparacion <- formatearReporte(comparacion)
    comparacion <- ordenarComparaciones(comparacion, resumen)
    archivo <- paste0("Comparaciones clusters ", tolower(sexo), ".csv")
    archivo <- paste(RUTA_RESULTADOS, subconjunto, archivo, sep = "/")
    guardarDataframe(comparacion, archivo)
    
    # Añadir columna con diferencias al resumen y guardarlo en archivo .csv.
    resumen$Dif <- sapply(
      split(comparacion, comparacion$Variable), esSignificativa)
    
    archivo <- paste0("Caracterizacion clusters ", tolower(sexo), ".csv")
    archivo <- paste(RUTA_RESULTADOS, subconjunto, archivo, sep = "/")
    guardarDataframe(resumen, archivo)
  }
}

compararCompetencia <- function(sexo, df, variable) {
  # Compara las medias entre grupos para una variable de competencias
  # informacionales.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - df: conjunto de datos con las variables y observaciones de interés.
  # - variable: string con el nombre de la variable de interés.
  # Salida: data frame con los resultados de la comparación.
  
  # Determinar cantidad de grupos.
  n <- nlevels(df$cluster)
  
  # Generar data frame para comparaciones.
  dfPrueba <- data.frame(cluster = df$cluster, variable = df[[variable]])
  
  # Aplicar la prueba que corresponda.
  prueba <- NULL
  
  if(n > 2) {
    prueba <- kruskal(dfPrueba$variable, dfPrueba$cluster)
    prueba$Variable <- variable
    prueba <- prueba %>% relocate(Variable, .before = Prueba)
  } else {
    filtrado = dfPrueba %>% filter(cluster == 1)
    grupo1 <- filtrado$variable
    
    filtrado = dfPrueba %>% filter(cluster == 2)
    grupo2 <- filtrado$variable
    
    if(startsWith(variable, "A_")) {
      prueba <- wilcoxIndep(grupo1, grupo2)
      prueba <- data.frame(Variable = variable, prueba)
    } else {
      prueba <- tTestIndep(grupo1, grupo2)
      prueba <- data.frame(Variable = variable, prueba)
    }
  }
  
  # Agregar columnas adicionales.
  prueba$Subconjunto <- subconjunto
  prueba <- prueba %>% relocate(Subconjunto, .before = Prueba)
  return(prueba)
}

generarTituloCompetencias <- function(sexo, subconjunto) {
  # Genera el título para el gráfico de variabilidad por grupo de competencias
  # informacionales.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # Salida: string con el título del gráfico.
  
  titulo <- "Variabilidad por grupos de competencias informacionales"
  
  if(sexo == "MASCULINO") {
    titulo <- paste(titulo, "(solo hombres)")
  } else if(sexo == "FEMENINO") {
    titulo <- paste(titulo, "(solo mujeres)")
  }
  
  titulo <- paste0(titulo, "\n(Grupos según competencias informacionales")
  
  if(subconjunto == "CIA") {
    titulo <- paste(titulo, "autopercibidas)")
  } else if(subconjunto == "CIO") {
    titulo <- paste(titulo, "observadas)")
  } else {
    titulo <- paste(titulo, "autopercibidas y observadas)")
  }
  
  return(titulo)
}

resumirCompetencia <- function(sexo, df, variable) {
  # Genera el resumen por grupo (media y desviación estándar) para una variable
  # de competencias informacionales.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - df: conjunto de datos con las variables y observaciones de interés.
  # - variable: string con el nombre de la variable de interés.
  # Salida: dataframe con el resumen de la variable.
  
  # Determinar grupos.
  grupos <- levels(df$cluster)
  
  # Inicializar resultado.
  resultado <- data.frame(
    Subconjunto = ifelse(startsWith(variable, "A_"), "CInfA", "CInfO"),
    Variable = variable)
  
  # Agregar medidas de tendencia central por grupo.
  for(grupo in grupos) {
    media <- mean(df[df$cluster == grupo, variable])
    media <- formatearFlotante(media)
    desv <- sd(df[df$cluster == grupo, variable])
    desv <- formatearFlotante(desv)
    nombre <- paste0("Grupo_", grupo)
    resultado[[nombre]] <- paste0(media, " (", desv, ")")
  }
  
  return(resultado)
}



################################################################################
# Funciones para caracterizar según variables de ingreso.
################################################################################

caracterizarIngreso <- function(subconjunto, df) {
  # Crea las tablas de resumen de los clusters y de comparación entre clusters
  # según sus variables de ingreso.
  # Entrada:
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - df: dataframe normalizado con los clusters añadidos.
  # Salida:
  # - Archivo .csv con el resumen de los clusters.
  # - Archivo .csv con los resultados de las pruebas estadísticas.
  # - Archivos .pdf con los gráficos de variabilidad o proporciones.
  
  # Hacer caracterización.
  for(sexo in SEXOS) {
    # Inicializar data frames de resultados.
    resumen <- data.frame()
    comparacion <- data.frame()
    
    # Filtrar columnas y observaciones de interés.
    filtrado <- filtrarDatos(df, subconjunto, sexo, "INGRESO")
    
    # Determinar los grupos.
    grupos <- levels(filtrado$cluster)

    # Obtener nombres de las variables.
    variables <- colnames(filtrado %>% select(-cluster))
    
    # Inicializar listas de gráficos.
    graficosCategoricas <- list()
    graficosNumericas <- list()
    
    # Analizar variables.
    for(variable in variables) {
      # Obtener estadísticas de resumen.
      resumenActual <- resumirIngreso(sexo, filtrado, variable)
      
      # Hacer comparaciones.
      prueba <- compararIngreso(sexo, filtrado, variable)
      
      # Agregar resultados a los data frames correspondientes.
      resumen <- rbind(resumen, resumenActual)
      comparacion <- rbind(comparacion, prueba)
      
      # Generar el gráfico de variabilidad o proporciones.
      if(is.factor(filtrado[[variable]])) {
        g <- graficarCategorica(filtrado, variable, prueba)
        graficosCategoricas[[variable]] <- g
      } else {
        g <- graficarNumerica(filtrado, variable, prueba)
        graficosNumericas[[variable]] <- g
      }
    }
    
    # Generar gráficos combinados.
    titulo <- generarTituloIngreso(sexo, subconjunto, TRUE)
    archivo <- generarNombreArchivo(sexo, subconjunto, "ingreso")
    combinarGraficos(graficosCategoricas, titulo, archivo)

    titulo <- generarTituloIngreso(sexo, subconjunto, FALSE)
    archivo <- generarNombreArchivo(sexo, subconjunto, "pdt")
    combinarGraficos(graficosNumericas, titulo, archivo)
    
    # Guardar archivo .csv con las comparaciones.
    comparacion <- formatearReporte(comparacion)
    comparacion <- ordenarComparaciones(comparacion, resumen)
    archivo <- paste0("Comparaciones ingreso ", tolower(sexo), ".csv")
    archivo <- paste(RUTA_RESULTADOS, subconjunto, archivo, sep = "/")
    guardarDataframe(comparacion, archivo)
    
    # Guardar archivo .csv con el resumen.
    significancia <- sapply(
      split(comparacion, comparacion$Variable), esSignificativa)
    
    resumen$Dif <- significancia[resumen$Variable]
    archivo <- paste0("Caracterizacion ingreso ", tolower(sexo), ".csv")
    archivo <- paste(RUTA_RESULTADOS, subconjunto, archivo, sep = "/")
    guardarDataframe(resumen, archivo)
  }
}

compararIngreso <- function(sexo, df, variable) {
  # Compara las medias entre grupos para una variable de ingreso.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - df: conjunto de datos con las variables y observaciones de interés.
  # - variable: string con el nombre de la variable de interés.
  # Salida: data frame con los resultados de la comparación.
  
  # Determinar cantidad de grupos.
  n <- nlevels(df$cluster)
  
  # Generar data frame para comparaciones.
  dfPrueba <- data.frame(cluster = df$cluster, variable = df[[variable]])
  prueba <- NULL
  
  # Aplicar la prueba que corresponda.
  if(is.numeric(df[[variable]])) {
    if(n == 2) {
      filtrado = dfPrueba %>% filter(cluster == 1)
      grupo1 <- filtrado$variable
      
      filtrado = dfPrueba %>% filter(cluster == 2)
      grupo2 <- filtrado$variable
      
      prueba <- tTestIndep(grupo1, grupo2)
      prueba <- data.frame(Variable = variable, prueba)
    } else {
      prueba <- kruskal(dfPrueba$variable, dfPrueba$cluster)
      prueba$Variable <- variable
      prueba <- prueba %>% relocate(Variable, .before = Prueba)
    }
  } else {
    if(n == 2 & nlevels(dfPrueba$variable) == 2) {
      prueba <- fisher(dfPrueba$cluster, dfPrueba$variable)
      prueba <- data.frame(Variable = variable, prueba)
    } else {
      prueba <- chiCuadrado(dfPrueba$cluster, dfPrueba$variable)
      prueba <- data.frame(Variable = variable, prueba)
    }
  }
  
  return(prueba)
}

generarTituloIngreso <- function(sexo, subconjunto, categoricas) {
  # Genera el título para el gráfico de variabilidad o proporciones para
  # variables de ingreso.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - categoricas: booleano que indica si se trata de variables numéricas o 
  #   categóricas.
  # Salida: string con el título del gráfico.
  
  titulo <- ""
  
  if(categoricas) {
    titulo <- "Proporciones por grupo para variables categóricas de ingreso"
  } else {
    titulo <- "Variabilidad por grupos de puntajes PDT"
  }
  
  if(sexo == "MASCULINO") {
    titulo <- paste(titulo, "(solo hombres)")
  } else if(sexo == "FEMENINO") {
    titulo <- paste(titulo, "(solo mujeres)")
  }
  
  titulo <- paste0(titulo, "\n(Grupos según competencias informacionales")
  
  if(subconjunto == "CIA") {
    titulo <- paste(titulo, "autopercibidas)")
  } else if(subconjunto == "CIO") {
    titulo <- paste(titulo, "observadas)")
  } else {
    titulo <- paste(titulo, "autopercibidas y observadas)")
  }
  
  return(titulo)
}

resumirIngreso <- function(sexo, df, variable) {
  # Genera el resumen por grupo (media y desviación estándar si la variable es
  # numérica, frecuencia y porcentaje en caso contrario) para una variable de
  # ingreso.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - df: conjunto de datos con las variables y observaciones de interés.
  # - variable: string con el nombre de la variable de interés.
  # Salida: data frame con el resumen de la variable.
  
  # Determinar grupos.
  grupos <- levels(df$cluster)
  
  # Inicializar resultado.
  resultado <- NULL
  
  if(is.numeric(df[[variable]])) {
    # Agregar columnas de descripción.
    resultado <- data.frame(
      Sexo = sexo, Tipo = "Numéricas", Variable = variable, Nivel = "")
    
    # Agregar medidas de tendencia central.
    for(grupo in grupos) {
      media <- mean(df[df$cluster == grupo, variable])
      media <- formatearFlotante(media)
      desv <- sd(df[df$cluster == grupo, variable])
      desv <- formatearFlotante(desv)
      nombre <- paste0("Grupo_", grupo)
      resultado[[nombre]] <- paste0(media, " (", desv, ")")
    }
  } else {
    niveles <- levels(df[[variable]])
    resultado <- NULL
    
    for(nivel in niveles) {
      actual <- data.frame(
        Sexo = sexo, Tipo = "Categóricas", Variable = variable, Nivel = nivel)
      
      for(grupo in grupos) {
        frecuencia <- sum(df[[variable]] == nivel & df$cluster == grupo)
        porcentaje <- (frecuencia / sum(df$cluster == grupo)) * 100
        porcentaje <- formatearFlotante(porcentaje)
        nombre <- paste0("Grupo_", grupo)
        actual[[nombre]] <- paste0(frecuencia, " (", porcentaje, "%)")
      }
      
      resultado <- rbind(resultado, actual)
    }
  }
  
  if(sexo != "TODOS") {
    resultado$Sexo <- NULL
  }
  
  return(resultado)
}



################################################################################
# Funciones para caracterizar según variables de rendimiento.
################################################################################

caracterizarRendimiento <- function(subconjunto, df) {
  # Crea las tablas de resumen de los clusters y de comparación entre clusters
  # según sus variables de rendimiento.
  # Entrada:
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - df: dataframe normalizado con los clusters añadidos.
  # Salida:
  # - Archivo .csv con el resumen de los clusters.
  # - Archivo .csv con los resultados de las pruebas estadísticas.
  # - Archivo .pdf con los gráficos de variabilidad.
  
  # Hacer caracterización.
  for(sexo in SEXOS) {
    # Inicializar data frames de resultados parciales.
    resumenSit <- data.frame()
    resumenNota <- data.frame()
    comparacionSit <- data.frame()
    comparacionNota <- data.frame()
    
    # Filtrar columnas y observaciones de interés.
    filtrado <- filtrarDatos(df, subconjunto, sexo, "RENDIMIENTO")
    
    # Inicializar listas de gráficos.
    graficosCategoricas <- list()
    graficosNumericas <- list()
    
    for(curso in CURSOS) {
      # Filtrar datos del curso.
      variableSit <- paste0("SIT_", curso)
      variableNota <- paste0("NOTA_", curso)
      nombreCurso <- NOMBRES_CURSOS[[curso]]
      
      dfCurso <- data.frame(
        cluster = filtrado$cluster, Sit = filtrado[[variableSit]],
        Nota = filtrado[[variableNota]])
      
      # Obtener estadísticas de resumen.
      resumenSitActual <- resumirSituacion(sexo, dfCurso, nombreCurso)
      resumenNotaActual <- resumirNota(sexo, dfCurso, nombreCurso)
      
      # Hacer comparaciones.
      pruebaSit <- compararSituacion(sexo, dfCurso, nombreCurso)
      pruebaNota <- compararNota(sexo, dfCurso, nombreCurso)
      
      # Agregar resultados a los data frames correspondientes.
      resumenSit <- rbind(resumenSit, resumenSitActual)
      comparacionSit <- rbind(comparacionSit, pruebaSit)
      resumenNota <- rbind(resumenNota, resumenNotaActual)
      comparacionNota <- rbind(comparacionNota, pruebaNota)
      
      # Generar gráficos de variabilidad y proporciones.
      g <- graficarCategorica(filtrado, variableSit, pruebaSit)
      graficosCategoricas[[variableSit]] <- g
      g <- graficarNumerica(filtrado, variableNota, pruebaNota)
      graficosNumericas[[variableNota]] <- g
    }
    
    # Agregar resultados parciales a los resultados generales.
    resumen <- rbind(resumenSit, resumenNota)
    comparacion <- rbind(comparacionSit, comparacionNota)
    
    # Generar gráficos combinados.
    titulo <- generarTituloRendimiento(sexo, subconjunto, TRUE)
    archivo <- generarNombreArchivo(sexo, subconjunto, "sit")
    combinarGraficos(graficosCategoricas, titulo, archivo)
    
    titulo <- generarTituloRendimiento(sexo, subconjunto, FALSE)
    archivo <- generarNombreArchivo(sexo, subconjunto, "nota")
    combinarGraficos(graficosNumericas, titulo, archivo)
    
    # Guardar archivo .csv con las comparaciones.
    comparacion <- formatearReporte(comparacion)
    comparacion <- ordenarComparaciones(comparacion, resumen, c("Ind", "Curso"))
    archivo <- paste0("Comparaciones rendimiento ", tolower(sexo), ".csv")
    archivo <- paste(RUTA_RESULTADOS, subconjunto, archivo, sep = "/")
    guardarDataframe(comparacion, archivo)
    
    # Guardar archivo .csv con el resumen.
    Dif <- comparacion %>% group_by(Curso, Ind) %>%
      summarise(p = first(p), .groups = "drop") %>%
      mutate(Dif = ifelse(grepl("\\*", p), "Sí", "No")) %>%
      select(Curso, Ind, Dif)
    
    resumen <- resumen %>% left_join(Dif, by = c("Curso", "Ind"))
    archivo <- paste0("Caracterizacion rendimiento ", tolower(sexo), ".csv")
    archivo <- paste(RUTA_RESULTADOS, subconjunto, archivo, sep = "/")
    guardarDataframe(resumen, archivo)
  }
}

compararNota <- function(sexo, df, nombreCurso) {
  # Compara las medias entre grupos para la nota final del curso.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - df: conjunto de datos con las columnas cluster, Sit y Nota.
  # - nombreCurso: string con el nombre del curso.
  # Salida: data frame con el resumen de la variable.
  
  # Determinar cantidad de grupos.
  n <- nlevels(df$cluster)
  
  # Generar data frame para comparaciones.
  dfPrueba <- data.frame(cluster = df$cluster, variable = df$Nota)
  prueba <- NULL
  
  # Aplicar la prueba que corresponda.
  if(n == 2) {
    filtrado = dfPrueba %>% filter(cluster == 1)
    grupo1 <- filtrado$variable
    
    filtrado = dfPrueba %>% filter(cluster == 2)
    grupo2 <- filtrado$variable
    
    prueba <- tTestIndep(grupo1, grupo2)
  } else {
    prueba <- kruskal(dfPrueba$variable, dfPrueba$cluster)
  }
  
  # Agregar columnas adicionales.
  prueba$Ind <- "Nota"
  prueba$Curso <- nombreCurso
  prueba <- prueba %>% relocate(Ind, .before = Prueba)
  prueba <- prueba %>% relocate(Curso, .before = Prueba)
  return(prueba)
}

compararSituacion <- function(sexo, df, nombreCurso) {
  # Compara las proporciones entre grupos para la situación final del curso.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - df: conjunto de datos con las columnas cluster, Sit y Nota.
  # - nombreCurso: string con el nombre del curso.
  # Salida: data frame con el resumen de la variable.
  
  # Generar data frame para comparaciones.
  dfPrueba <- data.frame(cluster = df$cluster, variable = df$Sit)
  prueba <- NULL
  
  # Aplicar la prueba que corresponda.
  if(nlevels(dfPrueba$cluster) == 2) {
    prueba <- fisher(dfPrueba$cluster, dfPrueba$variable)
  } else {
    prueba <- chiCuadrado(dfPrueba$cluster, dfPrueba$variable)
  }
  
  # Agregar columnas adicionales.
  prueba <- cbind(Ind = "Sit.", Curso = nombreCurso, prueba)
  return(prueba)
}

generarTituloRendimiento <- function(sexo, subconjunto, categoricas) {
  # Genera el título para el gráfico de variabilidad o proporciones para
  # variables de rendimiento académico.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - subconjunto: subconjunto de variables consideradas para el agrupamiento.
  # - categoricas: booleano que indica si se trata de variables numéricas o 
  #   categóricas.
  # Salida: string con el título del gráfico.
  
  titulo <- ""
  
  if(categoricas) {
    titulo <- "Proporciones por grupo aprobación (A) y reprobación (R)"
  } else {
    titulo <- "Variabilidad por grupos de calificación final"
  }
  
  if(sexo == "MASCULINO") {
    titulo <- paste(titulo, "(solo hombres)")
  } else if(sexo == "FEMENINO") {
    titulo <- paste(titulo, "(solo mujeres)")
  }
  
  titulo <- paste0(titulo, "\n(Grupos según competencias informacionales")
  
  if(subconjunto == "CIA") {
    titulo <- paste(titulo, "autopercibidas)")
  } else if(subconjunto == "CIO") {
    titulo <- paste(titulo, "observadas)")
  } else {
    titulo <- paste(titulo, "autopercibidas y observadas)")
  }
  
  return(titulo)
}

resumirNota <- function(sexo, df, nombreCurso) {
  # Genera el resumen por grupo (media y desviación estándar) para la nota final
  # del curso.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - df: conjunto de datos con las columnas cluster, Sit y Nota.
  # - nombreCurso: string con el nombre del curso.
  # Salida: data frame con el resumen de la variable.
  
  # Determinar grupos.
  grupos <- levels(df$cluster)
  
  # Separar aprobados y reprobados.
  aprobados <- df %>% filter(Sit == "A")
  reprobados <- df %>% filter(Sit == "R")
  
  # Inicializar resultado.
  resultado <- data.frame(Curso = nombreCurso, Ind = "Nota")
  
  # Agregar medidas de tendencia central.
  for(grupo in grupos) {
    media <- mean(df[df$cluster == grupo, "Nota"])
    media <- formatearFlotante(media)
    
    mediaA <- mean(aprobados[aprobados$cluster == grupo, "Nota"])
    mediaA <- formatearFlotante(mediaA)
    
    mediaR <- mean(reprobados[reprobados$cluster == grupo, "Nota"])
    mediaR <- formatearFlotante(mediaR)
    
    desv <- sd(df[df$cluster == grupo, "Nota"])
    desv <- formatearFlotante(desv)
    
    desvA <- sd(aprobados[aprobados$cluster == grupo, "Nota"])
    desvA <- formatearFlotante(desvA)
    
    desvR <- sd(reprobados[reprobados$cluster == grupo, "Nota"])
    desvR <- formatearFlotante(desvR)
    
    nombreGrupo <- paste0("G", grupo, "_")
    columnaTodos <- paste0(nombreGrupo, "Todos")
    columnaA <- paste0(nombreGrupo, "A")
    columnaR <- paste0(nombreGrupo, "R")
    
    resultado[[columnaTodos]] <- paste0(media, " (", desv, ")")
    resultado[[columnaA]] <- paste0(mediaA, " (", desvA, ")")
    resultado[[columnaR]] <- paste0(mediaR, " (", desvR, ")")
  }
  
  return(resultado)
}

resumirSituacion <- function(sexo, df, nombreCurso) {
  # Genera el resumen por grupo (frecuencia y porcentaje en caso contrario) para
  # la situación final del curso.
  # Entrada:
  # - sexo: string con el sexo a considerar.
  # - df: conjunto de datos con las columnas cluster, Sit y Nota.
  # - nombreCurso: string con el nombre del curso.
  # Salida: data frame con el resumen de la variable.
  
  # Determinar grupos.
  grupos <- levels(df$cluster)
  
  # Separar aprobados y reprobados.
  aprobados <- df %>% filter(Sit == "A")
  reprobados <- df %>% filter(Sit == "R")
  
  # Inicializar resultado.
  resultado <- data.frame(Curso = nombreCurso, Ind = "Sit.")
  
  # Agregar medidas de tendencia central.
  for(grupo in grupos) {
    frecuencia <- nrow(df %>% filter(cluster == grupo))
    frecuenciaA <- nrow(aprobados %>% filter(cluster == grupo))
    frecuenciaR <- nrow(reprobados %>% filter(cluster == grupo))
    
    porcentaje <- "100,000"
    porcentajeA <- (frecuenciaA /frecuencia) * 100
    porcentajeA <- formatearFlotante(porcentajeA)
    
    porcentajeR <- (frecuenciaR /frecuencia) * 100
    porcentajeR <- formatearFlotante(porcentajeR)
    
    nombreGrupo <- paste0("G", grupo, "_")
    columnaTodos <- paste0(nombreGrupo, "Todos")
    columnaA <- paste0(nombreGrupo, "A")
    columnaR <- paste0(nombreGrupo, "R")
    
    resultado[[columnaTodos]] <- paste0(frecuencia, " (", porcentaje, "%)")
    resultado[[columnaA]] <- paste0(frecuenciaA, " (", porcentajeA, "%)")
    resultado[[columnaR]] <- paste0(frecuenciaR, " (", porcentajeR, "%)")
  }
  
  return(resultado)
}



################################################################################
# Main
################################################################################

cluster <- activarParalelismo()
datos <- normalizarCInf()

for(subconjunto in SUBCONJUNTOS) {
  # Obtener los grupos.
  filtrado <- filtrarCompetenciasNormalizadas(datos, subconjunto)
  grupos <- evaluarClusters(filtrado, subconjunto)

  # Agregar grupos al dataframe.
  nombre <- paste0("clusters_", subconjunto)
  datos[[nombre]] <- factor(grupos$cluster)

  # Obtener perfiles de los grupos.
  reportarClusters(subconjunto, datos, grupos)
  caracterizarCompetencias(subconjunto, datos)
  caracterizarIngreso(subconjunto, datos)
  caracterizarRendimiento(subconjunto, datos)
}

# Guadar dataframe con clusters.
crearRuta(DATASET_CLUSTERS)
write.csv2(datos, DATASET_CLUSTERS, row.names = FALSE)

desactivarParalelismo(cluster)
