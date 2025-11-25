################################################################################
# Configuración
################################################################################

# Cargar funciones de apoyo.
source("Scripts/0 Comun/Utilidades.R")
source("Scripts/0 Comun/Estadisticas.R")

# Cargar paquetes.
importarPaquetes(
  c("Boruta", "car", "doParallel", "doRNG", "fastDummies", "ggpubr", "ggrepel",
    "hardhat", "irace", "kernlab", "patchwork", "pROC", "PRROC", "ranger",
    "tidymodels", "tidyverse", "xgboost", "zoo"))

# Cargar componentes.
source("Scripts/4 Prediccion/Configuracion.R")
source("Scripts/4 Prediccion/FuncionesGenerales.R")
source("Scripts/4 Prediccion/EntrenarModelo.R")
source("Scripts/4 Prediccion/AjustarParametros.R")
source("Scripts/4 Prediccion/SeleccionarCaracteristicas.R")
source("Scripts/4 Prediccion/EscogerModelo.R")
source("Scripts/4 Prediccion/CompararModelos.R")

################################################################################
# Main
################################################################################

# Activar paralelización.
if(!exists("cluster")) {
  cluster <- activarParalelismo()
}

# Cargar datos y separarlos por grupos de columnas (variables de ingreso,
# variables de competencias informacionales y respuestas).
datos <- cargarDatos()

for(curso in CURSOS) {
  cat(paste0(curso, ": clasificación: modelo base\n"))
  # Seleccionar características para el clasificador base (solo variables de
  # ingreso).
  clasificacion <- TRUE
  datosModelo <- inicializarDescriptor(datos, curso, clasificacion)
  cat("\tSelección de características\n")
  datosModelo <- seleccionarCaracteristicas(datosModelo)
  caracteristicasBase <- colnames(datosModelo$df %>% select(-respuesta))

  # Ajustar clasificador base.
  base <- escogerModelo(datosModelo)

  # Seleccionar características para el clasificador final (con competencias
  # informacionales).
  cat(paste0(curso, ": clasificación: modelo final\n"))

  datosModelo <- inicializarDescriptor(
    datos, curso, clasificacion, dfBase = datosModelo$df)

  cat("\tSelección de características\n")
  datosModelo <- seleccionarCaracteristicas(datosModelo)
  caracteristicasFinal <- colnames(datosModelo$df %>% select(-respuesta))

  # Si la selección es la misma que para el modelo base, terminar la evaluación
  # del curso actual.
  if(length(setdiff(caracteristicasFinal, caracteristicasBase)) == 0) next

  # Ajustar clasificador final.
  final <- escogerModelo(datosModelo)

  # Comparar modelos.
  datosModelo <- list(
    clasificacion = datosModelo$clasificacion,
    respuesta = datosModelo$respuesta, parsimonia = datosModelo$parsimonia)

  cumple <- compararModelos(base, final, datosModelo)

  # Si no hay diferencias significativas, terminar la evaluación del curso
  # actual.
  if(!cumple) next

  # Seleccionar características para el regresor base (solo variables de
  # ingreso).
  cat(paste0(curso, ": regresión: modelo base\n"))
  clasificacion <- FALSE
  datosModelo <- inicializarDescriptor(datos, curso, clasificacion)
  cat("\tSelección de características\n")
  datosModelo <- seleccionarCaracteristicas(datosModelo)
  caracteristicasBase <- colnames(datosModelo$df %>% select(-respuesta))

  # Ajustar regresor base.
  base <- escogerModelo(datosModelo)

  # Seleccionar características para el regresor final (con competencias
  # informacionales).
  cat(paste0(curso, ": regresión: modelo final\n"))
  datosModelo <- inicializarDescriptor(
    datos, curso, clasificacion, dfBase = datosModelo$df)

  cat("\tSelección de características\n")
  datosModelo <- seleccionarCaracteristicas(datosModelo)
  caracteristicasFinal <- colnames(datosModelo$df %>% select(-respuesta))

  # Si la selección es la misma que para el modelo base, terminar la evaluación
  # del curso actual.
  if(length(setdiff(caracteristicasFinal, caracteristicasBase)) == 0) next

  # Ajustar regresor final.
  final <- escogerModelo(datosModelo)

  # Comparar modelos.
  datosModelo <- list(
    clasificacion = datosModelo$clasificacion,
    respuesta = datosModelo$respuesta, parsimonia = datosModelo$parsimonia)

  compararModelos(base, final, datosModelo)
}

# Desactivar paralelización.
if(exists("cluster") && !is.null(cluster)) {
  desactivarParalelismo(cluster)
}
