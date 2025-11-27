################################################################################
# Constantes
################################################################################

# Rutas de archivos.
RUTA_RESULTADOS <- "Resultados/3 Prediccion"
DATASET_LIMPIO <- "Datos/3 Limpio/Dataset limpio.csv"
DATASET_APLANADO <- "Datos/5 Prediccion/Dataset aplanado.csv"

# Configuración de paralelización.
NUCLEOS <-  detectCores() - 2

# Metricas de evaluación.
NOMBRES_METRICAS <- c(
    RMSE = "rmse", R2 = "rsq", MCC = "mcc", Sens = "sens", Espec = "spec",
    AUC_ROC = "roc_auc", AUC_PR = "pr_auc")

# Límite de inflación de varianza para regresión lineal y logística.
MAX_VIF <- 5

# Validación cruzada.
CV <- list(folds = 5, repeticiones = 10)

# Métricas principales.
METRICA_CLAS <- "MCC"
METRICA_REG <- "RMSE"

RANGO_METRICAS <- list(
  minClas = if(METRICA_CLAS == "MCC") -1 else 0, maxClas = 1,
  minReg = 0, maxReg = 6)

# Random forest.
NTREE <- list(
  maximo = 2500L, salto = 10L, sinMejora = 10L, tolerancia = 1e-2, ventana = 10)

# Límites de hiperparámetros.
LIMITES_PARAMETROS <- list(
  SVM_L = list(cost = c(2^-5, 2^15), epsilon = c(10^-4, 10^0)),
  SVM_R = list(
    cost = c(2^-5,  2^15), sigma = c(2^-15, 2^3), epsilon = c(10^-4, 10^0)),
  XGB = list(
    eta = c(0.01, 0.3), max_depth = c(2L,  6L), min_child_weight = c(1L,  10L),
    gamma = 0, subsample = 0.8, colsample_bytree = 0.8))

# Irace.
LOG_IRACE <- "iraceLog.rda"

# Tipos de modelos.
TIPOS_MODELO <- c("RL", "RF", "SVM_L", "SVM_R", "XGB")

# Nombres para variables de respuesta.
CURSOS <- c("ALG", "CAL", "FIS", "PPA")
