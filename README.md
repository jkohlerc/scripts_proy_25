# Tesis-doctoral
Contiene los scripts en R utilizados para el desarrollo de la tesis, organizados en carpetas según las diferentes etapas:
- 0 Comun:
  - Utilidades.R: define funciones de apoyo utilizadas transversalmente.
  - Estadisticas.R: implementa pruebas estadísticas con remuestreo y define funciones de apoyo para el reporte de resultados de pruebas estadísticas en general.
- 1 Preparacion:
  - CrearDataset.R: construye el conjunto de datos bruto a partir de los archivos fuente.
- 2 Estadisticas:
  - Analisis.R: realiza el análisis estadístico preliminar y construye el conjunto de datos limpio.
- 3 Caracterizacion:
  - Clustering.R: realiza los agrupamientos según competencias informacionales y caracteriza los grupos identificados.
- 4 Prediccion:
  - Prediccion.R: script principal que gestiona el proceso.
  - Configuracion.R: define las constantes necesarias, incluyendo rangos de valores para ajustes de hiperparámetros.
  - FuncionesGenerales.R: define funciones de apoyo comunes a diferentes secciones del código.
  - SeleccionarCaracteristicas.R: define la función para realizar el proceso de selección de características.
  - AjustarParametros.R: define la función para ajustar hiperparámetros con las variantes correspondientes a cada tipo de modelo.
  - EscogerModelo.R: define la función que ajusta los diferentes modelos (regresión logística o lineal, random forest, SVM o SVR con kernels lineal y radial, extreme gradient boosting) para un subconjunto específico de datos, y selecciona el mejor modelo según la métrica correspondiente.
  - EntrenarModelo.R: define la función que ajusta un modelo específico según las características de cada uno.
  - CompararModelos.R: define la función que compara dos modelos.
