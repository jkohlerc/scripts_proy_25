# Tesis-doctoral
Repositorio con los scripts de análisis utilizados en un proyecto de investigación.
La estructura está organizada por etapas de procesamiento.

0_comun/
  - Utilidades.R: funciones auxiliares de uso general.
  - Estadisticas.R: funciones para pruebas estadísticas con remuestreo.

1_preparacion/
  - CrearDataset.R: construcción del conjunto de datos a partir de los archivos fuente.

2_estadisticas/
  - Analisis.R: análisis preliminar y preparación del conjunto de datos limpio.

3_caracterizacion/
  - Clustering.R: procedimientos de agrupamiento y caracterización de grupos.

4_prediccion/
  - Prediccion.R: script principal del proceso.
  - Configuracion.R: constantes y rangos de parámetros.
  - FuncionesGenerales.R: utilidades compartidas.
  - SeleccionarCaracteristicas.R: selección de características.
  - AjustarParametros.R: ajuste de hiperparámetros para distintos modelos.
  - EscogerModelo.R: ajuste y comparación entre varios modelos.
  - EntrenarModelo.R: entrenamiento de modelos específicos.
  - CompararModelos.R: comparación de desempeños entre modelos.
  - CompararModelos.R: define la función que compara dos modelos.
