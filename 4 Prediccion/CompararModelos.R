compararModelos <- function(base, final, datosModelo) {
  # Compara dos modelos.
  # Entrada:
  # - base: modelo base (solo variables de ingreso).
  # - final: modelo final (con competencias informacionales).
  # - datosModelo: descriptor con los datos generales del modelo.
  # Salida:
  # - Booleano que indica si se encuentran diferencias significativas favorables
  #   a la hipótesis de investigación.
  # - Archivo .txt con los resultados de las pruebas.
  # - Archivo .pdf con los gráficos comparativos.
  
  ##############################################################################
  # Funciones de apoyo para comparar clasificadores.
  ##############################################################################
  
  compararClasificadores <- function(base, final, datosModelo) {
    # Compara dos clasificadores.
    # Entrada:
    # - base: modelo base (solo variables de ingreso).
    # - final: modelo final (con competencias informacionales).
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida:
    # - Booleano que indica si se encuentran diferencias significativas.
    # - Archivo .csv con los resultados de las pruebas.
    # - Archivo .pdf con la comparación de las curvas ROC y PR.
    
    # Obtener curvas ROC.
    rocBase <- generarCurvaROC(base)
    rocFinal <- generarCurvaROC(final)
    
    # Obtener curvas PR.
    prBase <- generarCurvaPR(base)
    prFinal <- generarCurvaPR(final)
    
    # Generar gráfico con curvas ROC y PR.
    graficarClasificadores(
      rocBase, rocFinal, prBase, prFinal, datosModelo)
    
    # Inicializar resultados.
    resultado <- data.frame()
    cumple <- TRUE
    
    # Comparar curvas ROC.
    prueba <- rocTest(rocBase, rocFinal)
    resultado <- rbind(resultado, prueba)
    cumple <- cumple || (prueba$p < ALFA && rocFinal$auc > rocBase$auc)
    
    # Comparar curvas PR.
    prueba <- prTest(base, final)
    resultado <- rbind(resultado, prueba)
    
    cumple <- cumple || (prueba$p.value >= ALFA &&
      prFinal$auc.davis.goadrich > prBase$auc.davis.goadrich)
    
    # Comparar métrica.
    prueba <- tTestPar(
      base$metricas[[METRICA_CLAS]], final$metricas[[METRICA_CLAS]])
    
    resultado <- rbind(resultado, prueba)
    
    cumple <- cumple || (prueba$p < ALFA &&
      base$resumen$media[[METRICA_CLAS]] > final$resumen$media[[METRICA_CLAS]])
    
    # Guardar reporte con los resultados.
    guardarReporte(
      datosModelo, formatearReporte(resultado), "Comparacion modelos",
      usarNombre = FALSE)
    
    return(cumple)
  }
  
  graficarClasificadores <- function(
    rocBase, rocFinal, prBase, prFinal, datosModelo) {
    # Genera un gráfico con las curvas ROC y PR de dos modelos de clasificación.
    # Entrada:
    # - rocBase: curva ROC del modelo base (solo variables de ingreso).
    # - rocFinal: curva ROC del modelo final (con competencias
    #   informacionales).
    # - prBase: curva PR del modelo base (solo variables de ingreso).
    # - prFinal: curva PR del modelo final (con competencias informacionales).
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida: archivo .pdf con el gráfico generado.
    
    # Generar cada subgráfico.
    roc <- graficarROC(rocBase, rocFinal)
    pr <- graficarPR(prBase, prFinal)
    
    # Generar los títulos del gráfico combinado.
    titulo = "Comparación de clasificadores"
    
    # Generar y guardar el gráfico combinado.
    g <- (roc | pr) +
      plot_annotation(
        title = titulo, subtitle = datosModelo$respuesta,
        tag_levels = "a", tag_prefix = "(", tag_suffix = ")" ) &
      theme(plot.tag.position = "bottom")
    
    guardarGrafico(
      datosModelo, g, "Comparacion modelos", usarNombre = FALSE, width = 12,
      height = 5)
  }
  
  graficarPR <- function(curvaBase, curvaFinal) {
    # Genera un gráfico con las curvas PR de dos modelos de clasificación
    # Entrada:
    # - curvaBase: curva PR del modelo base (solo variables de ingreso).
    # - curvaFinal: curva PR del modelo final (con competencias informacionales).
    # Salida: objeto con el gráfico generado.
    
    ############################################################################
    # Función de apoyo.
    ############################################################################
    
    construirDataframe <- function(curva, nombreModelo) {
      # Construye el dataframe con los datos de una curva PR para ggplot2.
      # Entrada:
      # - curva: objeto con la curva PR.
      # - nombreModelo: string con el nombre del modelo.
      # Salida: dataframe con datos para graficar.
      
      dfCurva <- data.frame(
        recall = curva$curve[,1],          # eje X = recall
        precision = curva$curve[,2],       # eje Y = precision
        Modelo = nombreModelo
      )
      
      return(dfCurva)
    }
    
    ############################################################################
    # Función principal.
    ############################################################################
    
    # Crear dataframe para generar las curvas
    dfCurvas <- rbind(
      construirDataframe(curvaBase, "Base"),
      construirDataframe(curvaFinal, "Final")
    )
    
    # Crear títulos y anotaciones
    titulo <- "Curvas PR"
    aucBase <- formatearFlotante(curvaBase$auc.davis.goadrich)
    aucFinal <- formatearFlotante(curvaFinal$auc.davis.goadrich)
    
    subtitulo <- paste0(
      "AUC-PR base = ", aucBase, "\nAUC-PR final = ", aucFinal)
    
    # Definir formato para marcas de los ejes.
    formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 1))
    
    # Generar gráfico.
    g <- ggplot(dfCurvas, aes(x = recall, y = precision, color = Modelo)) +
      geom_line(size = 1.2) +
      labs(
        title = titulo, subtitle = subtitulo, x = "Recall", y = "Precision",
        color = "Modelo") + theme_pubr() +
      scale_x_continuous(labels = formatoEjes) +
      scale_y_continuous(labels = formatoEjes)
    
    return(g)
  }
  
  graficarROC <- function(curvaBase, curvaFinal) {
    # Genera un gráfico con las curvas ROC de dos modelos de clasificación
    # Entrada:
    # - curvaBase: curva ROC del modelo base (solo variables de ingreso).
    # - curvaFinal: curva ROC del modelo final (con competencias
    #   informacionales).
    # Salida: objeto con el gráfico generado.
    
    ############################################################################
    # Función de apoyo.
    ############################################################################
    
    construirDataframe <- function(curva, nombreModelo) {
      # Construye el dataframe con los datos de una curva ROC para ggplot2.
      # Entrada:
      # - curva: objeto con la curva ROC.
      # - nombreModelo: string con el nombre del modelo.
      # Salida: dataframe con datos para graficar.
      
      dfCurva <- data.frame(
        falsosPositivos = 1 - curva$specificities,
        verdaderosPositivos = curva$sensitivities, Modelo = nombreModelo)
      
      return(dfCurva)
    }
    
    ############################################################################
    # Función principal.
    ############################################################################
    
    # Crear dataframe para generar las curvas.
    dfCurvas <- rbind(
      construirDataframe(curvaBase, "Base"),
      construirDataframe(curvaFinal, "Final"))
    
    # Crear títulos y anotaciones.
    titulo <- "Curvas ROC"
    aucBase <- formatearFlotante(curvaBase$auc)
    aucFinal <- formatearFlotante(curvaFinal$auc)
    
    subtitulo <- paste0(
      "AUC-ROC base = ", aucBase, "\nAUC-ROC final = ", aucFinal)
    
    # Definir formato para marcas de los ejes.
    formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 1))
    
    # Generar gráfico.
    g <- ggplot(
      dfCurvas, aes(
        x = falsosPositivos, y = verdaderosPositivos, color = Modelo)) +
      geom_line(size = 1.2) +
      geom_abline(
        slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
      labs(
        title = titulo, subtitle = subtitulo, x = "1 - Especificidad",
        y = "Sensibilidad", color = "Modelo") + theme_pubr() +
      scale_x_continuous(labels = formatoEjes) +
      scale_y_continuous(labels = formatoEjes)
    
    
    return(g)
  }
  
  
  
  ##############################################################################
  # Funciones de apoyo para comparar regresores.
  ##############################################################################
  
  compararRegresores <- function(base, final, datosModelo) {
    # Compara dos regresores.
    # Entrada:
    # - base: modelo base (solo variables de ingreso).
    # - final: modelo final (con competencias informacionales).
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida:
    # - Booleano que indica si se encuentran diferencias significativas.
    # - Archivo .csv con los resultados de las pruebas.
    # - Archivo .pdf con la comparación de las predicciones y los residuos.
    
    # Generar gráfico con predicciones y residuos.
    graficarRegresores(base, final, datosModelo)
    
    # Comparar métrica.
    resultado <- tTestPar(
      base$metricas[[METRICA_REG]], final$metricas[[METRICA_REG]])
    
    cumple <- resultado$p >= ALFA &&
      final$resumen$media[[METRICA_REG]] < base$resumen$media[[METRICA_REG]]
    
    # Guardar reporte con los resultados.
    guardarReporte(
      datosModelo, formatearReporte(resultado), "Comparacion modelos",
      usarNombre = FALSE)
    
    return(cumple)
  }
  
  graficarPredicciones <- function(base, final) {
    # Compara las predicciones de dos modelos de regresión.
    # Entrada:
    # - base: modelo base (solo variables de ingreso).
    # - final: modelo final (con competencias informacionales).
    # Salida: objeto con el gráfico generado.
    
    df <- data.frame(
      observado = base$predicciones$Observado,
      prediccionesBase = base$predicciones$Predicho,
      prediccionesFinal = final$predicciones$Predicho)
    
    df <- df %>%
      pivot_longer(
        cols = c(prediccionesBase, prediccionesFinal),
        names_to = "Modelo",
        values_to = "Predicho") %>%
      mutate(Modelo = dplyr::recode(
        Modelo, "prediccionesBase" = "Base", "prediccionesFinal" = "Final"))
    
    titulo <- "Estimaciones"
    formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 1))
    
    g <- ggplot(df, aes(x = observado, y = Predicho, color = Modelo)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = "gray40") +
      scale_color_manual(values = c("Base" = "#F8766D", "Final" = "#00BFC4")) +
      labs(
        title = titulo, x = "Valor observado", y = "Valor estimado",
        color = "Modelo") + theme_pubr() +
      scale_x_continuous(labels = formatoEjes) +
      scale_y_continuous(labels = formatoEjes)
    
    return(g)
  }
  
  graficarRegresores <- function(base, final, datosModelo) {
    # Genera un gráfico con las predicciones y los residuos de dos modelos de
    # regresión.
    # Entrada:
    # - base: modelo base (solo variables de ingreso).
    # - final: modelo final (con competencias informacionales).
    # - datosModelo: descriptor con los datos generales del modelo.
    # Salida: archivo .pdf con el gráfico generado.
    
    # Generar cada subgráfico.
    predicciones <- graficarPredicciones(base, final)
    residuos <- graficarResiduos(base, final)
    
    # Generar los títulos del gráfico combinado.
    titulo = "Comparación de regresores"
    
    # Generar y guardar el gráfico combinado.
    g <- (predicciones | residuos) +
      plot_annotation(
        title = titulo, subtitle = datosModelo$respuesta,
        tag_levels = "a", tag_prefix = "(", tag_suffix = ")" ) &
      theme(plot.tag.position = "bottom")
    
    guardarGrafico(
      datosModelo, g, "Comparacion modelos", usarNombre = FALSE, width = 12,
      height = 5)
  }
  
  graficarResiduos <- function(base, final) {
    # Compara las distribuciones de residuos de dos modelos de regresión.
    # Entrada:
    # - base: modelo base.
    # - final: modelo final.
    # Salida: objeto con el gráfico generado.
    
    df <- rbind(
      data.frame(
        Modelo = "Base",
        Residuo = base$predicciones$Observado - base$predicciones$Predicho),
      data.frame(
        Modelo = "Final",
        Residuo = final$predicciones$Observado - final$predicciones$Predicho))
    
    titulo <- "Residuos"
    formatoEjes <- function(x) sapply(x, function(v) formatearFlotante(v, 1))
    
    g <- ggplot(df, aes(x = Residuo, fill = Modelo)) +
      geom_density(alpha = 0.5) +
      labs(title = titulo, x = "Residuo", y = "Densidad", fill = "Modelo") +
      theme_pubr() + scale_x_continuous(labels = formatoEjes) +
      scale_y_continuous(labels = formatoEjes)
    
    return(g)
  }
  
  
  
  ##############################################################################
  # Función principal.
  ##############################################################################
  
  diferencia <- if(datosModelo$clasificacion) {
    compararClasificadores(base, final, datosModelo)
  } else {
    compararRegresores(base, final, datosModelo)
  }
  
  return(diferencia)
}
