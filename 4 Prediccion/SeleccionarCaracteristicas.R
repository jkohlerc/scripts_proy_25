seleccionarCaracteristicas <- function(
    datosModelo, maximoVif = MAX_VIF, semilla = SEMILLA) {
  # Hace el proceso de selección de características con Boruta y luego descarta
  # variables fuertemente correlacionadas.
  # Entrada:
  # - datosModelo: descriptor con los datos generales del modelo.
  # - maximoVif: máxima inflación de varianza permitida.
  # - semilla: semilla.
  # Salida:
  # - Descriptor con los datos generales del modelo actualizado.
  # - Archivo .csv con el reporte de los resultados.
  # - Archivo .pdf con el gráfico de importancia de las variables.
  # - Archivo .pdf con el gráfico de convergencia para la cantidad de árboles de
  #   random forest.
  
  ##############################################################################
  # Funciones de apoyo.
  ##############################################################################
  
  descartarCorrelacionados <- function(df, datosModelo, maximoVif = MAX_VIF) {
    # Elimina variables con un alto factor de inflación de varianza para modelos
    # de regresión lineal y logística.
    # Entrada:
    # - df: dataframe con el conjunto de predictores seleccionado por Boruta.
    # - datosModelo: descriptor con los datos generales del modelo.
    # - maximoVif: máxima inflación de varianza permitida.
    # Salida: vector con los nombres de los predictores seleccionados.
    
    predictores <- setdiff(names(df), "respuesta")
    continuar <- TRUE
    primero <- TRUE
    
    while(continuar && length(predictores) > 1) {
      formula <- as.formula(
        paste("respuesta ~ ", paste(predictores, collapse = "+")))
      
      modelo <- tryCatch({
        if(datosModelo$clasificacion) {
          glm(formula, data = df, family = binomial(link = "logit"))
        } else {
          lm(formula, data = df)
        }
      }, error = function(e) NULL)
      
      if(is.null(modelo)) break
      vifs <- tryCatch(car::vif(modelo), error = function(e) NULL)
      
      if(is.null(vifs) || all(is.na(vifs))) {
        break
      } else if(primero) {
        primero <- FALSE
        dfVifs <- data.frame(Variable = names(vifs), VIF = round(vifs, 3))
        guardarReporte(datosModelo, dfVifs, "VIF seleccion", usarNombre = TRUE)
      }
      
      if(max(vifs, na.rm = TRUE) > maximoVif) {
        predictores <- setdiff(predictores, names(vifs)[which.max(vifs)])
      } else {
        continuar <- FALSE
      }
    }

    return(predictores)
  }
  
  graficarImportanciaBoruta <- function(
    datosModelo, resultadoBoruta, predictores) {
    # Genera un gráfico de cajas con la importancia calculada por Boruta para
    # todas las variables, diferenciando por color el estado final.
    # Entrada:
    # - datosModelo: descriptor con los datos generales del modelo.
    # - resultadoBoruta: lista con el resultado de Boruta.
    # - predictores: vector con nombres de predictores seleccionados.
    # Salida: archivo .pdf con el gráfico.
    
    # Preparar dataframe con importancias de Boruta.
    importancia <- as.data.frame(resultadoBoruta$boruta$ImpHistory)
    importancia$Iteracion <- as.integer(rownames(importancia))
    
    importancia <- importancia %>%
      pivot_longer(
        cols = -Iteracion, names_to = "Variable", values_to = "Importancia") %>%
      filter(is.finite(Importancia)) %>% as.data.frame()
    
    # Añadir una columna con el estado final de cada variable.
    importancia$Estado <- "Rechazado Boruta"
    importancia$Estado[grepl("shadow", importancia$Variable)] <- "Sombra"
    
    tentativas <- names(resultadoBoruta$boruta$finalDecision)[
      resultadoBoruta$boruta$finalDecision == "Tentative"]
    importancia$Estado[importancia$Variable %in% tentativas] <- "Tentativo"
    
    importancia$Estado[importancia$Variable %in% predictores] <- "Confirmado"
    rechazadoVif <- setdiff(resultadoBoruta$predictores, predictores)
    importancia$Estado[importancia$Variable %in% rechazadoVif] <- "Rechazado VIF"
    
    niveles <- c(
      "Sombra", "Rechazado Boruta", "Tentativo", "Rechazado VIF", "Confirmado")
    
    importancia$Estado <- factor(importancia$Estado, levels = niveles)
    
    # Renombrar columnas de sombras.
    importancia$Variable <- stringr::str_replace_all(
      importancia$Variable, c(
        "shadowMax"  = "sombraMax", "shadowMin"  = "sombraMin",
        "shadowMean" = "sombraMedia"))
    
    # Ordenar importancias por estado.
    ordenado <- importancia %>%
      group_by(Estado, Variable) %>%
      summarise(MediaImportancia = mean(Importancia), .groups = "drop") %>%
      arrange(Estado, desc(MediaImportancia))
    
    importancia$Variable <- factor(
      importancia$Variable, levels = ordenado$Variable)
    
    # Colores para cada estado
    colores <- c(
      "Sombra" = "#5DADE2", "Confirmado" = "#58D68D", "Tentativo" = "#F4D03F",
      "Rechazado Boruta" = "#E74C3C", "Rechazado VIF" = "#EB984E")
    
    # Generar el gráfico
    g <- ggplot(importancia, aes(x = Variable, y = Importancia, fill = Estado)) +
      geom_boxplot(outlier.size = 1, outlier.alpha = 0.5) +
      scale_fill_manual(values = colores, name = "Estado") +
      theme_pubr() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      labs(
        title = "Selección de características",
        subtitle = paste0(
          datosModelo$respuesta, ": modelo ", tolower(datosModelo$nombre)),
        x = "Variable",
        y = "Importancia")
    
    # Guardar el gráfico.
    cajas <- length(unique(importancia$Variable))
    ancho <- 0.8 * cajas
    
    guardarGrafico(
      datosModelo, g, "Importancia Boruta", usarNombre = TRUE, width = ancho,
      height = 6)
  }
  
  seleccionarRelevantes <- function(df, semilla = SEMILLA) {
    # Elimina variables que no aportan información.
    # Entrada:
    # - df: dataframe con el conjunto inicial de predictores.
    # - semilla: semilla.
    # Salida: lista con los siguientes elementos:
    # - boruta: objeto con los resultados de Boruta.
    # - seleccion: objeto con el ajuste de Boruta (TentativeRoughFix).
    # - predictores: vector con los nombres de los predictores seleccionados.
    
    # Seleccionar predictores iniciales con Boruta.
    iteraciones <- (ncol(df) - 1) * 20
    
    set.seed(semilla)
    
    boruta <- Boruta(
      respuesta ~., data = df, doTrace = 0, maxRuns = iteraciones)
    
    seleccion <- TentativeRoughFix(boruta)
    
    predictores <- names(
      seleccion$finalDecision[seleccion$finalDecision == "Confirmed"])
    
    if(length(predictores) == 0) {
      predictores <- names(
        seleccion$finalDecision[seleccion$finalDecision != "Rejected"])
    }
    
    if(length(predictores) == 0) {
      predictores <- colnames(df %>% select(-respuesta))
    }
    
    resultado <- list(
      boruta = boruta, seleccion = seleccion, predictores = predictores)
    
    return(resultado)
  }
  
  
  
  ##############################################################################
  # Función principal.
  ##############################################################################
  
  resultadoBoruta <- seleccionarRelevantes(datosModelo$df)
  
  filtrado <- datosModelo$df %>% select(
    all_of(c("respuesta", resultadoBoruta$predictores)))
  
  predictores <- descartarCorrelacionados(filtrado, datosModelo, maximoVif)
  filtrado <- filtrado %>% select(all_of(c("respuesta", predictores)))
  graficarImportanciaBoruta(datosModelo, resultadoBoruta, predictores)
  datosModelo$df <- filtrado  
  return(datosModelo)
}
