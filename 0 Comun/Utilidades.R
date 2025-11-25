crearRuta <- function(nombreArchivo) {
  # Verifica si existe la ruta donde se requiere guardar una ruta y la crea en
  # caso necesario.
  # Entrada:
  # - nombreArchivo: string con el nombre (con ruta) del archivo.
  # Salida: ninguna.
  
  ruta <- dirname(nombreArchivo)
  
  if(!dir.exists(ruta)) {
    dir.create(ruta, recursive = TRUE)
  }
}

formatearFlotante <- function(numero, decimales = 3) {
  # Formatea un número flotante como un string con coma decimal y la cantidad
  # solicitada de decimales.
  # Entrada:
  # - numero: flotante a formatear.
  # - decimales: cantidad de decimales.
  # Salida: string correspondiente al flotante formateado.
  
  resultado <- sapply(numero, function(x) {
    if(is.na(x)) return("")
    formatC(x, format = "f", digits = decimales, decimal.mark = ",")
  })
  
  return(resultado)
}

guardarDataframe <- function(df, nombreArchivo) {
  # Guarda un dataframe en un archivo csv con codificación latin1 y sin nombres
  # de fila. Si la ruta no existe, la crea.
  # Entrada:
  # - df: dataframe.
  # - nombreArchivo: nombre (con ruta) del archivo.
  # Salida: archivo .csv.
  
  crearRuta(nombreArchivo)
  write.csv2(df, nombreArchivo, row.names = FALSE, fileEncoding = "Latin1")
}

importarPaquetes <- function(paquetes) {
  # Importa paquetes requeridos de manera silenciosa.
  # Entrada:
  # - paquetes: vector con los nombres de los paquetes a importar.
  # Salida: ninguna.
  
  # Función de apoyo para importar un paquete.  
  importarSilencioso <- function(paquete) {
    suppressPackageStartupMessages({
      if(!require(paquete, character.only = TRUE)) {
        install.packages(paquete, dependencies = TRUE)
        require(paquete, character.only = TRUE)
      }
    })
  }
  
  # Importación de paquetes.
  opcionesIniciales <- options(warn = -1)
  lapply(paquetes, importarSilencioso)
  options(opcionesIniciales)
  rm(opcionesIniciales)
}

