# Función para cargar y preparar datos
load_and_prepare_data <- function(file_path) {
  # Cargar el conjunto de datos desde un archivo CSV
  data <- read.csv(file_path, header = TRUE)
  
  dataclass <- data$NObeyesdad  # Correcta extracción de la columna
  
  # Convertir datos categóricos a numéricos
  data$Gender <- as.numeric(factor(data$Gender, levels = c("Female","Male"),labels = c(0,1)))
  data$CAEC <- as.numeric(factor(data$CAEC, levels = c("no", "Sometimes", "Frequently", "Always"), labels = c(0, 1, 2, 3)))
  data$CALC <- as.numeric(factor(data$CALC, levels = c("no", "Sometimes", "Frequently", "Always"), labels = c(0, 1, 2, 3)))
  data$MTRANS <- as.numeric(factor(data$MTRANS, levels = c("Public_Transportation","Walking","Automobile","Motorbike","Bike"), labels = c(0, 1, 2, 3, 4)))
  data$family_history_with_overweight <- as.numeric(factor(data$family_history_with_overweight, levels = c("no","yes"), labels = c(0,1)))
  data$FAVC <- as.numeric(factor(data$FAVC, levels = c("no","yes"), labels = c(0,1)))
  #data$SMOKE <- as.numeric(factor(data$SMOKE, levels = c("no","yes"), labels = c(0,1)))
  #data$SCC <- as.numeric(factor(data$SCC, levels = c("no","yes"), labels = c(0,1)))
  
  
  # Filtrar columnas numéricas para calcular las medias y desviaciones estándar
  numeric_cols <- sapply(data, is.numeric)
  df_numeric <- data[, numeric_cols]
  
  # Calcular las medias y desviaciones estándar de las columnas numéricas
  mean_features <- sapply(df_numeric, mean, na.rm = TRUE)
  sd_features <- sapply(df_numeric, sd, na.rm = TRUE)
  
  # Normalizar los datos utilizando las medias y desviaciones estándar calculadas
  dataNorm <- as.data.frame(scale(df_numeric, center = mean_features, scale = sd_features))
  
  # Número de filas antes de eliminar valores extremos
  num_filas_antes <- nrow(dataNorm)
  
  # Filtrar valores extremos por media
  isindexExtremo <- apply(dataNorm, 2, function(x) isVExtremo(x, 0.0, 1.0))
  dataNorm <- dataNorm[!apply(isindexExtremo, 1, any), ]
  
  # Número de filas después de eliminar valores extremos
  num_filas_después <- nrow(dataNorm)
  
  # Imprimir el número de filas antes y después de eliminar valores extremos
  cat("No filas antes de eliminar valores extremos: ", num_filas_antes, "\n")
  cat("No filas después de eliminar valores extremos: ", num_filas_después, "\n")
  cat("Filas eliminadas: ", num_filas_antes - num_filas_después, "\n")
  
  
  # Crear un nuevo dataframe 'dataclass_filtered' que contenga solo las filas que quedan en 'dataNorm'
  dataclass_filtered <- dataclass[!apply(isindexExtremo, 1, any)]
  # Convertir los datos en un dataframe de una columna
  dataclass_filtered <- data.frame(NObeyesdad = dataclass_filtered)
  # Obtener el número de filas en dataclass_filtered
  num_filas_dataclass <- nrow(dataclass_filtered)
  
  # Imprimir el número de filas
  cat("Número de filas en dataclass_filtered:", num_filas_dataclass, "\n")
  
  return(list(dataNorm = dataNorm, dataclass = dataclass_filtered,noFilas = num_filas_después))
}

