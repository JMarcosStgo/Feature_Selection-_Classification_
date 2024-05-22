calculate_fisher <- function(data, dataclass, noFilas) {
  # Calcular la media global de cada columna
  media_global <- colMeans(data)
  cat("Medias globales:", media_global, "\n")
  
  # Obtener las clases únicas en la columna NObeyesdad  
  elementos_unicos <- unique(dataclass$NObeyesdad)
  cat("Clases únicas:", elementos_unicos, "\n")
  
  # Combinar data y dataclass en un solo dataframe
  df <- cbind(data, dataclass)
  colnames(df)[ncol(df)] <- "Class" # Renombrar la última columna como "Class"
  
  # Convertir la columna "Class" a factor
  df$Class <- as.factor(df$Class)
  
  # Inicializar una lista para almacenar los resultados por columna
  results <- list()
  
  # Iterar sobre cada columna de data
  for (col in 1:ncol(data)) {
    # Calcular la media y la desviación estándar por clase para cada columna
    stats <- aggregate(df[, col], by=list(df$Class), FUN=function(x) c(mean=mean(x), sd=sd(x)))
    # Organizar los resultados en un dataframe
    results[[col]] <- stats
  }
  
  # Obtener la frecuencia de los elementos únicos en la columna "Class"
  class_frequencies <- table(df$Class)
  
  # Inicializar una lista para almacenar los resultados de frecuencia
  fs <- numeric()  
  
  # Inicializar un vector para almacenar los nombres de las columnas
  column_names <- names(data)
  
  # Inicializar un dataframe para almacenar los resultados de fs
  fs_df <- data.frame(column_names, fs=numeric(length(column_names)))
  
  # Iterar sobre cada elemento de la lista de resultados
  for (col_index in seq_along(results)) {
    # Acceder a los resultados de la columna actual
    column_results <- results[[col_index]]
    
    # Inicializar listas para medias y desviaciones estándar
    medias <- numeric()
    desvests <- numeric() 
    
    # Iterar sobre cada fila de resultados en la columna actual
    for (i in seq_len(nrow(column_results))) {
      # Acceder al resultado actual
      result_row <- column_results[i, ]
      mediaSd <- result_row[2]
      
      # Acceder a los valores de x.mean y x.sd por nombre de columna
      media_clase <- mediaSd[1, 1][1]
      sd_clase <- mediaSd[1, 1][2]
      
      medias <- c(medias, media_clase)
      desvests <- c(desvests, sd_clase)
    }
    
    # Calcular el factor de Fisher para cada columna
    numerator <- sum((class_frequencies / noFilas) * (medias - media_global[col_index])^2)
    denominator <- sum((class_frequencies / noFilas) * desvests^2)
    fs <- numerator / denominator
    
    # Asignar el valor de fs al dataframe fs_df
    fs_df$fs[col_index] <- fs
  }
  
  cat("Factor de Fisher:", "\n")
  
  # Ordenar fs_df por el valor de fs de mayor a menor
  fs_df <- fs_df[order(fs_df$fs, decreasing = TRUE), ]
  
  print(fs_df)
  
  # Obtener el índice del máximo valor en la lista fs
  indice_maximo <- which.max(fs_df$fs)
  
  # Obtener el valor máximo
  valor_maximo <- fs_df$fs[indice_maximo]
  
  return(list(fs=fs_df, indice_maximo=indice_maximo, valor_maximo=valor_maximo))
}
