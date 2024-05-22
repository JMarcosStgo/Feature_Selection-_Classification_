select_forward_features <- function(dataNorm, fs, alpha1 = 1, alpha2 = 1) {
  
  # Crear un dataframe a partir de fs
  fs_df <- data.frame(t(fs$fs), stringsAsFactors = FALSE)
  
  # Establecer los nombres de las columnas
  colnames(fs_df) <- fs$column_names
  
  # Obtener el nombre de la columna en la posición 0, con más rankeo
  namesCol <- names(fs_df)  
  
  # Obtener el primer elemento de namesCol
  primerElemento <- head(namesCol, 1)
  
  # Obtener el resto de elementos de namesCol
  restoElementos <- tail(namesCol, -1)
  
  # Calcular la correlación de Pearson entre el primer elemento y el resto de elementos en dataNorm
  correlations <- sapply(restoElementos, function(col) cor(dataNorm[[primerElemento]], dataNorm[[col]]))
  print("correlations")
  print(correlations)
  
  
  
  # Seleccionar la segunda característica
  
  # Inicializar un vector para almacenar los resultados
  resultados <- numeric(length(correlations))
  # Calcular el resultado para cada par de valores J2 y R15
  for (i in seq_along(correlations)) {
    resultados[i] <- 0.5 * fs_df[[i]] - 0.5 * abs(correlations[i])
  }
  print("resultados para seleccionar la segunda caracteristica")
  print(resultados)
  
  
  return(correlations)
}
