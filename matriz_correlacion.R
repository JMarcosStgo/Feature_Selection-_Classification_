# Función para calcular y imprimir la matriz de correlación
calculate_and_print_correlation <- function(dataNorm) {
  # Calcular la matriz de correlación redondeada a dos decimales
  correlacion_df <- round(cor(dataNorm), 2)
  
  # Imprimir la matriz de correlación
  print(correlacion_df)
  
  # Grafico de correlación de datos
  ggcorr(correlacion_df, label_size = 7) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ajustar el ángulo de las etiquetas del eje x
}
