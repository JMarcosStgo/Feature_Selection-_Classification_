library(corrplot)
library(cluster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)


# Establecer semilla para reproducibilidad
set.seed(123)

# Ruta del archivo CSV
setwd("//your+path//")
file_path <- "your+path//ObesityDataSet_raw_and_data_sinthetic.csv"

# Cargar módulos
source("preprocesamiento.R")
source("fisher.R")
source("matriz_correlacion.R")
source("escalarHaciaAdelante.R")



# Cargar y aplicar el preprocesamiento de los datos
load_data <- load_and_prepare_data(file_path)

#obtiene la correlacion
calculate_and_print_correlation(load_data$dataNorm)

# obtiene el factor de fisher por cada columna
stats <- calculate_fisher(load_data$dataNorm, load_data$dataclass, load_data$noFilas)
# Imprimir el índice y el valor máximo
print(paste("El índice del máximo valor es:", stats$indice_maximo))
print(paste("El máximo valor es:", stats$valor_maximo))

View(load_data$dataNorm)
selected_features <- select_forward_features(load_data$dataNorm, stats$fs)
#print("Características seleccionadas:")
#print(selected_features)






