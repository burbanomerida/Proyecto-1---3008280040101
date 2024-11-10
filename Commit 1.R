install.packages("haven")
install.packages("dplyr")

# Cargar la biblioteca haven para leer archivos .sav
library(haven)

#MATRIMONIOS:

# Ruta a la carpeta que contiene los archivos .sav
carpeta <- "D:/Datos Matrimonios y Divorcios/Matrimonios"

# Obtener una lista de todos los archivos .sav en la carpeta
archivos_sav <- list.files(path = carpeta, pattern = "\\.sav$", full.names = TRUE)

# Leer cada archivo .sav y almacenarlo en una lista
lista_datos <- lapply(archivos_sav, read_sav)

# Visualizar la lista de data.frames
str(lista_datos)

# Visualizar cada data.frame en la lista
View(lista_datos[[1]]) # Muestra el 1
View(lista_datos[[2]]) # Muestra el 2
View(lista_datos[[3]]) # Muestra el 3
View(lista_datos[[4]]) # Muestra el 4
View(lista_datos[[5]]) # Muestra el 5
View(lista_datos[[6]]) # Muestra el 6
View(lista_datos[[7]]) # Muestra el 7
View(lista_datos[[8]]) # Muestra el 8

# Cargando el paquete dplyr
library(dplyr)

# Detecta y convierte automáticamente las columnas con diferentes tipos de datos
lista_datos <- lapply(lista_datos, function(df) {
  df <- mutate(df,
               CIUOHOM = as.character(CIUOHOM),
               CIUOMUJ = as.character(CIUOMUJ))  # Convertimos CIUOHOM y CIUOMUJ a character
  return(df)
})

# Combinando todos los dataframes en un solo dataframe
combined_df <- bind_rows(lista_datos)

# Mostrar registros del dataframe combinado
View(combined_df)




#DIVORCIOS:

# Ruta a la carpeta que contiene los archivos .sav
carpeta <- "D:/Datos Matrimonios y Divorcios/Divorcios"

# Obtener una lista de todos los archivos .sav en la carpeta
archivos_sav2 <- list.files(path = carpeta, pattern = "\\.sav$", full.names = TRUE)

# Leer cada archivo .sav y almacenarlo en una lista
lista_datos2 <- lapply(archivos_sav, read_sav)

# Visualizar la lista de data.frames
str(lista_datos2)

# Visualizar cada data.frame en la lista
View(lista_datos2[[1]]) # Muestra el 1
View(lista_datos2[[2]]) # Muestra el 2
View(lista_datos2[[3]]) # Muestra el 3
View(lista_datos2[[4]]) # Muestra el 4
View(lista_datos2[[5]]) # Muestra el 5
View(lista_datos2[[6]]) # Muestra el 6
View(lista_datos2[[7]]) # Muestra el 7
View(lista_datos2[[8]]) # Muestra el 8

# Detecta y convierte automáticamente las columnas con diferentes tipos de datos
lista_datos2 <- lapply(lista_datos2, function(df2) {
  df2 <- mutate(df2,
               CIUOHOM = as.character(CIUOHOM),
               CIUOMUJ = as.character(CIUOMUJ))  # Convertimos CIUOHOM y CIUOMUJ a character
  return(df2)
})


# Combinando todos los dataframes en un solo dataframe
combined_df2 <- bind_rows(lista_datos2)

# Mostrar registros del dataframe combinado
View(combined_df2)