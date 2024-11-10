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





#ALGORITMO APRIORI:

install.packages("arules")
library(arules)

#MATROMONIOS:

#Genera reglas de asociación en combined_df con soporte 0.4 y confianza 0.6.
reglas <- apriori(combined_df, parameter = list(support=0.4, confidence=0.6))

#Convierte las reglas en un dataframe para fácil visualización
reglasframe <- as (reglas, "data.frame")
View(reglasframe)

#DIVORCIOS:

#Genera reglas de asociación en combined_df con soporte 0.4 y confianza 0.6.
reglas2 <- apriori(combined_df2, parameter = list(support=0.4, confidence=0.6))

#Convierte las reglas en un dataframe para fácil visualización
reglasframe2 <- as (reglas, "data.frame")
View(reglasframe2)





#ALGORITMO FPGROWTH:
#Instalar paquete de aplicación de fim4r
install.packages("https://mhahsler.github.io/arules/docs/fim4r/fim4r_latest.tar.gz", repos = NULL, type = "source")
system("make --version")

#MATRIMONIOS:

#Se discrimina el pueblo de la mujer a Maya
datamsc <- subset(combined_df, PUEMUJ == 1)
datamsc2 <- datamsc[, !(names(datamsc) %in% c("PUEMUJ"))]

#Se crean las transacciones y se visualizan
reglas2_1 <- fim4r(datamsc2, method = "fpgrowth", target = "rules", supp = .2, conf = .5)
reglas2_1_df <- as(reglas2_1, "data.frame")
View(reglas2_1_df)


#DIVORCIOS

#Se discrimina la educación del hombre a Primaria
datamsc_ <- subset(combined_df2, ESCHOM == 1)
datamsc2_ <- datamsc_[, !(names(datamsc_) %in% c("ESCHOM"))]

#Se crean las transacciones y se visualizan
reglas2_2 <- fim4r(datamsc2_, method = "fpgrowth", target = "rules", supp = .2, conf = .5)
reglas2_2_df <- as(reglas2_2, "data.frame")
View(reglas2_2_df)




#ANÁLISIS DE CLUSTER:

#MATRIMONIOS:
#Se seleccionan los datos a evaluar
data_seleccion <- combined_df[, c("EDADHOM", "EDADMUJ")]
data_seleccion <- subset(data_seleccion, EDADHOM < 100)
data_reducida <- sample_n(data_seleccion, 1000)
cluster <- kmeans(data_reducida, centers = 4)


# DIVORCIOS
# Selección de datos para el segundo gráfico
data_seleccion2 <- combined_df2[, c("EDADHOM", "EDADMUJ")]
data_seleccion2 <- subset(data_seleccion2, EDADHOM < 100)
data_reducida2 <- sample_n(data_seleccion2, 1000)
cluster2 <- kmeans(data_reducida2, centers = 4)



