# Instalar las bibliotecas necesarias
install.packages("haven")  # Para leer archivos .sav de SPSS
install.packages("dplyr")  # Para manipulación y transformación de datos
install.packages("arules") # Para aplicar el algoritmo Apriori
install.packages("https://mhahsler.github.io/arules/docs/fim4r/fim4r_latest.tar.gz", repos = NULL, type = "source")  # Para el algoritmo FP-Growth
install.packages("ggplot2") # Para graficar

# Cargar las bibliotecas
library(haven)    # Carga la librería haven para trabajar con archivos .sav
library(dplyr)    # Carga dplyr para manipulación de datos
library(arules)   # Carga arules para aplicar el algoritmo Apriori
library(ggplot2)  # Carga ggplot2 para gráficos de KMeans
library(fim4r)    # Carga fim4r para aplicar el algoritmo FP-Growth

# 1. Cargar Matrimonios:

# Ruta a la carpeta que contiene los archivos .sav
carpeta <- "D:/Datos Matrimonios y Divorcios/Matrimonios"

# Obtener una lista de todos los archivos .sav en la carpeta
archivos_sav <- list.files(path = carpeta, pattern = "\\.sav$", full.names = TRUE)

# Leer cada archivo .sav y almacenarlo en una lista
lista_datos <- lapply(archivos_sav, read_sav)

# Convertir las columnas CIUOHOM y CIUOMUJ a tipo character para evitar problemas de conversión
lista_datos <- lapply(lista_datos, function(df) {
  df <- mutate(df,
               CIUOHOM = as.character(CIUOHOM),
               CIUOMUJ = as.character(CIUOMUJ))  # Convertimos CIUOHOM y CIUOMUJ a character
  return(df)
})

# Combinar todos los dataframes en un solo dataframe
combined_df <- bind_rows(lista_datos)

# 2. Cargar Divorcios:

# Ruta a la carpeta que contiene los archivos .sav para los divorcios
carpeta <- "D:/Datos Matrimonios y Divorcios/Divorcios"

# Obtener una lista de todos los archivos .sav en la carpeta
archivos_sav2 <- list.files(path = carpeta, pattern = "\\.sav$", full.names = TRUE)

# Leer cada archivo .sav y almacenarlo en una lista
lista_datos2 <- lapply(archivos_sav2, read_sav)

# Convertir las columnas CIUOHOM y CIUOMUJ a tipo character
lista_datos2 <- lapply(lista_datos2, function(df2) {
  df2 <- mutate(df2,
                CIUOHOM = as.character(CIUOHOM),
                CIUOMUJ = as.character(CIUOMUJ))  # Convertimos CIUOHOM y CIUOMUJ a character
  return(df2)
})

# Combinar todos los dataframes de divorcios en un solo dataframe
combined_df2 <- bind_rows(lista_datos2)

# 3. Aplicación del Algoritmo Apriori:

# Genera reglas de asociación para Matrimonios con soporte 0.4 y confianza 0.6
reglas <- apriori(combined_df, parameter = list(support=0.4, confidence=0.6))

# Convierte las reglas en un dataframe para fácil visualización
reglasframe <- as(reglas, "data.frame")
View(reglasframe)

# Genera reglas de asociación para Divorcios con soporte 0.4 y confianza 0.6
reglas2 <- apriori(combined_df2, parameter = list(support=0.4, confidence=0.6))

# Convierte las reglas de divorcios en un dataframe para visualización
reglasframe2 <- as(reglas2, "data.frame")
View(reglasframe2)

# 4. Aplicación del Algoritmo FP-Growth:

# Para Matrimonios:
# Filtrar los datos donde el pueblo de la mujer es Maya (PUEMUJ == 1)
datamsc <- subset(combined_df, PUEMUJ == 1)
# Eliminar la columna "PUEMUJ" para evitarla en el análisis
datamsc2 <- datamsc[, !(names(datamsc) %in% c("PUEMUJ"))]

# Generar reglas de asociación usando FP-Growth con soporte 0.2 y confianza 0.5
reglas2_1 <- fim4r(datamsc2, method = "fpgrowth", target = "rules", supp = .2, conf = .5)
reglas2_1_df <- as(reglas2_1, "data.frame")
View(reglas2_1_df)

# Para Divorcios:
# Filtrar los datos donde el hombre tiene educación Primaria (ESCHOM == 1)
datamsc_ <- subset(combined_df2, ESCHOM == 1)
# Eliminar la columna "ESCHOM"
datamsc2_ <- datamsc_[, !(names(datamsc_) %in% c("ESCHOM"))]

# Generar reglas de asociación usando FP-Growth con soporte 0.2 y confianza 0.5
reglas2_2 <- fim4r(datamsc2_, method = "fpgrowth", target = "rules", supp = .2, conf = .5)
reglas2_2_df <- as(reglas2_2, "data.frame")
View(reglas2_2_df)

# 5. Análisis de Clúster:

# Para Matrimonios:
# Selección de las columnas de edad del hombre y la mujer
data_seleccion <- combined_df[, c("EDADHOM", "EDADMUJ")]
# Filtrar los datos para excluir edades mayores a 100
data_seleccion <- subset(data_seleccion, EDADHOM < 100)
# Tomar una muestra aleatoria de 1000 registros
data_reducida <- sample_n(data_seleccion, 1000)

# Aplicar el algoritmo KMeans con 4 clústeres
cluster <- kmeans(data_reducida, centers = 4)

# Graficar los resultados de KMeans para Matrimonios
centros <- as.data.frame(cluster$centers)
names(centros) <- c("EDADHOM", "EDADMUJ")

grafico <- ggplot(data_reducida, aes(x = EDADHOM, y = EDADMUJ, color = as.factor(cluster$cluster))) + 
  geom_point() +
  geom_point(data = centros, aes(x = EDADHOM, y = EDADMUJ), color = "red", size = 4, shape = 8) +
  labs(color = "Cluster") +
  theme_minimal()

# Mostrar gráfico de Matrimonios
grafico

# Para Divorcios:
# Selección de las columnas de edad del hombre y la mujer
data_seleccion2 <- combined_df2[, c("EDADHOM", "EDADMUJ")]
# Filtrar los datos para excluir edades mayores a 100
data_seleccion2 <- subset(data_seleccion2, EDADHOM < 100)
# Tomar una muestra aleatoria de 1000 registros
data_reducida2 <- sample_n(data_seleccion2, 1000)

# Aplicar el algoritmo KMeans con 4 clústeres para Divorcios
cluster2 <- kmeans(data_reducida2, centers = 4)

# Graficar los resultados de KMeans para Divorcios
centros2 <- as.data.frame(cluster2$centers)
names(centros2) <- c("EDADHOM2", "EDADMUJ2")

grafico2 <- ggplot(data_reducida2, aes(x = EDADHOM, y = EDADMUJ, color = as.factor(cluster2$cluster))) + 
  geom_point() +
  geom_point(data = centros2, aes(x = EDADHOM2, y = EDADMUJ2), color = "blue", size = 4, shape = 8) +
  labs(color = "Cluster") +
  theme_minimal()

# Mostrar gráfico de Divorcios
grafico2

