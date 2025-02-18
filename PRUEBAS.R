#EJEMPLO DE SISTEMA AGROFORESTAL
#ARTICULO DEL PUEBLO 
#EMPEZANDO 2 DE FEBRERO 

# INDICE DE VALOR DE IMPORTANCIA ------------------------------------------

library(ggplot2)
library(dplyr)
library(plotly)


datos <- read.csv("Datos.csv")
head(datos)


datos$CANT <- as.numeric(datos$CANT)

sum(is.na(datos$CANT))

# Calcular el número total de individuos
total_individuos <- sum(datos$CANT, na.rm = TRUE)

# Asegurarse de que el total de individuos sea correcto
print(total_individuos)

# Calcular la abundancia total por especie
abundancia_total <- datos %>%
  group_by(NOM_CO) %>%
  summarise(Abundancia_Total = sum(CANT, na.rm = TRUE)) %>%
  arrange(desc(Abundancia_Total))  # Ordenar de mayor a menor

# Verificar las abundancias por especie
print(abundancia_total)

# Calcular la abundancia relativa
abundancia_relativa <- abundancia_total %>%
  mutate(Abundancia_Relativa = (Abundancia_Total / total_individuos) * 100)

# Mostrar el resultado final
print(abundancia_relativa)


# Datos de ejemplo (puedes modificar estos valores)
especies <- data.frame(
  nombre = c("Annona reticulata L.", "Byrsonima crassifolia (L.) Kunth.", "Cedrela odorata L.", "Chamaedorea tepejilote Liebm.", "Citrus limon (L.) Burm. f.", "Citrus reticulata Blanco", "Citrus sinensis (L.) Osbeck", "Cocos nucifera L.", "Cordia alliodora (Ruiz & Pav.) Oken", "Inga inicuil (Kunth) DC", "Inga vera Willd", "Litchi chinensis Sonn", "Mangifera indica L. 'Manila'", "Mangifera indica L. 'Petacón'", "Manilkara zapota (L.) P.Royen", "Musa acuminata Colla", "Musa paradisiaca L.", "Persea americana Mill.", "Persea schiedeana Nees", "Pouteria sapota (Jacq.) H.E. Moore & Stearn", "Psidium friedrichsthalianum O. Berg.", "Sideroxylon celastrinum (Kunth) T.D.Penn", "Spondias mombin L.", "Spondias purpurea L.", "Tamarindus indica L.", "Theobroma cacao L.", "Theobroma cacao L. var. tigre"),
  no_ha = c(3, 1, 23, 12, 2, 12, 29, 15, 12, 12, 5, 11, 5, 4, 2, 15, 24, 6, 10, 5, 8, 1, 2, 2, 3, 60, 27),
  abr = c(0.965, 0.322, 7.395, 3.859, 0.643, 3.859, 9.325, 4.823, 3.859, 3.859, 1.608, 3.537, 1.608, 1.286, 0.643, 4.823, 7.717, 1.929, 3.215, 1.608, 2.572, 0.322, 0.643, 0.643, 0.965, 19.293, 8.682)
)


# Mapa de calor con colores más intensos y rangos definidos

mapa_calor <- ggplot(especies, aes(x = reorder(nombre, -abr), y = "", fill = abr)) +
  geom_tile(color = "black") +  # Añadir borde negro a cada cuadrante
  scale_fill_gradientn(colors = c("white", "red"), breaks = seq(0, 20, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        legend.key.height = unit(2.5, "cm")) +
  labs(x = "Especie", fill = "AR %")

# Mostrar el mapa de calor
print(mapa_calor)

# ABUNDANCIA RELATIVA -------------------------------------------------------
# Crear el dataframe con los datos proporcionados
datos <- data.frame(
  ESPECIES = c("Theobroma cacao L.", "Citrus sinensis (L.) Osbeck", "Theobroma cacao L. var. tigre", 
               "Musa paradisiaca L.", "Cedrela odorata L.", "Cocos nucifera L.", 
               "Musa acuminata Colla", "Chamaedorea tepejilote Liebm.", "Citrus reticulata Blanco", 
               "Cordia alliodora (Ruiz & Pav.) Oken", "Inga inicuil (Kunth) DC", 
               "Litchi chinensis Sonn", "Persea schiedeana Nees", "Psidium friedrichsthalianum O. Berg.", 
               "Persea americana Mill.", "Inga vera Willd", "Mangifera indica L. 'Manila'", 
               "Pouteria sapota (Jacq.) H.E. Moore & Stearn", "Mangifera indica L. 'Petacón'", 
               "Annona reticulata L.", "Tamarindus indica L.", "Citrus limon (L.) Burm. f.", 
               "Manilkara zapota (L.) P.Royen", "Spondias mombin L.", "Spondias purpurea L.", 
               "Byrsonima crassifolia (L.) Kunth.", "Sideroxylon celastrinum (Kunth) T.D.Penn"),
  NO_HA = c(60, 29, 27, 24, 23, 15, 15, 12, 12, 12, 12, 11, 10, 8, 6, 5, 5, 5, 4, 3, 3, 2, 2, 2, 2, 1, 1),
  ABR = c(19.293, 9.325, 8.682, 7.717, 7.395, 4.823, 4.823, 3.859, 3.859, 3.859, 3.859, 3.537, 3.215, 
          2.572, 1.929, 1.608, 1.608, 1.608, 1.286, 0.965, 0.965, 0.643, 0.643, 0.643, 0.643, 0.322, 0.322)
)

# Ordenar los datos por la columna ABR en orden ascendente
datos <- datos[order(datos$ABR), ]

# Crear el gráfico de barras
library(ggplot2)
ggplot(datos, aes(x = reorder(ESPECIES, ABR), y = ABR)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Para hacerlo en horizontal
  theme_minimal() +
  labs(title = "Abundancia Relativa por Especie", x = "Especie", y = "Abundancia Relativa (ABR)") +
  theme(axis.text.y = element_text(size = 8))




library(ggplot2)

# Crear el gráfico de barras verticales con un gradiente de color
ggplot(datos, aes(x = reorder(ESPECIES, ABR), y = ABR, fill = NO_HA)) +
  geom_bar(stat = "identity", width = 0.8) +  # Ajustar el ancho de las barras
  scale_fill_gradient(low = "lightblue", high = "red", breaks = seq(0, 70, by =5)) +  # Corregir los paréntesis
  coord_flip() + 
  theme_minimal() +
  labs(
    x = "Especies", 
    y = "Abundancia Relativa (ARi)", 
    fill = "Ai"  # Cambiar el nombre de la leyenda
  ) + scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 2))  + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 10),   # Reducir el tamaño del texto de la leyenda
    legend.title = element_text(size = 12),  # Reducir el tamaño del título de la leyenda
    legend.key.size = unit(2.8, "cm"),         # Reducir el tamaño de los cuadros de la leyenda
    legend.key.width = unit(.3, "cm")       # Reducir el ancho de los bloques de color (fill) de la leyenda
  )


# dominancia relativa -----------------------------------------------------

# Crear el dataframe con los datos proporcionados
library(ggplot2)
library(viridis)
library(ggplot2)
library(ggiraphExtra)
library(dplyr)
library(ggimage)

# Datos corregidos (agregando una especie faltante o eliminando la extra)
data <- data.frame(
  ESPECIES = c("Theobroma cacao L.", "Citrus sinensis (L.) Osbeck", "Cedrela odorata L.",
               "Theobroma cacao L. var. tigre", "Cordia alliodora (Ruiz & Pav.) Oken", "Cocos nucifera L.",
               "Musa paradisiaca L.", "Inga inicuil (Kunth) DC", "Persea schiedeana Nees", "Litchi chinensis Sonn",
               "Citrus reticulata Blanco", "Musa acuminata Colla", "Persea americana Mill.", "Chamaedorea tepejilote Liebm.",
               "Psidium friedrichsthalianum O. Berg.", "Mangifera indica L. Manila", "Inga vera Willd",
               "Mangifera indica L. 'Petacón'", "Annona reticulata L.", "Pouteria sapota (Jacq.) H.E. Moore & Stearn",
               "Citrus limon (L.) Burm. f.", "Tamarindus indica L.", "Manilkara zapota (L.) P.Royen",
               "Spondias mombin L.", "Spondias purpurea L.", "Byrsonima crassifolia (L.) Kunth.", 
               "Sideroxylon celastrinum (Kunth) T.D.Penn"),
  DR = c(20.696, 13.287, 13.773, 7.683, 7.041, 5.714, 1.968, 3.355, 3.403, 2.926, 2.008, 1.011, 2.691, 0.590,
         1.742, 1.990, 1.391, 1.414, 1.259, 0.435, 1.310, 0.838, 1.134, 1.134, 0.372, 0.421, 0.411),
  AR = c(19.293, 9.325, 7.395, 8.682, 3.859, 4.823, 7.717, 3.859, 3.215, 3.537, 3.859, 4.823, 1.929, 3.859,
         2.572, 1.608, 1.608, 1.286, 0.965, 1.608, 0.643, 0.965, 0.643, 0.643, 0.322, 0.322),
  IVI = c(19.994, 11.306, 10.584, 8.182, 5.450, 5.269, 4.843, 3.607, 3.309, 3.231, 2.933, 2.917, 2.310, 2.224,
          2.157, 1.799, 1.500, 1.350, 1.112, 1.021, 0.977, 0.901, 0.889, 0.889, 0.508, 0.371, 0.366)
)

length(data$ESPECIES)  # Longitud de la columna de especies
length(data$DR)        # Longitud de la columna DR
length(data$AR)        # Longitud de la columna AR
length(data$IVI)       # Longitud de la columna IVI
# Crear un gráfico de Voronoi usando la función deldir
voronoi <- deldir(data$DR, data$AR)

# Visualizar el gráfico con ggplot2
ggplot() +
  geom_tile(data = voronoi, aes(x = x, y = y, fill = factor(region)), alpha = 0.2) +
  geom_point(data = data, aes(x = DR, y = AR), color = "black", size = 3) +
  geom_text(data = data, aes(x = DR, y = AR, label = ESPECIES), size = 3, hjust = 0, vjust = 0) +
  theme_minimal() +
  labs(title = "Diagrama de Voronoi de las especies", x = "DR", y = "AR") +
  theme(legend.position = "none")