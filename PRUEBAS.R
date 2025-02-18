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
library(deldir)
library(ggrepel)
library(gridExtra)

# Datos
especies <- c("Theobroma cacao L.", "Citrus sinensis", "Cedrela odorata", "T. cacao var. tigre", "Cordia alliodora",
              "Cocos nucifera", "Musa paradisiaca", "Inga inicuil", "Persea schiedeana", "Litchi chinensis",
              "Citrus reticulata", "Musa acuminata", "Persea americana", "Chamaedorea tepejilote", "Psidium friedrichsthalianum",
              "Mangifera indica 'Manila'", "Inga vera", "Mangifera indica 'Petacón'", "Annona reticulata", "Pouteria sapota",
              "Citrus limon", "Tamarindus indica", "Manilkara zapota", "Spondias mombin", "Spondias purpurea",
              "Byrsonima crassifolia", "Sideroxylon celastrinum")

dr <- c(20.70, 13.29, 13.77, 7.68, 7.04, 5.71, 1.97, 3.36, 3.40, 2.93,
        2.01, 1.01, 2.69, 0.59, 1.74, 1.99, 1.39, 1.41, 1.26, 0.43,
        1.31, 0.84, 1.13, 1.13, 0.37, 0.42, 0.41)

ar <- c(19.29, 9.32, 7.40, 8.68, 3.86, 4.82, 7.72, 3.86, 3.22, 3.54,
        3.86, 4.82, 1.93, 3.86, 2.57, 1.61, 1.61, 1.29, 0.96, 1.61,
        0.64, 0.96, 0.64, 0.64, 0.64, 0.32, 0.32)

ivi <- c(19.99, 11.31, 10.58, 8.18, 5.45, 5.27, 4.84, 3.61, 3.31, 3.23,
         2.93, 2.92, 2.31, 2.22, 2.16, 1.80, 1.50, 1.35, 1.11, 1.02,
         0.98, 0.90, 0.89, 0.89, 0.51, 0.37, 0.37)

# Crear data frame
data <- data.frame(ID = 1:length(especies), Especie = especies, DR = dr, AR = ar, IVI = ivi)

# Calcular diagrama de Voronoi
voronoi <- deldir(data$DR, data$AR)

# Convertir polígonos en un data frame para ggplot
tiles <- tile.list(voronoi)
vor_df <- do.call(rbind, lapply(seq_along(tiles), function(i) {
  tile <- tiles[[i]]
  data.frame(
    x = tile$x,
    y = tile$y,
    ID = data$ID[i],
    IVI = data$IVI[i]
  )
}))

# Crear la gráfica de Voronoi
voronoi_plot <- ggplot() +
  geom_polygon(data = vor_df, aes(x = x, y = y, group = ID, fill = IVI), color = "black", alpha = 0.8) +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = "IVI", 
                      limits = c(0, 20),
                      breaks = seq(0, 20, by = 2),
                      guide = guide_colorbar(barwidth = 2, barheight = 30)) +
  geom_text(data = data, aes(x = DR, y = AR, label = ID), size = 3, fontface = "bold", color = "black") +
  labs(
       x = "Dominancia Relativa (DRi)", y = "Abundancia Relativa (ARi)") +
  theme_minimal()

# Crear la tabla de leyenda con ID, Especie e IVI
legend_plot <- ggplot(data, aes(y = ID, x = 1, label = paste(ID, Especie, "-", IVI))) +
  geom_text(hjust = 0, size = 3) +
  theme_void() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

# Ajustar la disposición de los gráficos para acercar la leyenda de especies a la leyenda de IVI
grid.arrange(voronoi_plot, legend_plot, ncol = 2, widths = c(5,0))
