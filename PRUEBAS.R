#EJEMPLO DE SISTEMA AGROFORESTAL
#ARTICULO DEL PUEBLO 
#EMPEZANDO 2 DE FEBRERO 


library(sf)
library(ggplot2)

# Cargar el archivo KML (reemplaza "mi_archivo.kml" por tu archivo)
area_kml <- st_read("AREA.kml")

# Ver información del archivo
print(area_kml)

ggplot() +
  geom_sf(data = area_kml, fill = "lightgreen", color = "darkgreen", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Área del Sistema Agroforestal", subtitle = "Datos cargados desde KML")

# Simulación de puntos de árboles
set.seed(123)
df <- data.frame(
  x = runif(50, min(st_bbox(area_kml)[1]), max(st_bbox(area_kml)[3])),
  y = runif(50, min(st_bbox(area_kml)[2]), max(st_bbox(area_kml)[4])),
  especie = sample(c("Cedro", "Caoba", "Cacao", "Mango"), 50, replace = TRUE)
)

# Convertir a objeto espacial
df_sf <- st_as_sf(df, coords = c("x", "y"), crs = st_crs(area_kml))

# Graficar el área y los puntos
ggplot() +
  geom_sf(data = area_kml, fill = "lightgreen", color = "darkgreen", alpha = 0.5) +
  geom_sf(data = df_sf, aes(color = especie), size = 2) +
  theme_minimal() +
  labs(title = "Distribución de Especies en el Sistema Agroforestal")



