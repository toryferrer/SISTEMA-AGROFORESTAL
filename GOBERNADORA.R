#DATOS_PARA_MODELOS_POR_COMPONENTES
#CORRIDAS_INICIALES

DATOS <- read.csv("GOBERNADORA/GOB.CSV")
library(ggplot2)

# MODELOS NO LINEALES PARA HOJAS SECAS --------------------------------------------------------

#POTENCIA

POTENCIA <- nls(PSFH ~ B0 * Dp ^B1, data = DATOS,  
              start = list(B0 = 1, B1 = 1)) 
POTENCIA

summary(POTENCIA)


# Crear valores predichos a partir del modelo ajustado
DATOS$Pred <- predict(POTENCIA)

# Gráfico con ggplot2
ggplot(DATOS, aes(x = Dp, y = PSFH)) +
  geom_point(color = "blue", size = 2) +  # Puntos originales
  geom_line(aes(y = Pred), color = "red", size = 1) +  # Línea ajustada
  labs(title = "Ajuste del modelo no lineal",
       x = "Diámetro promedio (Dp)",
       y = "PSFH") +
  theme_minimal()

#R2AJUSTADA
# Calcular valores observados y predichos
y_obs <- DATOS$PSFH
y_pred <- predict(POTENCIA)

# Suma de cuadrados del error (SSE)
SSE <- sum((y_obs - y_pred)^2)

# Suma de cuadrados totales (SST)
SST <- sum((y_obs - mean(y_obs))^2)

# Pseudo R^2 ajustado
R2_j <- 1 - (SSE / SST)

cat("Pseudo R^2_j:", R2_j, "\n")

#ERROR ESTANDAR

n <- length(y_obs)  # Número de datos
p <- length(coef(POTENCIA))  # Número de parámetros

SEE <- sqrt(SSE / (n - p))

cat("Error estándar de la estimación (SEE):", SEE, "\n")






# MODELOS NO LINEALES PARA RAMAS SECAS ------------------------------------

POTENCIA1 <- nls(PSFT ~ B0 * Dp ^B1, data = DATOS,  
                start = list(B0 = 1, B1 = 1)) 
POTENCIA1

# Crear valores predichos a partir del modelo ajustado
DATOS$Pred1 <- predict(POTENCIA1)

# Gráfico con ggplot2
ggplot(DATOS, aes(x = Dp, y = PSFT)) +
  geom_point(color = "blue", size = 2) +  # Puntos originales
  geom_line(aes(y = Pred1), color = "red", size = 1) +  # Línea ajustada
  labs(title = "Ajuste del modelo no lineal",
       x = "Diámetro promedio (Dp)",
       y = "PSFH") +
  theme_minimal()

