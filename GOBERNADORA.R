#DATOS_PARA_MODELOS_POR_COMPONENTES
#CORRIDAS_INICIALES

DATOS <- read.csv("GOBERNADORA/GOB.CSV")
library(ggplot2)
library(lmtest)
library(car)
# MODELOS NO LINEALES PARA HOJAS SECAS --------------------------------------------------------

#POTENCIA

POTENCIA <- nls(PSFT ~ B0 * Dp^B1, data = DATOS,  
              start = list(B0 = 1, B1 = 1)) 

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
y_obs <- DATOS$PSFT
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

#COEFICIENTE DE VARIACION 
# Extraer los residuos del modelo
residuos <- residuals(POTENCIA)

# Calcular la desviación estándar de los residuos
error_estandar <- sd(residuos)

# Media de la variable dependiente (PSFH)
media_y <- mean(DATOS$PSFT)

# Coeficiente de variación (CV) en porcentaje
CV <- (error_estandar / media_y) * 100

# Mostrar resultado
CV




#distribucion normal
shapiro_test <- shapiro.test(residuos)

print(shapiro_test)

#autocorrelacion

DATOS$residuos <- residuals(POTENCIA)  # Guardar residuos

dw_model <- lm(residuos ~ 1, data = DATOS)
dwtest(dw_model)


#heterocedasticidad
residuos <- resid(POTENCIA)
ajustados <- fitted(POTENCIA)

# Crear un modelo auxiliar con los residuos al cuadrado
aux_model <- lm(residuos^2 ~ ajustados)

# Aplicar la prueba de Breusch-Pagan
bp_test <- bptest(aux_model)

# Mostrar resultados
print(bp_test)


# MODELO LINEAL PARA HOJAS ------------------------------------------------

MODELO <- lm(PSFT ~  DPH, data = DATOS)
summary(MODELO)

breu <- bptest(MODELO)
print(breu)

dwtest(MODELO)

shapiro.test(residuals(MODELO))


SIEE <- sqrt(sum(residuals(MODELO)^2) / (length(residuals(MODELO)) - length(coef(MODELO))))  # Error estándar de la estimación
media_obs <- mean(DATOS$PSFT)  # Media de la variable dependiente
CV <- (SIEE / media_obs) * 100
CV
