
DATOS <- read.csv("GOBERNADORA/DATOS.CSV")
library(ggplot2)
library(lmtest)
library(car)
library(gridExtra)

# MODELO PARA RAMAS -------------------------------------------------------

modelo <- nls(log(PSFT) ~  b0 + b1* log(Dp^2*h), 
              data = DATOS, 
              start = list(b0 = 0.01, b1 = 0.05))
summary(modelo)

# Predicciones
pred <- predict(modelo)

# Residuos
residuos <- residuals(modelo)

# Suma de cuadrados del residuo (RSS)
RSS <- sum(residuos^2)

# Suma total de cuadrados (TSS)
TSS <- sum((log(DATOS$PSFH) - mean(log(DATOS$PSFH)))^2)

# R²
R2 <- 1 - (RSS / TSS)

# Número de observaciones y parámetros
n <- nrow(DATOS)
p <- 2  # b1 y b2 (b0 es el intercepto)

# R² ajustado
R2_aj <- 1 - ((1 - R2) * (n - 1)) / (n - p - 1)
R2_aj

# RMSE
RMSE <- sqrt(mean(residuos^2))

# Media del log(PSFH)
media_y <- mean(log(DATOS$PSFH))

# CV (%)
CV <- (RMSE / media_y) * 100
CV

dwtest(residuos ~ 1)

shapiro.test(residuos)

bptest(residuos ~ fitted(modelo))

# Gráfico de residuos vs predichos
plot(fitted(modelo), residuos, 
     xlab = "Valores ajustados", ylab = "Residuos", 
     main = "Residuos vs Ajustados")
abline(h = 0, col = "red")

# Histograma y QQ plot
par(mfrow = c(1, 2))
hist(residuos, main = "Histograma de residuos", col = "lightblue", xlab = "Residuos")
qqnorm(residuos); qqline(residuos, col = "red")

# Autocorrelación
acf(residuos, main = "Autocorrelación de residuos", col = "blue")

# Calcular estimados en escala original
DATOS$Estimados <- exp(predict(modelo))

# Graficar
library(ggplot2)
ggplot(DATOS, aes(x = Dp)) +
  geom_point(aes(y = PSFT, color = "Observado"), size = 2, alpha = 0.7) +
  geom_line(aes(y = Estimados, color = "Estimado"), size = 1) +
  labs(title = "Valores Observados y Estimados de PSFT vs Dp",
       x = "Diámetro (Dp)",
       y = "PSFT",
       color = "Tipo de valor") +
  scale_color_manual(values = c("Observado" = "darkblue", "Estimado" = "orange")) +
  theme_minimal()

# Calcular estimados en escala original
DATOS$Estimados <- exp(predict(modelo))

# Graficar puntos observados y estimados
library(ggplot2)
ggplot(DATOS, aes(x = Dp)) +
  geom_point(aes(y = PSFT, color = "Observado"), size = 2.5, alpha = 0.7) +
  geom_point(aes(y = Estimados, color = "Estimado"), size = 2.5, shape = 17, alpha = 0.7) +
  labs(title = "Valores Observados y Estimados de PSFT vs Dp",
       x = "Diámetro (Dp)",
       y = "PSFT",
       color = "Tipo de valor") +
  scale_color_manual(values = c("Observado" = "blue", "Estimado" = "red")) +
  theme_minimal()


