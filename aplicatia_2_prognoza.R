# Pachete necesare
library(tidyverse)
library(lmtest)
library(car)
library(tseries)
library(DataCombine)
library(caret)
library(MLmetrics)  # Adăugăm acest pachet pentru funcții de evaluare

# Citirea datelor
data <- read.csv("Figure.csv")

# Verificăm dacă sunt valori lipsă
sum(is.na(data))

# Înlocuim valorile lipsă cu media, dacă este cazul
data <- data %>%
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))))

# Împărțirea datelor în set de antrenare și set de testare
set.seed(123)
training.samples <- data$lnpat %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

# Model de regresie pentru setul de antrenare
model_0 <- lm(lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
                `The.level.of.science.and.education` + 
                `The.administrative.level`, data = train.data) 

# Predicția pe setul de testare
y_pred <- predict(model_0, newdata = test.data)

# Indicatori de acuratețe
# RMSE
rmse_value <- RMSE(y_pred, test.data$lnpat)
print(paste("RMSE:", rmse_value))

# MAE
mae_value <- MAE(y_pred, test.data$lnpat)
print(paste("MAE:", mae_value))

# MSE (calculat manual)
mse_value <- mean((y_pred - test.data$lnpat)^2)
print(paste("MSE:", mse_value))

# MAPE
mape_value <- MAPE(y_pred, test.data$lnpat)
print(paste("MAPE:", mape_value))

# Prognoză pentru câteva scenarii Out of sample
out_of_sample <- data.frame(
  ind = c(0.3, 0.4, 0.5),
  hum = c(0.05, 0.1, 0.15),
  inn = c(1, 3, 5),
  lngdp = c(11.0, 11.5, 12.0),
  Qua = c(5, 10, 15),
  Mea = c(0, 50, 100),
  `The.level.of.science.and.education` = c(0, 1, 0),
  `The.administrative.level` = c(0, 1, 1)
)

# Prognoza
y_pred_outsample <- predict(model_0, newdata = out_of_sample)
out_of_sample_results <- data.frame(
  out_of_sample, 
  Prognoza_brevete_licențiate = exp(y_pred_outsample)
)

# Afișare rezultate prognoză
print(out_of_sample_results)

# Model cu variabilă dummy
model_1 <- lm(lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
                `The.level.of.science.and.education` * 
                `The.administrative.level`, data = data)
summary(model_1)

# Scenariu de prognoză cu variabilă dummy
prognoza_dummy <- data.frame(
  ind = 0.4,
  hum = 0.1,
  inn = 3,
  lngdp = 11.5,
  Qua = 10,
  Mea = 50,
  `The.level.of.science.and.education` = 1,
  `The.administrative.level` = 1
)

# Prognoza pentru scenariul cu variabilă dummy
y_pred_scenariu <- predict(model_1, newdata = prognoza_dummy)
print("Prognoza pentru scenariul cu variabilă dummy:")
print(exp(y_pred_scenariu))

# Model cu termen de interacțiune
model_2 <- lm(lnpat ~ ind + hum + inn * lngdp + Qua + Mea + 
                `The.level.of.science.and.education` + 
                `The.administrative.level`, data = data)
summary(model_2)

# Verificare multicoliniaritate
print("VIF pentru modelul cu termen de interacțiune:")
print(vif(model_2))