# Importarea pachetelor necesare
library(tidyverse)
library(stargazer)
library(car)

# Citirea datelor
data <- read.csv("Figure.csv")

# Model regresie multiplă
model2 <- lm(lnpat ~ lngdp + ind + hum + inn + Qua + Mea + 
            `The.level.of.science.and.education` + 
            `The.administrative.level`, data = data)

# Afișare rezumat model
summary(model2)

# Testarea validității modelului (F test)
# Deja inclus în summary()

# Testarea multicoliniarității 
vif(model2)

# Bonitatea modelului
# R-squared și R-squared ajustat sunt în summary()

# Calculare componente R-squared manual
y <- data$lnpat
yhat <- fitted(model2)
ehat <- resid(model2)

SSE <- sum((yhat - mean(y))^2) 
SSR <- sum(ehat^2)
SST <- SSE + SSR

# Număr observații și parametri
n <- nobs(model2)
k <- model2$rank - 1

# Grade de libertate
df_SSR <- n - k - 1
df_SST <- n - 1

# R-squared și R-squared ajustat manual
R_squared <- SSE/SST
adj_R_squared <- 1 - (SSR/df_SSR)/(SST/df_SST)

# Afișare rezultate
print("R-squared manual:")
print(R_squared)
print("R-squared ajustat manual:")
print(adj_R_squared)

stargazer(model2, 
          title = "Rezultatele modelului de regresie multiplă",
          type = "html",
          covariate.labels = c("PIB pe cap de locuitor (log)", 
                              "Pondere industrie", 
                              "Densitate populație educată",
                              "Brevete tehnologii inteligente",
                              "Forța politicilor",
                              "Deschiderea proiectelor",
                              "Nivel științific și educațional",
                              "Nivel administrativ"),
          dep.var.labels = "Număr total brevete (log)",
          digits = 3,
          style = "default",
          notes = "Niveluri de semnificație: *** p<0.01, ** p<0.05, * p<0.1",
          notes.align = "r",
          out = "rezultate_regresie.html")