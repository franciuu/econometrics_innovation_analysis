# Citirea datelor din fișierul CSV
data <- read.csv("Figure.csv")

# Modelul de regresie simplă: lnpat = beta0 + beta1 * lngdp + u
model <- lm(lnpat ~ lngdp, data = data)

# Afișarea rezultatelor modelului
summary(model)

# Testarea validității modelului (testul F)
# H0: beta1 = 0 (modelul nu este valid)
# H1: beta1 ≠ 0 (modelul este valid)
# Dacă p-value < 0.05, respingem H0 și considerăm modelul valid
print(paste("P-value pentru testul F:", summary(model)$fstatistic[1]))

# Verificarea semnificației parametrilor (testul t)
# H0: beta0 = 0 sau beta1 = 0 (parametrul nu este semnificativ)
# H1: beta0 ≠ 0 sau beta1 ≠ 0 (parametrul este semnificativ)
# Dacă p-value < 0.05, respingem H0 și considerăm parametrul semnificativ
print(paste("P-value pentru testul t (intercept):", summary(model)$coefficients[1,4]))
print(paste("P-value pentru testul t (lngdp):", summary(model)$coefficients[2,4]))

# Indicatorii de bonitate (R-squared și R-squared ajustat)
print(paste("R-squared:", round(summary(model)$r.squared, 3)))
print(paste("R-squared ajustat:", round(summary(model)$adj.r.squared, 3)))


install.packages("stargazer")
library(stargazer)

# Creați tabelul cu rezultatele regresiei
stargazer(model, type = "html", out = "rezultate_regresie.html", title = "Rezultatele modelului de regresie simplă")