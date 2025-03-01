# Instalare si activare pachete
library(tidyverse)
library(lmtest)
library(car)
library(tseries)
library(olsrr)
library(moments)
library(whitestrap)
library(DataCombine)

# Citirea datelor
data <- read.csv("Figure.csv")

# Model inițial
model2 <- lm(lnpat ~ lngdp + ind + hum + inn + Qua + Mea + 
             `The.level.of.science.and.education` + 
             `The.administrative.level`, data = data)

# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, poate fi scris ca o functie liniara

# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model2) > (model2$rank - 1)

# Ipoteza 3 - Modelul de regresie este corect specificat
resettest(model2)

# Ipoteza 4 - Variabilitatea in x este pozitiva
var(data$lngdp)
var(data$ind)
var(data$hum)
var(data$inn)
var(data$Qua)
var(data$Mea)

# Ipoteza 5 - Media reziduurilor este 0
mean(model2$residuals)

# Ipoteza 6 - Testare multicoliniaritate
vif(model2)

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(data$lngdp, model2$residuals)
cor.test(data$ind, model2$residuals)
cor.test(data$hum, model2$residuals)
cor.test(data$inn, model2$residuals)
cor.test(data$Qua, model2$residuals)
cor.test(data$Mea, model2$residuals)

# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model2)
white_test(model2)

# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(model2$residuals)
dwtest(model2)
bgtest(model2)
bgtest(model2, order=2)

# Ipoteza 10 - Reziduurile sunt normal distribuite
jarque.bera.test(model2$residuals)
ols_plot_cooksd_bar(model2)