#getwd()
#setwd("C:\\Users\\Lenovo\\Desktop\\trie proj")
# Instalare si activare pachete necesare
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", 
                  "olsrr", "moments", "whitestrap", "ggplot2", "DataCombine",
                  "tseries", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Citirea datelor
data <- read.csv("Figure.csv")

# Model de regresie pentru inovare
model_0 <- lm(inn ~ ind + hum + lnpat + lngdp + Qua + Mea + 
              `The.level.of.science.and.education` + 
              `The.administrative.level`, data = data)
summary(model_0)

# Testarea ipotezelor modelului de regresie:

# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara:
# inn = β₀ + β₁*ind + β₂*hum + β₃*lnpat + β₄*lngdp + β₅*Qua + β₆*Mea + 
#       β₇*science_edu + β₈*admin + ε

# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model_0) > (model_0$rank - 1)

# Ipoteza 3 - Modelul de regresie este corect specificat
# Vom folosi testul RESET
resettest(model_0)

# Ipoteza 4 - Variabilitatea in x este pozitiva
var(data$ind)
var(data$hum)
var(data$lnpat)
var(data$lngdp)
var(data$Qua)
var(data$Mea)
var(data$`The.level.of.science.and.education`)
var(data$`The.administrative.level`)

# Ipoteza 5 - Media reziduurilor este 0
mean(model_0$residuals)

# Ipoteza 6 - Testare multicoliniaritate
vif(model_0)

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(data$ind, model_0$residuals)
cor.test(data$hum, model_0$residuals)
cor.test(data$lnpat, model_0$residuals)
cor.test(data$lngdp, model_0$residuals)
cor.test(data$Qua, model_0$residuals)
cor.test(data$Mea, model_0$residuals)
cor.test(data$`The.level.of.science.and.education`, model_0$residuals)
cor.test(data$`The.administrative.level`, model_0$residuals)

# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model_0)
white_test(model_0)

# Dacă există heteroscedasticitate, corectăm prin WLS
model_WLS <- lm(formula = inn ~ ind + hum + lnpat + lngdp + Qua + Mea + 
                `The.level.of.science.and.education` + 
                `The.administrative.level`, 
                data = data, weights = 1/ind)

# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(model_0$residuals)
dwtest(model_0)
bgtest(model_0)
bgtest(model_0, order=2)

# Ipoteza 10 - Reziduurile sunt normal distribuite
jarque.bera.test(model_0$residuals)
ols_plot_cooksd_bar(model_0)

# Identificarea și eliminarea outlierilor dacă e necesar
cooksd <- cooks.distance(model_0)
plot(cooksd, type="h")
influential <- which(cooksd > 4/length(cooksd))

if(length(influential) > 0) {
  data_clean <- data[-influential, ]
  model_final <- lm(inn ~ ind + hum + lnpat + lngdp + Qua + Mea + 
                    `The.level.of.science.and.education` + 
                    `The.administrative.level`, 
                    data = data_clean)
  summary(model_final)
  
  # Retestăm normalitatea
  jarque.bera.test(model_final$residuals)
}

# Prognoză
# Creăm un nou set de date pentru prognoză cu valori medii
newdata <- data.frame(
  ind = mean(data$ind),
  hum = mean(data$hum),
  lnpat = mean(data$lnpat),
  lngdp = mean(data$lngdp),
  Qua = mean(data$Qua),
  Mea = mean(data$Mea),
  `The.level.of.science.and.education` = median(data$`The.level.of.science.and.education`),
  `The.administrative.level` = median(data$`The.administrative.level`)
)

# Realizăm prognoza
predict(model_0, newdata, interval = "confidence")

# Abordarea 1: Transformare log-log
# Adăugăm o constantă mică pentru a evita log(0)
model_log <- lm(log(inn + 1) ~ ind + hum + lnpat + lngdp + log(Qua + 1) + log(Mea + 1) + 
                  `The.level.of.science.and.education` + 
                  `The.administrative.level`, data = data)

# Testăm noul model
summary(model_log)

# Retestăm specificarea modelului
resettest(model_log)

# Retestăm heteroschedasticitatea
bptest(model_log)
white_test(model_log)

# Retestăm normalitatea
jarque.bera.test(residuals(model_log))

# Abordarea 2: WLS cu pondere alternativă
# Mai întâi rulăm modelul inițial pentru a obține valorile estimate
initial_fit <- fitted(model_0)
weights <- 1/initial_fit^2

model_WLS2 <- lm(inn ~ ind + hum + lnpat + lngdp + Qua + Mea + 
                   `The.level.of.science.and.education` + 
                   `The.administrative.level`, 
                 data = data, 
                 weights = weights)

# Testăm noul model WLS
summary(model_WLS2)

# Retestăm heteroschedasticitatea pentru modelul WLS
bptest(model_WLS2)
white_test(model_WLS2)

# Retestăm normalitatea pentru modelul WLS
jarque.bera.test(residuals(model_WLS2))

# Identificarea outlierilor pentru modelul WLS
cooksd_wls <- cooks.distance(model_WLS2)
plot(cooksd_wls, type="h", main="Cook's distance pentru modelul WLS")
influential_wls <- which(cooksd_wls > 4/length(cooksd_wls))

# Dacă există outlieri, încercăm un model WLS fără ei
if(length(influential_wls) > 0) {
  data_clean_wls <- data[-influential_wls, ]
  model_WLS_clean <- lm(inn ~ ind + hum + lnpat + lngdp + Qua + Mea + 
                          `The.level.of.science.and.education` + 
                          `The.administrative.level`, 
                        data = data_clean_wls,
                        weights = weights[-influential_wls])
  
  summary(model_WLS_clean)
  jarque.bera.test(residuals(model_WLS_clean))
  bptest(model_WLS_clean)
  white_test(model_WLS_clean)
}

# Comparăm R-squared ajustat și criteriile de informare pentru ambele modele
AIC(model_log)
AIC(model_WLS2)
BIC(model_log)
BIC(model_WLS2)


##

# Model nou cu lnpat ca variabilă dependentă
model_pat <- lm(lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
                  `The.level.of.science.and.education` + 
                  `The.administrative.level`, data = data)

# Testăm noul model
summary(model_pat)

# Retestăm ipotezele problematice:
# Specificarea modelului
resettest(model_pat)

# Heteroschedasticitatea
bptest(model_pat)
white_test(model_pat)

# Normalitatea
jarque.bera.test(residuals(model_pat))

# Comparăm cu modelul anterior
AIC(model_pat)
BIC(model_pat)




#nounou

# Model de regresie pentru lnpat
model_pat <- lm(lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
                  `The.level.of.science.and.education` + 
                  `The.administrative.level`, data = data)
summary(model_pat)
data %<>% mutate(uhat = resid(model_pat)) # extragem reziduurile din model

# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara:
# lnpat = β₀ + β₁*ind + β₂*hum + β₃*inn + β₄*lngdp + β₅*Qua + β₆*Mea + 
#         β₇*science_edu + β₈*admin + ε

# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model_pat) > (model_pat$rank - 1)

# Ipoteza 3 - Modelul de regresie este corect specificat
resettest(model_pat)

# Ipoteza 4 - Variabilitatea in x este pozitiva
var(data$ind)
var(data$hum)
var(data$inn)
var(data$lngdp)
var(data$Qua)
var(data$Mea)
var(data$`The.level.of.science.and.education`)
var(data$`The.administrative.level`)

# Ipoteza 5 - Media reziduurilor este 0
mean(model_pat$residuals)

# Ipoteza 6 - Testare multicoliniaritate
vif(model_pat)

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(data$ind, model_pat$residuals)
cor.test(data$hum, model_pat$residuals)
cor.test(data$inn, model_pat$residuals)
cor.test(data$lngdp, model_pat$residuals)
cor.test(data$Qua, model_pat$residuals)
cor.test(data$Mea, model_pat$residuals)
cor.test(data$`The.level.of.science.and.education`, model_pat$residuals)
cor.test(data$`The.administrative.level`, model_pat$residuals)

# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model_pat)
white_test(model_pat)
coeftest(model_pat, vcov. = vcovHC(model_pat, type = "HC1"))

# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(model_pat$residuals)
dwtest(model_pat)
bgtest(model_pat)
bgtest(model_pat, order=2)

# Ipoteza 10 - Reziduurile sunt normal distribuite
jarque.bera.test(model_pat$residuals)
ols_plot_cooksd_bar(model_pat)

# Identificarea outlierilor dacă e necesar
cooksd <- cooks.distance(model_pat)
plot(cooksd, type="h")
influential <- which(cooksd > 4/length(cooksd))

# Criterii informaționale
AIC(model_pat)
BIC(model_pat)


#incercare corectie!!!!


# Cream un nou set de date cu reziduurile modelului initial
data_pat <- data.frame(data, resid_pat = model_pat$residuals)

# Cream variabila lag1 pentru reziduuri
data_pat_1 <- slide(data_pat, Var = "resid_pat", NewVar = "lag1", slideBy = -1)
data_pat_2 <- na.omit(data_pat_1) # eliminam valorile NA

# Reimplementam modelul cu noua variabila lag1 adaugata
model_pat_corrected <- lm(lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
                            `The.level.of.science.and.education` + 
                            `The.administrative.level` + lag1, 
                          data = data_pat_2)

# Testăm noul model
summary(model_pat_corrected)

# Retestăm autocorelarea pe noul model
# ACF 
acf(model_pat_corrected$residuals)

# Durbin Watson 
dwtest(model_pat_corrected)

# Breusch-Godfrey 
bgtest(model_pat_corrected)
bgtest(model_pat_corrected, order = 2)

# Verificăm și celelalte ipoteze pentru noul model
# Heteroschedasticitatea
bptest(model_pat_corrected)
white_test(model_pat_corrected)

# Normalitatea
jarque.bera.test(model_pat_corrected$residuals)

# Comparăm criteriile informaționale
AIC(model_pat)
AIC(model_pat_corrected)
BIC(model_pat)
BIC(model_pat_corrected)




###incercare eliminare observatii
cooksd <- cooks.distance(model_pat)
plot(cooksd, type="h")
influential <- which(cooksd > 4/length(cooksd))
print(influential)

data_clean <- data[-influential, ]

# Reestimăm modelul pe datele curățate
model_pat_clean <- lm(lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
                        `The.level.of.science.and.education` + 
                        `The.administrative.level`, 
                      data = data_clean)

summary(model_pat_clean)

# Retestăm toate ipotezele cheie
resettest(model_pat_clean)  # specificare model
dwtest(model_pat_clean)     # autocorelare
bgtest(model_pat_clean)     # autocorelare
bptest(model_pat_clean)     # heteroschedasticitate
white_test(model_pat_clean) # heteroschedasticitate
jarque.bera.test(residuals(model_pat_clean)) # normalitate



##alta incercare 

# Cream un nou set de date cu reziduurile modelului curatat
data_pat_clean <- data.frame(data_clean, resid_pat_clean = model_pat_clean$residuals)

# Cream variabila lag1 pentru reziduuri
data_pat_clean_1 <- slide(data_pat_clean, Var = "resid_pat_clean", NewVar = "lag1", slideBy = -1)
data_pat_clean_2 <- na.omit(data_pat_clean_1) # eliminam valorile NA

# Reimplementam modelul final cu lag1 pe datele curatate
model_pat_final <- lm(lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
                        `The.level.of.science.and.education` + 
                        `The.administrative.level` + lag1, 
                      data = data_pat_clean_2)

# Testăm modelul final
summary(model_pat_final)

# Verificăm toate ipotezele pentru modelul final
# Autocorelarea
dwtest(model_pat_final)
bgtest(model_pat_final)
bgtest(model_pat_final, order=2)
acf(model_pat_final$residuals)

# Heteroschedasticitatea
bptest(model_pat_final)
white_test(model_pat_final)

# Normalitatea
jarque.bera.test(residuals(model_pat_final))

# Specificarea modelului
resettest(model_pat_final)

# Comparăm cu modelele anterioare
AIC(model_pat)         # modelul original
AIC(model_pat_clean)   # modelul fără outlieri
AIC(model_pat_final)   # modelul final

BIC(model_pat)         # modelul original
BIC(model_pat_clean)   # modelul fără outlieri
BIC(model_pat_final)   # modelul final


##eliminari noi

# Identificăm observațiile influente pentru modelul actual
cooksd_final <- cooks.distance(model_pat_final)
plot(cooksd_final, type="h", main="Distanța Cook pentru modelul final")
influential_final <- which(cooksd_final > 4/length(cooksd_final))
print(influential_final)  # Să vedem care sunt observațiile influente

# Să vedem valorile exacte ale distanței Cook pentru aceste observații
print(cooksd_final[influential_final])

# Creăm un nou set de date fără aceste observații
data_clean_final <- data_pat_clean_2[-influential_final, ]

# Reestimăm modelul
model_pat_final2 <- lm(lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
                         `The.level.of.science.and.education` + 
                         `The.administrative.level` + lag1, 
                       data = data_clean_final)

# Verificăm noul model
summary(model_pat_final2)

# Retestăm normalitatea și heteroschedasticitatea
jarque.bera.test(residuals(model_pat_final2))
white_test(model_pat_final2)
bptest(model_pat_final2)



###cel anterior e aproape bun, mai elimin observatii

# Identificăm observațiile influente pentru modelul actual
cooksd_final2 <- cooks.distance(model_pat_final2)
plot(cooksd_final2, type="h", main="Distanța Cook pentru modelul final 2")
influential_final2 <- which(cooksd_final2 > 4/length(cooksd_final2))
print(influential_final2)  # Să vedem care sunt observațiile influente
print(cooksd_final2[influential_final2])  # Valorile distanței Cook

# Creăm un nou set de date fără aceste observații noi
data_clean_final2 <- data_clean_final[-influential_final2, ]

# Reestimăm modelul
model_pat_final3 <- lm(lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
                         `The.level.of.science.and.education` + 
                         `The.administrative.level` + lag1, 
                       data = data_clean_final2)

# Verificăm noul model
summary(model_pat_final3)

# Testăm ipotezele problematice
jarque.bera.test(residuals(model_pat_final3))
white_test(model_pat_final3)
bptest(model_pat_final3)


# Ipoteza 4: Variabilitatea în variabilele independente este pozitivă
var(data_clean_final2$ind)
var(data_clean_final2$hum)
var(data_clean_final2$inn)
var(data_clean_final2$lngdp)
var(data_clean_final2$Qua)
var(data_clean_final2$Mea)
var(data_clean_final2$`The.level.of.science.and.education`)
var(data_clean_final2$`The.administrative.level`)
var(data_clean_final2$lag1)

# Ipoteza 6: Multicoliniaritate
vif(model_pat_final3)

# Ipoteza 7: Reziduurile nu sunt corelate cu variabilele independente
cor.test(data_clean_final2$ind, residuals(model_pat_final3))
cor.test(data_clean_final2$hum, residuals(model_pat_final3))
cor.test(data_clean_final2$inn, residuals(model_pat_final3))
cor.test(data_clean_final2$lngdp, residuals(model_pat_final3))
cor.test(data_clean_final2$Qua, residuals(model_pat_final3))
cor.test(data_clean_final2$Mea, residuals(model_pat_final3))
cor.test(data_clean_final2$`The.level.of.science.and.education`, residuals(model_pat_final3))
cor.test(data_clean_final2$`The.administrative.level`, residuals(model_pat_final3))
cor.test(data_clean_final2$lag1, residuals(model_pat_final3))

# Ipoteza 8: Homoscedasticitatea reziduurilor (utilizând erori standard robuste)
coeftest(model_pat_final3, vcov = vcovHC(model_pat_final3, type = "HC1"))

# Ipoteza 9: Nonautocorelarea reziduurilor
acf(residuals(model_pat_final3))
dwtest(model_pat_final3)
bgtest(model_pat_final3)



##model pat 2
# Specificarea modelului
resettest(model_pat_final2)

# Media reziduurilor
mean(model_pat_final2$residuals)

# Multicoliniaritate
vif(model_pat_final2)

# Corelarea reziduurilor cu variabilele independente
cor.test(data_clean_final$ind, residuals(model_pat_final2))
cor.test(data_clean_final$hum, residuals(model_pat_final2))
cor.test(data_clean_final$inn, residuals(model_pat_final2))
cor.test(data_clean_final$lngdp, residuals(model_pat_final2))
cor.test(data_clean_final$Qua, residuals(model_pat_final2))
cor.test(data_clean_final$Mea, residuals(model_pat_final2))
cor.test(data_clean_final$`The.level.of.science.and.education`, residuals(model_pat_final2))
cor.test(data_clean_final$`The.administrative.level`, residuals(model_pat_final2))
cor.test(data_clean_final$lag1, residuals(model_pat_final2))

# Autocorelare
dwtest(model_pat_final2)
bgtest(model_pat_final2)
bgtest(model_pat_final2, order=2)
acf(residuals(model_pat_final2))


