# Pachete necesare pentru analiza de date panel
library(tidyverse)
library(plm)
library(lmtest)
library(car)
library(tseries)
library(readxl)

# 1. Stadiul actual al cunoașterii
# NOTĂ: A se completa în raportul de cercetare cu minimum 5 referințe științifice
# care utilizează modele panel în studiul inovației tehnologice

# 2. Extinderea bazei de date
# Baza de date Figure2.csv conține date panel pentru 141 de regiuni 
# pe o perioadă de 5 ani (2019-2023)
data <- read.csv("Figure2.csv")

library(psych)
describe(data[c("lnpat", "ind", "hum", "inn", "lngdp", "Qua", "Mea", 
                "The.level.of.science.and.education", "The.administrative.level")])

# Statistici descriptive pentru întregul set de date
summary_stats <- describe(data[c("lnpat", "ind", "hum", "inn", "lngdp", "Qua", "Mea", 
               "The.level.of.science.and.education", "The.administrative.level")])

# Salvare în CSV
write.csv(summary_stats, "statistici_descriptive.csv")

# Afișare în consolă și salvare în txt
sink("statistici_descriptive.txt")
print(summary_stats)
sink()

# Convertim Year și Ccode la factor pentru a evita probleme
data$Year <- factor(data$Year)
data$Ccode <- factor(data$Ccode)

# Declararea setului de date de tip panel
pd.df <- pdata.frame(data, index = c("Ccode", "Year"), drop.index = TRUE)

# 2. Explorarea heterogeneitatii în secțiunea transversală
ggplot(data, aes(x = Ccode, y = lnpat)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Heterogeneitate în rândul regiunilor', 
       x = 'Cod Regiune', 
       y = 'Log Brevete')

# Explorarea heterogeneitatii în secțiunea temporală 
ggplot(data, aes(x = Year, y = lnpat)) +
  geom_boxplot() +
  labs(title = 'Heterogeneitate în timp', 
       x = 'An', 
       y = 'Log Brevete')

# 3. Testarea modelelor panel
# Formulă panel
formula_panel <- formula(
  lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
    `The.level.of.science.and.education` + `The.administrative.level`
)

# Model Pooled OLS
ols <- plm(formula_panel, data = pd.df, model = "pooling")

# Model cu Efecte Fixe (Fixed Effects)
fe <- plm(formula_panel, data = pd.df, model = "within")

# Model cu Efecte Aleatorii (Random Effects)
re <- plm(formula_panel, data = pd.df, model = "random")

# Testarea selecției modelului
# Test F pentru efecte individuale
f_test <- pFtest(fe, ols)

# Testul Hausman pentru alegerea între efecte fixe și aleatorii
hausman_test <- phtest(fe, re)

# Test LM pentru efecte aleatorii
plmtest(ols, type = c("bp"))

# Test pentru efecte fixe în timp
formula_time <- formula(
  lnpat ~ ind + hum + inn + lngdp + Qua + Mea + 
    `The.level.of.science.and.education` + `The.administrative.level` + 
    factor(Year)
)

fixed.time <- plm(formula_time, 
                  data = data,  
                  model = "within")

# Testarea semnificației efectelor în timp
pFtest(fixed.time, fe)
plmtest(fe, c('time'), type = 'bp')

# 4. Estimarea parametrilor modelului
# Vom folosi modelul cu efecte fixe cu corecție robustă
model_final <- summary(fe, vcov = vcovHC(fe, type = "HC0"))

# Teste suplimentare
# Dependență transversală
pcdtest(fe, test = c("lm"))
pcdtest(fe, test = c("cd"))

# Test de autocorelare
pbgtest(fe, order = 1)

# Test de heteroscedasticitate
bptest(formula_panel, 
       data = data, 
       studentize = FALSE)

# Verificare multicoliniaritate
vif(lm(formula_panel, data = data))

# Test de normalitate a reziduurilor
jarque.bera.test(residuals(fe))

# Extrage efectele fixe
alpha_i <- fixef(fe)

# Afișează primele câteva efecte fixe
head(alpha_i)

# Salvează într-un CSV pentru toate regiunile
write.csv(alpha_i, "efecte_fixe.csv")

# 5. Interpretare rezultate
# Salvare rezultate detaliate
sink("rezultate_panel_data.txt")
print("Test F pentru efecte individuale:")
print(f_test)
print("\nTestul Hausman:")
print(hausman_test)
print("\nModel final (efecte fixe cu corecție robustă):")
print(model_final)
sink()

# Vizualizări suplimentare pentru efecte fixe
# Grafic pentru reziduuri
par(mfrow=c(2,2))
plot(residuals(fe), main="Reziduuri")
hist(residuals(fe), main="Histogramă reziduuri")
qqnorm(residuals(fe), main="Q-Q Plot reziduuri")
boxplot(residuals(fe), main="Boxplot reziduuri")
par(mfrow=c(1,1))

# Opțional: Salvare grafice
png("diagnostics_plot.png")
par(mfrow=c(2,2))
plot(residuals(fe), main="Reziduuri")
hist(residuals(fe), main="Histogramă reziduuri")
qqnorm(residuals(fe), main="Q-Q Plot reziduuri")
boxplot(residuals(fe), main="Boxplot reziduuri")
par(mfrow=c(1,1))
dev.off()
# Exportare rezultate pentru raport
write.csv(model_final$coefficients, "coeficienti_panel.csv")

# Salvare coeficienți într-un fișier text formatat
sink("coeficienti_panel.txt")
cat("Coeficienți Model Panel cu Efecte Fixe\n")
cat("======================================\n\n")
print(model_final)
cat("\n\nInterpretare:\n")
cat("* Valorile cu semnificație statistică sunt marcate cu '*', '**', '***'\n")
cat("* '***' indică semnificație la nivel de 0.001\n")
cat("* '**' indică semnificație la nivel de 0.01\n")
cat("* '*' indică semnificație la nivel de 0.05\n")
sink()