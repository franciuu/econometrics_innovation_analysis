#se pune dupa modelul de regresie aplicatia_1.R
# Crearea unui nou set de date pentru prognoză
newdata <- data.frame(lngdp = seq(min(data$lngdp), max(data$lngdp), length.out = 100))

# Realizarea prognozei utilizând modelul estimat anterior
prognoza <- predict(model, newdata, interval = "confidence")

# Convertirea rezultatelor din logaritm în valori reale
newdata$lnpat_previzionat <- prognoza[,1]
newdata$lnpat_lower <- prognoza[,2]
newdata$lnpat_upper <- prognoza[,3]
newdata$pat_previzionat <- exp(newdata$lnpat_previzionat)
newdata$pat_lower <- exp(newdata$lnpat_lower)
newdata$pat_upper <- exp(newdata$lnpat_upper)

# Afișarea rezultatelor
head(newdata)

# Reprezentarea grafică a prognozei
library(ggplot2)

ggplot(newdata, aes(x = lngdp)) +
  geom_line(aes(y = pat_previzionat), color = "blue") +
  geom_ribbon(aes(ymin = pat_lower, ymax = pat_upper), alpha = 0.2) +
  geom_point(data = data, aes(x = lngdp, y = exp(lnpat)), color = "red") +
  labs(x = "Logaritmul PIB-ului pe cap de locuitor (lngdp)",
       y = "Numărul de brevete licențiate",
       title = "Prognoza numărului de brevete licențiate în funcție de PIB-ul pe cap de locuitor") +
  theme_minimal()