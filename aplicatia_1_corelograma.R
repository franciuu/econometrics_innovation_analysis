# Citirea datelor din fișierul CSV
data <- read.csv("Figure.csv")

# Crearea corelogramei
par(mfrow = c(1, 2))  # Împarte fereastra grafică în două părți

# Diagrama de dispersie (scatterplot)
plot(data$lnpat, data$lngdp, main = "Corelograma lnpat vs lngdp",
     xlab = "lnpat", ylab = "lngdp", pch = 19)

# Calcularea coeficientului de corelație Pearson
cor_coef <- cor(data$lnpat, data$lngdp)

# Afișarea coeficientului de corelație
mtext(paste("Coeficientul de corelație Pearson:", round(cor_coef, 2)), side = 3)