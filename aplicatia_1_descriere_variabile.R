# Instalarea și încărcarea pachetelor necesare
library(tidyverse)
library(moments)
library(psych)

# Citirea datelor
data <- read.csv("Figure.csv")

# Selectarea variabilelor de interes
variables <- c("lnpat", "lngdp")

# Statistici descriptive detaliate
descriptive_stats <- data %>%
  select(all_of(variables)) %>%
  describe()

# Calcularea asimetriei și boltării
skewness_data <- data %>%
  select(all_of(variables)) %>%
  summarise(across(everything(), skewness))

kurtosis_data <- data %>%
  select(all_of(variables)) %>%
  summarise(across(everything(), kurtosis))

# Afișarea rezultatelor
print("Statistici Descriptive pentru lnpat și lngdp:")
print(descriptive_stats)

print("\nAsimetrie:")
print(skewness_data)

print("\nBoltare:")
print(kurtosis_data)

# Histogramă
data %>%
  select(all_of(variables)) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribuția variabilelor lnpat și lngdp", 
       x = "Valoare", 
       y = "Frecvență")

# Boxplot pentru identificarea valorilor extreme
data %>%
  select(all_of(variables)) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Boxplot pentru lnpat și lngdp", 
       x = "Variabilă", 
       y = "Valoare")