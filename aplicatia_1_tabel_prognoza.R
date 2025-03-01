# Instalarea și încărcarea pachetului knitr (dacă nu este deja instalat)
install.packages("knitr")
library(knitr)

# Selectarea unui subset de date pentru tabel
tabel_prognoza <- newdata[seq(1, nrow(newdata), by = 10),]

tabel_html <- kable(tabel_prognoza[,c("lngdp", "pat_previzionat", "pat_lower", "pat_upper")], 
                    col.names = c("Logaritmul PIB-ului pe cap de locuitor", "Numărul previzionat de brevete", 
                                  "Limita inferioară", "Limita superioară"),
                    caption = "Prognoza numărului de brevete licențiate în funcție de PIB-ul pe cap de locuitor",
                    digits = 2,
                    format = "html")

# Salvarea tabelului într-un fișier HTML
cat(tabel_html, file = "tabel_prognoza.html")