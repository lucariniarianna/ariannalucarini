# CODICE R PER ANALISI DI IMMAGINI SATELLITARI

# scarico e/o richiamo i pacchetti che verranno utilizzati
install.packages("raster")
library(raster)

#setto la directory
setwd("~/Documents/lab")

#la funzione brick prende un immagine satellitare all'interno di una cartella e le da un nome
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#plot iniziale per estrarre dati base (riflettanze ecc...)
plot(p224r63_2011) # si nota un paesaggio in varie bande (b1...b7 ognuno con una lunghezza d'onda diversa)
