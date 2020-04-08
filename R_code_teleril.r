# CODICE R PER ANALISI DI IMMAGINI SATELLITARI 07/04/20

# scarico e/o richiamo i pacchetti che verranno utilizzati
install.packages("raster")
library(raster)

#setto la directory
setwd("~/Documents/lab")

#la funzione brick prende un immagine satellitare all'interno di una cartella e le da un nome
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#plot iniziale per estrarre dati base (riflettanze ecc...)
plot(p224r63_2011) # si nota un paesaggio in varie bande (b1...b7 ognuno con una lunghezza d'onda diversa)

# 08/04/20

#setto la directory
setwd("~/Documents/lab")

#Richiamo i dati 
load("~/Documents/lab/.RData")
ls() #[1] "covid_agg"    "p224r63"      "p224r63_2011"

#richiamo la libreria
library(raster)

plot(p224r63_2011) #plot delle singole bande dell'immagine satellitare
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# cambiamo la colorazione da bianco a nero
cl<-colorRampPalette(c("black","grey","light grey"))(100) #il colore grigio chiaro definisce la riflettanza maggiore
plot(p224r63_2011,col=cl)
cllow<-colorRampPalette(c("black","grey","light grey"))(5) #esperimento per vedere la riflettanza usando solo 5 gamme di colore

#plottiamo l'immagine con la gamma del blu
names (p224r63_2011) #per vedere i nomi delle bande che stiamo utilizzando
#[1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"

clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_2011$B1_sre,col=clb)
# attach(dataframe) non funziona con la funzione raster
# simbolo che lega la colonna (la banda) al dataset (immagine satellitare) -> $
# abbiamo prodotto un'immagine nella prima banda con i colori associati alla riflettanza di questa banda

#Esercizio : plottare la banda dell'infrarosso vicino con colorramppalette che varia dal rosso, all'arancione al giallo
clnir <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_2011$B4_sre,col=clnir)
#ci sarà molta vegetazione perchè le piante riflettono molto l'infrarosso vicino

#plot di tutte e 4 le bande
#par ci peremette di utilizzare a blocchi la nostra finestra
par(mfrow=c(2,2))

#blue
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_2011$B1_sre,col=clb)

#green
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_2011$B2_sre,col=clg)

#red
clr <- colorRampPalette(c("dark red","red","pink"))(100)
plot(p224r63_2011$B3_sre,col=clr)

#near infrared
clnir <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_2011$B4_sre,col=clnir)

#montiamo le bande insieme in modo tale da poterle vedere in modo naturale
dev.off() #device=finestra grafica -> chiude le immagini appena plottate
#natural colours 
# 3 componenti all'interno del computer : R G B
#in ognuno di questi tre componenti dobbiamo montare un'immagine
#3 bande: R= red ; G= green; B=blue
plotRGB(p224r63_2011,r=3,g=2,b=1)

#stretch-> per aumentare la gamma dei colori, in questo caso usiamo lo strech lineare
plotRGB(p224r63_2011,r=3,g=2,b=1,stretch="Lin")

#utilizziamo il nir per distinguere meglio la vegetazione (Dobbiamo però metterlo al posto di un altro perchè si possono usare soll 3 alla volta)
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin") #false colours

#salvare un'immagine in pdf o in #png("primografico.png") i grafici sono meno pesanti
pdf("primografico.pdf")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
dev.off()

#confrontare le due immagini in un grafico
#multiframe
par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=3,g=2,b=1,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
dev.off()

# nir nella componente red
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")

#Esercizio : nir nella componente green
plotRGB(p224r63_2011,r=5,g=4,b=3,stretch="Lin")

#nir nella componente blu
plotRGB(p224r63_2011,r=6,g=5,b=4,stretch="Lin")





