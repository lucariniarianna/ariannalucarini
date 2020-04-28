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


# day 2

# scarico e/o richiamo i pacchetti che verranno utilizzati
install.packages("raster")
library(raster)

#setto la directory
setwd("~/Documents/lab")
load("/Users/ariannalucarini/Documents/lab/Teleril.RData")

ls()
#importare file all'interno di R
brick("p224r63_1988_masked.grd")
p224r63_1988 <- brick("p224r63_1988_masked.grd")
#plot dell'immagine
plot(p224r63_1988)

#plot di tutte e 4 le bande
#par ci peremette di utilizzare a blocchi la nostra finestra
par(mfrow=c(2,2))

#blue
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_1988$B1_sre,col=clb)

#green
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_1988$B2_sre,col=clg)

#red
clr <- colorRampPalette(c("dark red","red","pink"))(100)
plot(p224r63_1988$B3_sre,col=clr)

#near infrared
clnir <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_1988$B4_sre,col=clnir)
dev.off()

plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

#Exercise : plot the image the nir on the "r" componment in the RGB space
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

#Plot dell'immagine 2011 e 1988 per notare le differenze
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

#titolo grafico
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin",main="2011")

#la parte agricola è molto più sviluppata rispetto al 1988 (le piante che riflettono il nir sono rosse mentre il suolo agricolo avrà un altro colore
dev.off()

#spectral index
#dvi1988=nir1988-red1988
dvi1988 <- p224r63_1988$B4_sre-p224r63_1988$B3_sre
# se la pianta è sana il valore sarà alto
plot(dvi1988)

#spectral index of 2011
dvi2011 <- p224r63_2011$B4_sre-p224r63_2011$B3_sre
plot(dvi2011)
cldvi <- colorRampPalette(c("light blue", "light green", "green")) (100)
plot(dvi2011, col=cldvi)

#differenza nel tempo dei due indici = analisi multitemporale
difdvi <- dvi2011-dvi1988
plot(difdvi)
cldifdvi <- colorRampPalette(c("red","white","blu"))(100)
plot(difdvi, col= cldifdvi)

#visualize output
#multiframe 1988rgb,2011rgb,difdvi
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)

#changing the grain or resolution
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)
p224r63_2011
p224r63_2011lr

par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

#lower resoltuzion
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
#original 30m -> resampled 1500m

par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

dv2011lr50 <- p224r63_2011lr50$B$_sre-p224r63_2011lr50$B4_sre
plot(dv2011lr50)


#dvi1988 low resolution
p224r63_1988lr50 <- aggregate(p224r63_2011, fact=50)
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
#differenza dvi corretta
difdvilr50 <- dvi2011lr50 - dvi1988lr50
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50,col=cldifdvi)

#multiframe
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)
 

#setto la directory
setwd("~/Documents/lab")
load("/Users/ariannalucarini/Documents/lab/Teleril.RData")
# controllo i dati che sono all'interno 
ls()

head (Tesi)
attach(Tesi)
summary(Tesi)

Library (spatstat)
ls()
plot(dT)
points(Tesippp, col ="green")

head(Tesi)
marks(Tesippp) <- Tesi$Species_richness
interpol <- Smooth(Tesippp)
plot(interpol)
points(Tesippp, col="green")



library(rgdal)
sanmarino <- readOGR("San_Marino.shp")

plot(sanmarino)
plot(interpol, add=T)
points(Tesippp, col="green")
plot(sanmarino, add=T)


#Esercizio : multiframe della densità e dell'interpolazione 

par(mfrow=c(2,1))

plot(Dt. main="Density of points")
points(Tesippp, col="green")
plot(interpol, add=T)
points(Tesippp, col="green")

#Esercizio : multiframe della densità e dell'interpolazione uno accanto all'altro
par(mfrow=c(1,2))

plot(Dt. main="Density of points")
points(Tesippp, col="green")
plot(interpol, add=T)
points(Tesippp, col="green")




