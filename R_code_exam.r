# R_code_exam.r

1. R_code_first.r   
2. R_code_spatial.r  [1 e 2] 
3. R_code_point_pattern   
4. R_code_teleril.r   
5. R_code_landcover.r   
6. R_code_multitemp.r   
7. R_code_multitemp_NO2.r   
8. R_code_snow.r   
9. R_code_patches.r   

# https://land.copernicus.vgt.vito.be/PDF/portal/Application.html

### 1. R code first

#Prima di tutto occorre installare la nuova libreria, attraverso i pacchetti, e richiamarla (ogni volta che vogliamo richiamare
#qualcosa che si trova esternamente a R occorrei usare le virgolette
install.packages("sp") 
library(sp) #sp ci fornisce classi e metodi per effettuare un'analisi spaziale

#data serve per richiamare i dati contenuti nella libreria; meuse è il nostro dataset sulle concentrazioni di metalli pesanti
#all'interno del terreno e una serie di variabili del suolo
data("meuse")

#scrivendo solo il nome del dataset siamo capace di vedere i dati contenuti in una tabella
meuse

#con il comando head possiamo vedere le prime 6 righe del nostro dataset
head(meuse)

#grazie al comando names siamo capace di vedere le variabili del nostro dataset
names(meuse)

#summary ci permette di visualizzare gli indici statistici più significativi rispetto tutti i dati del nostro dataset
summary(meuse)

#pairs è capace di creare un grafico che metta in correlazione le variabili del dataset
pairs (meuse)

# grazie a ~ possiamo creare un grafico, sempre con la funzione pairs, ma tenendo in considerazione solo le variabili da noi scelte
pairs(~ cadmium + copper + lead , data = meuse)
pairs(~ cadmium + copper + lead + zin , data = meuse)
#un modo alternativo al precedente è quello di scrivere quali righe della colonna sono interessate dalle variabili prese da noi in 
#considerazione
pairs(meuse[,3:6])
#con il comando col possiamo cambiare il colore di visualizzano del grafico
pairs(meuse[,3:6],col="red")
#con il comando pch possiamo scegliere i simboli da visualizzare
pairs(meuse[,3:6],col="red", pch=19)
#con il comando cex possiamo decidere la grandezza del testo
pairs(meuse[,3:6],col="red", pch=19,cex=3)
#con il comando main possiamo impostare un titolo al grafico
pairs(meuse[,3:6],col="red", pch=19,cex=3,main="Primo pairs")
pairs(meuse[,3:7],col="red", pch=19,cex=3,main="Primo pairs")

#andiamo a riprendere delle funzioni esterne
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r1=cor(x,y,use="pairwise.complete.obs")
    r <- abs(cor(x, y,use="pairwise.complete.obs"))

 

    txt <- format(c(r1, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}

 

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = 1, ...)
}

 


panel.histograms <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

#queste funzioni esterne servono per creare dei grafici esteticamente migliori
pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)
pairs(meuse[,3:6],lower.panel=panel.smoothing,upper.panel=panel.correlations,diag.panel = panel.histograms)


###FUNZIONE PLOT
plot(meuse$cadmium,meuse$copper)
attach(meuse)
plot(cadmium,copper, pch=17, col="green", main="primo.plot", xlab="cadmio",ylab="rame")



########################################################################################

### R spatial : funzioni spaziali in Ecologia del paesaggio


#richiamo il pacchetto library()
library(sp)
#richiamo dati
data(meuse)
meuse
head(meuse)
#plot cadmium e lead
#alleghiamo il dataframe
attach(meuse)
plot(cadmium,lead,col="red",pch=19,cex=2)
#esercizio n.1 fare un plot di rame e zinco con carattere "triangolo" e colore verde e grandezza del carattere non specificata
plot(cadmium,zinc, col="green",pch=17,cex=1)
#cambiare le etichette
plot(cadmium,zinc, col="green",pch=17,cex=1,xlab="rame", ylab="zinco")
#multiframe o multipanel
par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)
#invertiamo i grafici riga/colonna in colonna/riga
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)
#multiframe automatico
install.packages("GGally")
library(GGally)
ggpairs(meuse[,3:6])
#spatial
head(meuse)
coordinates(meuse)=~x+y
plot(meuse)
#funzione spplot per dati spaziali
spplot(meuse,"zinc")

# R spatial
#libreria sp
library(sp)
#dati da usare
data(meuse)
head(meuse)
#coordinate del dataframe 
coordinates(meuse)= ~x+y
#spplot dei dati di zinco
spplot(meuse,"zinc")
# Exercise : spplot dei dati di rame
head(meuse)
spplot(meuse,"copper")
# bubble per plottare i dati
bubble(meuse,"zinc")
# exercise: bubble del rame colorato di rosso
bubble (meuse,"copper", col="red")

#Esercizio per lavorare con dati propri
#formaminifer (sofia), carbon capture (marco)
foram <- c(10,20,35,55,67,80)
carbon <- c(5,15,30,70,85,99)
#plottiamo i dati per vedere se i dati sono +o- relazionati tra loro
plot(foram, carbon, col="green", cex=2,pch=19)

#Dati dall'esterno : covid19
#Bisogna stabilire da quale cartella occorre prendere i dati 
#files -> scegli cartella -> importa dati
covid <- read.table("covid_agg.csv",head= TRUE)

########################################################################


### 3. Analisi dei pattern legati ai punti

# installazione pacchetto spatstat
install.packages("spatstat")
# installazione pacchetto rgdal
install.packages("rgdal")

# richiamo i pacchetti precedentemente installati
library(ggplot2) # comando alternativo require(ggplot2)
library(spatstat)
library(rgdal)

# impostazione della working directory
setwd("~/Documents/lab")

# importazione dei dati
covid <- read.table("covid_agg.csv", head=T)  #head=T fa capire al sistema che si vuole aggiungere l'intestazione 

# richiamo le prime 6 righe della tabella
head(covid)

# creazione di un plot per iniziare a vedere la disposizione delle variabili
plot(covid$country,covid$cases) 
attach(covid) # modo alternativo per collegare una variabile al proprio dataset
plot(country,cases)

# con las si può modificare la modalità di visone dei labels
plot(covid$country,covid$cases,las=0) # parallel labels
plot(covid$country,covid$cases,las=1) # horizontal labels
plot(covid$country,covid$cases,las=2) # perpendicular labels
plot(covid$country,covid$cases,las=3) # vertical labels
#con ce.axis si controlla la grandezza dei labels
plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5) # vertical labels

# richiamo ggplot2
library(ggplot2)
# richiamo i dati nella libreria
data(mpg)
# mostro le prime 6 righe della tabella
head(mpg)
# 3 componenti fondamentali per un grafico:
# data
# aes -> estetica delle variabili
# tipo di geometria

# creo un plot che contenga queste 3 componenti
ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()  #poco fruibile ai fini dei dati in questione

# creo un ggplot di covid
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

# vado ad analizzare il fattore densità
#richiamo la libreria in relazione ai dati covid
library(spatstat)
attach(covid)

# tramite la funzione ppp creo un nuovo dataset che mi interessa per l'analisi spaziale
covids <- ppp(lon, lat, c(-180,180), c(-90,90))

# semplifico la variabile della densità chiamandola d
d <- density(covids)

# plot di d
plot(d)

# si aggiungono i punti al plot
points(covids)

#save the .RData

setwd("~/Documents/lab")
load("point_pattern.RData")
ls()

# plot della mappa di densità
plot(d)
#palette
cl <- colorRampPalette(c("yellow","orange","red"))
plot(d, col= cl)

#plot della mappa della densità dal verde al blu
cl <- colorRampPalette(c("green","blue","violet"))
plot(d, col=cl)

# con points si possono inserire i punti spaziali definiti grazie alla funzione ppp
points(covids)

#
coastlines <- readOGR ("ne_10m_coastline.shp")
plot(coastlines, add=T)

#Exercise : plot della mappa di densità con una nuova colorazione e aggiunta delle coastlines
cl <- colorRampPalette(c("light blue","yellow","red")) (800)
plot(d, col=cl)
plot(coastlines, add=T, col="yellow")

# carico il pointpatterns.rdata e creo un grafico della mappa di densità

setwd("~/Documents/lab")
covid <- read.table("covid_agg.csv",header = T)
library(spatstat)
attach(covid)
covids <- ppp(lon,lat,c(-180,180),c(-90,90))
d <- density(covids)
plot(d)
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
coastlines <- readOGR("ne_10m_coastline.shp")
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

#interpolation

#controllo la tabella
head(covids)

#creo valori per l'interpolazione
marks(covids) <- covid$cases

#funzione di interpolazione
s <- Smooth(covids)
plot(s)

ls()
covids <- ppp(lon, lat, c(-180,180), c(-90,90))
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))

#Exercise: plot(s) with points and coastlines
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

#mappa finale -> unico grafuco con entrambi i plot
par(mfrow=c(2,1))
# densità
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

 

# interpolazione del numero di casi
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)


#Esercizio San Marino
load("/Users/ariannalucarini/Documents/lab/Tesi.RData")
head(Tesi)
#richiamare libreria spat
library(spatstat)
attach(Tesi)

summary(Tesi) 

#x varia da 12.42 a 12.46
#y varia da 43.91 a 43.94
#per la figura aumentiamo un po' i margini

#point patterns : x longitudine, y latitudine
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47),c(43.9,43.95))

#density
dT <- density(Tesippp)
plot(dT)
points(Tesippp, col="green")

#################################################################

### 4. CODICE R PER ANALISI DI IMMAGINI SATELLITARI 07/04/20

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

##################################################################################

# R code analisi multitemporale di variazione della land cover

setwd("~/lab/")
# setwd("/Users/utente/lab") #mac
# setwd("C:/lab/") # windows

library(raster)
# install.packages("RStoolbox")
library(RStoolbox)

defor1 <- brick("defor1_.jpg") # .png for Mac
defor2 <- brick("defor2_.jpg")

defor1
# names: defor1_.1, defor1_.2, defor1_.3 
# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green

plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")

# Exercise plot della seconda data
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# classificazione non supervisionata
d1c <- unsuperClass(defor1, nClasses=2)

plot(d1c$map)
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)

# possibilità 2
cl <- colorRampPalette(c('green','black'))(100) # 
plot(d1c$map, col=cl)

# esempio sul significato del $
# mappageologica <- geomap(im_sat,nClasses=....)
# plot(mappageologica$lito)
# plot(mappageologica$lineaments)

# classificazione di defor2
# Exercise: classificare con due classi l'immagine satellitare defor2
d2c <- unsuperClass(defor2, nClasses=2)
plot(d2c$map, col=cl)

# plot delle due mappe ottenute
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

freq(d1c$map)
# aree aperte = 37039
# foresta = 304253

totd1 <- 37039 + 304253
totd1
# 341292

percent1 <- freq(d1c$map) * 100 / totd1

# percentuali
# foreste: 89.1
# aree aperte: 10.9

#---- 

freq(d2c$map)
# aree aperte: 165055
# foreste: 177671

totd2 <- 165055 + 177671
totd2
# 342726

percent2 <- freq(d2c$map) * 100 / totd2

# percent
# aree aperte: 48.2
# foreste: 51.8

#-----

cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)

output <- data.frame(cover,before,after)
output


library(ggplot2)
p1<-ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
# 
p2<-ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
install.packages("gridExtra")
library(gridExtra)

grid.arrange(p1, p2, nrow = 1) # this needs griExtra


#day 2
setwd("~/Documents/lab")
load("defor.RData")
ls()
library(raster)
par(mfrow=c(1,2))
cl <- colorRampPalette(c("black","green"))(100)
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
ls()
install.packages("ggplot2")
library(ggplot2)
ggplot(output, aes(x=cover,y=before, color=cover))
geom_bar(stat="identity", fill="white")

#Exercise: plot the istogram of land cover after deforestation
ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

dev.off()

install.packages("gridExtra")
library(gridExtra)

#histograms of the% cover before deforestation
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white")

 grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white")

#Exercise.  #grid.arrange per mettere due grafici uno accanto all'altro
grid.arrange(grafico1,grafico2,nrow=1)

#######################################################################

#R code for analysing NO" data from ESA - January to March 2020

setwd("~/Documents/lab")

#richiamare library
library(raster)

EN01 <- raster("EN_0001.png")
plot(EN01)



# EN <- stack(c("EN_0001.png","EN_0002.png","EN_0003.png","EN_0004.png","EN_0005.png","EN_0006.png","EN_0007.png","EN_0008.png","EN_0009.png","EN_0010.png","EN_0011.png","EN_0012.png","EN_0013.png"))

# brick
# EN01 <- brick("EN_0001.png")
# EN02 <- brick("EN_0002.png")
# EN03 <- brick("EN_0003.png")
# EN04 <- brick("EN_0004.png")
# EN05 <- brick("EN_0005.png")
# EN06 <- brick("EN_0006.png")
# EN07 <- brick("EN_0007.png")
# EN08 <- brick("EN_0008.png")
# EN09 <- brick("EN_0009.png")
# EN10 <- brick("EN_0010.png")
# EN11 <- brick("EN_0011.png")
# EN12 <- brick("EN_0012.png")
# EN13 <- brick("EN_0013.png")

# brick
# writeRaster(EN01[[3]], "snow2000r.tif")


# use .red to export the data: prepare the set
EN01 <- raster("EN_0001.png")
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 

par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

# close the window

difno2 <- EN13-EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) # 
plot(difno2, col=cldif)

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 

# PLOT ALL THE DATA
# video like
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)

par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)

# make a stack
EN <- stack(EN01,EN02,EN03,EN04,EN05,EN06,EN07,EN08,EN09,EN10,EN11,EN12,EN13)

plot(EN,col=cl)

# RGB
plotRGB(EN, red=EN13, green=EN13, blue=EN01, stretch="lin")

boxplot(EN,horizontal=T,axes=T,outline=F)

###########################################################################

# R_code_snow.r

#set working directory
setwd("~/Documents/lab")

# scarico libreria che permette di vedere i dati con estensione .nc
install.packages ("ndcf4")

#richiamo librerie
library(ncdf4)
library(raster)

#importo immagine .nc 
#raster = importa un singolo livello
#brick = importa vari livelli -> es. imm. satellitari a diverse bande

snowmay <-  raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

#ex. plot the snowcover whit the clpalette
plot(snowmay,col=cl)

#import snow data
#setto la nuova working directory
setwd("~/Documents/lab/snow")
rlist <- list.files(pattern=".tif")
list_rast <- lapply(rlist, raster) #applica una funzione all'intera lista di file 
snow.multitemp <- stack(list_rast)
plot(snow.multitemp, col=cl)

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl,zlim =c(0,250))
plot(snow.multitemp$snow2020r, col=cl,zlim =c(0,250))

dev.off

#differenza fra le due mappe
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette (c("blue","white","red"))(100)
plot(difsnow, col=cldiff)

#previsione multitemporale nel 2025
source("prediction.r")

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")

plot(predicted.snow.2025.norm) # previsione della neve al 2025 fatta secondo una previsione lineare

##################################################################################################


### 9. R_code_ patches

#richiamo libreria raster
library(raster)

#Settaggio working directory
setwd("~/Documents/lab")

#carico dati
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

#metto più plot all'interno dello stesso grafico
par(mfrow=c(1,2))
#faccio il plot
cl <- colorRampPalette(c("black","green"))(100)  #la foresta è la classe n.2 colorata di verde
plot(d1c, col=cl)
plot(d2c, col =cl)
#forest : class 2 ; agricolture : class 1

#cbind serve per annullare certi valori
d1c.for <- reclassify(d1c,cbind(1,NA)) #elimino tutto ciò che non è foresta
par(mfrow=c(1,2))
plot(d1c, col=cl)
plot(d1c.for, col =cl)

par(mfrow=c(1,2))
plot(d1c)
plot(d2c)

#crearing patches
install.packages("igraph")
d1c.for.patches <- clump(d1c.cat.for)
d2c.for.patches <- clump (d2c.cat.for)


#writeRaster(d1c.for.patches, "d1c.for.patches.tif")
#writeRaste(d2c.for.patches, "d2c.for.patches.tif")

#Exercise
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)


dic.for.patches
#max patches d1=301 
#max patches d2=1212

#plot results:
time <- c("Before deforestation", "After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)

#plot finale
library(ggplot)
ggplot(output,aes(x=time, y=npatches, color="red") + geom_bar(stat="identity",fill="white")


