# R_code_exam.r

#1. R_code_first.r   
#2. R_code_spatial.r  [1 e 2] 
#3. R_code_point_pattern   
#4. R_code_teleril.r   
#5. R_code_landcover.r   
#6. R_code_multitemp.r   
#7. R_code_multitemp_NO2.r   
#8. R_code_snow.r   
#9. R_code_patches.r 
#10.R_code_crop.r
#11.R_code_SDM.r
#12.R_code_exam_project.r 

#Copernicus Website
https://land.copernicus.vgt.vito.be/PDF/portal/Application.html

### 1. R code first

#Prima di tutto occorre installare la nuova libreria, attraverso i pacchetti, e richiamarla (ogni volta che vogliamo richiamare
#qualcosa che si trova esternamente a R occorrei usare le virgolette
install.packages("sp") 
library(sp) #sp ci fornisce classi e metodi per effettuare un'analisi spaziale
#un modo alternativo per richiamare le librerie è require()

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

#EXERCISE: mettere come lower panel lo smoothing, come diagonal apnel gli istogrammi e come upper panel le correlazioni 
pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)

###FUNZIONE PLOT
#tramite la funzione meuse $ possiamo creare un grafico prendendo in considerazione le colonne da noi scelte
plot(meuse$cadmium,meuse$copper)

#usiamo attach per fissare il dataframe meuse
attach(meuse)

#ora possiamo fare un plot senza dover scrivere ogni volta meuse perchè abbiamo usato la funzione attach
plot(cadmium,copper, pch=17, col="green", main="primo.plot", xlab="cadmio",ylab="rame")

#############################################################################################################################

### 2. R spatial : funzioni spaziali in Ecologia del paesaggio

#installo pacchetto che servirà nell'analisi spaziale
install.packages("GGally")

#richiamo il pacchetto library()
library(sp)
library(GGally)

#richiamo dati
data(meuse)

#vediamo i dati
meuse

#vediamo solo le prime 6 righe di dati
head(meuse)

#usiamo attach per fissare il dataset
attach(meuse)

#plot che mette in correlazione cadmio e piombo
plot(cadmium,lead,col="red",pch=19,cex=2)

#Exercise: fare un plot di rame e zinco con carattere "triangolo" e colore verde e grandezza del carattere non specificata
plot(cadmium,zinc, col="green",pch=17,cex=1)

#cambiare le etichette tramite funzioni xlab e ylab
plot(cadmium,zinc, col="green",pch=17,cex=1,xlab="rame", ylab="zinco")

#con multiframe o multipanel possiamo mostrare più grafici insieme
par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)

#invertiamo i grafici riga/colonna in colonna/riga tramite (mfrow=c())
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)

#richiamo pacchetto
library(GGally)

#la funzione ggpairs(meuse) è in grado di fare un grafico con tutte le variabili, in questo caso noi abbiamo specificato quali
ggpairs(meuse[,3:6])


#SPATIAL
#diciamo ad R che nel nostro dataset sono presenti delle coordinate
coordinates(meuse)=~x+y

#grafico di meuse
plot(meuse)

#la funzione spplot viene usata per creare grafici per dati spaziali
spplot(meuse,"zinc")
#dal grafico si evince che le zone più inquinate saranno quelle interessate dai punti gialli

# R spatial day 2

#richiamo libreria sp
library(sp)

#dati da usare
data(meuse)

#visualizzo le prime 6 righe di dati
head(meuse)

#definisco le coordinate del dataframe 
coordinates(meuse)= ~x+y

#spplot crea un grafico prendendo in considerazione le variabile zinco
spplot(meuse,"zinc")

# Exercise : spplot dei dati di rame
head(meuse) #in alternativa possiamo usare names per vedere i nomi delle colonne
spplot(meuse,"copper")

#la funzione bubble serve per plottare i dati secondo un grafico esteticamente diverso (a bolle)
bubble(meuse,"zinc")

#Exercise: bubble del rame colorato di rosso
bubble (meuse,"copper", col="red")

#Creiamo un nuovo oggetto contenente dei nostri dati
#formaminifer (dati presi da sofia), carbon capture (dati presi da marco)

#tramite "<-" diamo un nome al nostro oggetto
foram <- c(10,20,35,55,67,80)
carbon <- c(5,15,30,70,85,99)

#plottiamo i dati per vedere se i dati sono +o- relazionati tra loro
plot(foram, carbon, col="green", cex=2,pch=19)

#scarichiamo un nuovo pacchetto dati riguardanti il covid19

#Dati dall'esterno : covid19
#Bisogna stabilire da quale cartella occorre prendere i dati facendo files -> scegli cartella -> importa dati

#impostiamo una working directory 
setwd("~/Documents/lab")

#semplifichiamo il nome sempre tramite "<-"
covid <- covid_agg

#visualizziamo la tabella con intestazione tramite la funzione head=TRUE
covid <- read.table("covid_agg.csv",head= TRUE)

############################################################################################################################

### 3. Analisi dei pattern legati ai punti

# installazione pacchetto spatstat
install.packages("spatstat")
# installazione pacchetto rgdal
install.packages("rgdal")

# richiamo i pacchetti precedentemente installati
library(ggplot2) # in alternativa si può usare il comando require(ggplot2)
library(spatstat)
library(rgdal)

# impostazione della working directory
setwd("~/Documents/lab")

# importazione dei dati
covid <- read.table("covid_agg.csv", head=T)  #head=T fa capire al sistema che si vuole aggiungere l'intestazione 

# richiamo le prime 6 righe della tabella 
head(covid)

# creo un plot iniziale per vedere la distribuzione delle variabili
plot(covid$country,covid$cases) 

# dichiariamo che i dati da utilizzare sono quelli del covid
attach(covid) # modo alternativo per collegare una variabile al proprio dataset
plot(country,cases)

# con las si può modificare la modalità di visone dei labels
plot(covid$country,covid$cases,las=0) # etichette parallele
plot(covid$country,covid$cases,las=1) # etichette orizzontali
plot(covid$country,covid$cases,las=2) # etichette perpendicolari
plot(covid$country,covid$cases,las=3) # etichette verticali

#con ce.axis si controlla la grandezza dei labels
plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5) 

# richiamo ggplot2
library(ggplot2)

# richiamo i dati nella libreria
data(mpg)

# mostro le prime 6 righe della tabella
head(mpg)

# le componenti fondamentali per un grafico sono 3:
# data
# aes -> estetica delle variabili
# tipo di geometria

# creo un plot che contenga queste 3 componenti
ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()  #poco fruibile ai fini dei dati in questione

# creo un ggplot di covid
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

# vado ad analizzare il fattore densità e per farlo richiamo la libreria in relazione ai dati covid
library(spatstat)
attach(covid)

# tramite la funzione ppp creo un nuovo dataset che mi interessa per l'analisi spaziale
covids <- ppp(lon, lat, c(-180,180), c(-90,90))

# semplifico la variabile della densità chiamandola d
d <- density(covids)

# faccio un plot di d
plot(d)

# con points si possono inserire i punti spaziali definiti grazie alla funzione ppp
points(covids)

#save the .RData
setwd("~/Documents/lab")
load("point_pattern.RData")

#chiudo il grafico tramite questa funzione
ls()

# plot della mappa di densità
plot(d)
#cambio i colori del grafico tramite la colorRampPalette che chiameremo cl
cl <- colorRampPalette(c("yellow","orange","red"))

#faccio un plot della densità con i colori da me scelti
plot(d, col= cl)

#cambio i colori e faccio un altro plot della mappa della densità dal verde al blu
cl <- colorRampPalette(c("green","blue","violet"))
plot(d, col=cl)

# aggiungo i punti al grafico
points(covids)

#aggiungiamo i bordi dei vari paesi usando la libreria rgdal
#rgdal crea dei collegamenti alla libreria dei dati geospaziali
coastlines <- readOGR ("ne_10m_coastline.shp")

#faccio un plot completo di intestazione
plot(coastlines, add=T)

#Exercise : plot della mappa di densità con una nuova colorazione e aggiunta delle coastlines
cl <- colorRampPalette(c("light blue","yellow","red")) (800)
plot(d, col=cl)
plot(coastlines, add=T, col="yellow")

cl2 <- colorRampPalette(c('red','orange','yellow','green', 'blue')) (800)
plot(d, col=cl2)
plot(coastlines, add=T)

cl3 <- colorRampPalette(c('green', 'violet', 'blue')) (200)
plot(d, col=cl3)
plot(coastlines, add=T)

cl4 <- colorRampPalette(c('violet','yellow','green'))(100)
plot(d, col=cl4)
plot(coastlines, add=T)

cl5 <- colorRampPalette(c('darkcyan', 'purple', 'red')) (200) 
plot(d, col=cl5)
plot(coastlines, add=T)

cl6 <- colorRampPalette(c('darkcyan', 'purple', 'red')) (200) 
plot(d, col=cl6)
plot(coastlines, add=T)

cl7<-colorRampPalette(c('white','blue','green','red','orange','yellow')) (150)
plot(d, col=cl7)
plot(coastlines, add=T)

# Exercise: caricare il workspace point_pattern.RData (load("...")) e crare un garfico della mappa di densità

#richiamo le librerie utili
library(spatstat)
library(rgdal) # per le coastlines

#imposto la working directory
setwd("~/Documents/lab")

#sarico dati
load("point_pattern.RData")

#chiudo grafico
ls()

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="
")
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
text(covids)

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

### Esercizio San Marino
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
colors()

setwd("~/Documents/lab")

load("sanmarino.RData")

library(spatstat)
ls()

# dT=density map, Tesi=dataset originale, Tesi_ppp=point pattern

plot(dT)
points(Tesippp, col="green")

head(Tesi)

marks(Tesippp) <- Tesi$Species_richness
interpol <- Smooth(Tesippp)
plot(interpol)
points(Tesippp, col="green")

library(rgdal)
sanmarino <- readOGR("San_Marino.shp")

plot(sanmarino)
plot(interpol, add=T)
points(Tesippp,col="green")
plot(sanmarino, add=T)

# Exercise: plot multiframe di densità e interpolazione
par(mfrow=c(2,1))

plot(dT, main="Density of points")
points(Tesippp,col="green")

plot(interpol, main="Estimate of species richness")
points(Tesippp,col="green")

# Exercise: plot multiframe di densità e interpolazione uno acacnto all'alto
par(mfrow=c(1,2))

plot(dT, main="Density of points")
points(Tesippp,col="green")

plot(interpol, main="Estimate of species richness")
points(Tesippp,col="green")

#############################################################################################################################

### 4. CODICE R PER ANALISI DI IMMAGINI SATELLITARI - telerilevamento 07/04/20

# scarico e/o richiamo i pacchetti che verranno utilizzati
install.packages("raster") # il pacchetto raster ci permette dileggere, scrivere, manipolare, analizzare e modellare dati 
#spaziali su una griglia
#richiamo pacchetto
library(raster)

#RStoolbox serve per l'elaborazione e l'analisi delle immagini di telerilevamento 
install.packages("RStoolbox")

#setto la directory
setwd("~/Documents/lab")

#la funzione brick prende un immagine satellitare all'interno di una cartella e le da un nome
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#plot della nostra immagine satellitare iniziale per estrarre dati base (riflettanze ecc...)
plot(p224r63_2011) # si nota un paesaggio in varie bande (b1...b7 ognuno con una lunghezza d'onda diversa)

#Day 2 - 08/04/20

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

#una volta aggiornata la palette la richiamo con col=cl
plot(p224r63_2011,col=cl)

# cambiamo la scala cromatica
cllow<-colorRampPalette(c("black","grey","light grey"))(5) #esperimento per vedere la riflettanza usando solo 5 gamme di colore
plot(p224r63_2011, col=cllow)

#plottiamo l'immagine con la gamma del blu
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_2011$B1_sre,col=clb)

names (p224r63_2011) #per vedere i nomi delle bande che stiamo utilizzando
#[1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"
# attach(dataframe) non funziona con la funzione raster
# simbolo che lega la colonna (la banda) al dataset (immagine satellitare) -> $
# abbiamo prodotto un'immagine nella prima banda con i colori associati alla riflettanza di questa banda

#Esercizio : plottare la banda dell'infrarosso vicino con colorramppalette che varia dal rosso, all'arancione al giallo
clnir <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_2011$B4_sre,col=clnir)
#ci sarà molta vegetazione perchè le piante riflettono molto l'infrarosso vicino

#plot di tutte e 4 le bande tramite la funzione par che peremette di utilizzare a blocchi la nostra finestra
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

#stretch serve per aumentare la gamma dei colori, in questo caso usiamo lo strech lineare
plotRGB(p224r63_2011,r=3,g=2,b=1,stretch="Lin")

#utilizziamo il nir per distinguere meglio la vegetazione (Dobbiamo però metterlo al posto di un altro perchè si possono usare soll 3 alla volta)
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin") #false colours

#salvare un'immagine in pdf o in #png("primografico.png") i grafici sono meno pesanti
pdf("primografico.pdf")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
dev.off()

#per confrontare le due immagini in un grafico multiframe utilizziamo par
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

# Day 2

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

#plot dell'immagine del 1988
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

#Plot dell'immagine 1988 e 2011 per notare le differenze
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

#titolo grafico
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin",main="2011")

#la parte agricola è molto più sviluppata rispetto al 1988 (le piante che riflettono il nir sono rosse mentre il suolo agricolo avrà un altro colore
dev.off()

#calcoliamo l'indice di salute della vegetazione in base alla riflettanza dell'infrarosso dalle foglie
#DVI : Difference Vegetation index (spectral index)
#DVI=NIR-RED i risultati cambiano in base alla salute delle piante
#Sana = NIR alto
#Malata = RED alto

#dvi1988=nir1988-red1988
dvi1988 <- p224r63_1988$B4_sre-p224r63_1988$B3_sre  #$ serve per legare due oggetti
# vediamo il plot
plot(dvi1988)

#spectral index of 2011
dvi2011 <- p224r63_2011$B4_sre-p224r63_2011$B3_sre
plot(dvi2011)
cldvi <- colorRampPalette(c("light blue", "light green", "green")) (100)
plot(dvi2011, col=cldvi)

#differenza nel tempo dei due indici = analisi multitemporale, possiamo vedere il cambiamento dell'indice dello stato di vegetazione
#più il valore sarà alto e più lo stato di salute sarà migliore
difdvi <- dvi2011-dvi1988

#tramite questo plot possiamo vedere le zone dove le piante hanno subito più stress
plot(difdvi)
cldifdvi <- colorRampPalette(c("red","white","blu"))(100)
plot(difdvi, col= cldifdvi)

#visualizzazione dell'output tramite un multiframe dell'immagine del 1988, del 2011 e della differenza tra questi anni
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)

#la funzione aggregate serve per cambiare la risoluzione dell'immagine
p224r63_2011lr <- aggregate(p224r63_2011, fact=10) #fact 10 vuol dire che stiamo usando una scala 10 volte maggiore
p224r63_2011 #caratteristiche dell'immagine originale
p224r63_2011lr #caratteristiche della nuova immagine

#creo il grafico
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
dev.off()

#dvi della nuova immagine 2011 con una risoluzione minore
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
#original 30m -> resampled 1500m
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

dv2011lr50 <- p224r63_2011lr50$B$_sre-p224r63_2011lr50$B4_sre
plot(dv2011lr50)


#dvi della nuova immagine 1988 con risoluzione minore
p224r63_1988lr50 <- aggregate(p224r63_2011, fact=50) #la risoluzione è importante perchè altrimenti non riusciremo a distinguere le componenti della biodiversità
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
#differenza dvi corretta
difdvilr50 <- dvi2011lr50 - dvi1988lr50
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
#differenza dvi dei due anni a bassa risoluzione
difdvilr50 <- dvi2011lr50 - dvi1988lr50
#creo l'immagine
plot(difdvilr50,col=cldifdvi)

#multiframe del totale
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)
 
#############################################################################################################################

###5. R code landcover

#setto la directory
setwd("~/Documents/lab")
load("/Users/ariannalucarini/Documents/lab/Teleril.RData")
# controllo i dati che sono all'interno 
ls()

#richiamo pacchetti
library(spatstat)
library(rgdal)

#prime sei righe dei nostri dati
head (Tesi)

#riferisco quali dati utilizzare
attach(Tesi)

#vedo il sommario dei dati
summary(Tesi)
ls()

tesip <- ppp(Longitude,Latitude,c(12.41,12.47),c(43.90,43.95))

dT <- density(tesip)

#faccio grafico
plot(dT)

#la funzione marks va ad associare le variabili al point pattern
marks(Tesippp) <- Tesi$Species_richness

#Smooth serve per dare continuità alla mappa anche dove i valori non ci sono (fa una stima)
interpol <- Smooth(Tesippp)
plot(interpol)
points(Tesippp, col="green")

#metto i confini di San Marino e faccio il plot
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

#############################################################################################################################

###6. R code analisi multitemporale di variazione della land cover - R code multitemp

# imposto la woriking directory
setwd("~/Documents/lab")

# richiamo le librerie
library(raster)
library(RStoolbox)

#richiamo le immagini che ci servono per l'analisi
p224r63_2011 <- brick("p224r63_2011_masked.grd")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")

#con questa funzione è possibile far leggere le immagini al programma
defor1 <- brick("defor1_.jpg") # .png for Mac
defor2 <- brick("defor2_.jpg")

#guardo le caratteristiche dell'immagine
defor1
# names: defor1_.1, defor1_.2, defor1_.3 
# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green

#plot RGB
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")

# Exercise plot della seconda data
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

#creo un multiframe per vedere i due plot a confronto
par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# classificazione non supervisionata, ovvero, non si specificano le classi (si utilizza la libreria RStoolbox
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

# plot delle due mappe ottenute per confrontarle
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

#frequenza delle due mappe
freq(d1c$map)
# aree aperte = 37039
# foresta = 304253

totd1 <- 37039 + 304253
totd1
# 341292

#la percentuale di foresta è uguale alla frequenza della prima mappa x 100 diviso il totale
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
#imposto la directory
setwd("~/Documents/lab")

#installo e richiamo pacchetto
install.packages("gridExtra")
library(gridExtra)

#richiamiamo il file salvato:
load("Analisi_multitemporale.R")

cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)

#visualizzo l'output
output <- data.frame(cover,before,after)
output

#richiamo libreria
library(ggplot2)

#sulle ordinate avremo il valore della prima
p1<-ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")

#plot del dopo deforestazione, sempre sulle y avremo la percentuale del dopo
p2<-ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

#questa funzione è in grado di prendere diversi plot e visualizzarli insieme all'interno di uno stesso grafico
grid.arrange(p1, p2, nrow = 1)


#day 2  Nuova Analisi Multitemporale

setwd("~/Documents/lab")
load("defor.RData")
ls()
install.packages("gridExtra")
library(gridExtra)
install.packages("ggplot2")
library(ggplot2)
library(raster)


par(mfrow=c(1,2))
cl <- colorRampPalette(c("black","green"))(100)
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
ls()

ggplot(output, aes(x=cover,y=before, color=cover))
geom_bar(stat="identity", fill="white")

#Exercise: plot the istogram of land cover after deforestation
ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

dev.off()

#histograms of the% cover before deforestation
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white")

#Exercise.  #grid.arrange per mettere due grafici uno accanto all'altro
grid.arrange(grafico1,grafico2,nrow=1)

#############################################################################################################################

###7. R code for analysing NO" data from ESA - January to March 2020

setwd("~/Documents/lab")

#richiamare library
library(raster)

#richiamo immagini in formato png perchè utilizzo mac
EN01 <- raster("EN_0001.png")

#visualizzo il plot
plot(EN01)

#per importare tutte le immaggini posso utilizzare o stack che le importa tutte dentro EN oppure brick per caricarle una alla volta

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

#utilizzo raster per importare le immagini
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

#creo la mia color ramp palette
cl <- colorRampPalette(c('red','orange','yellow'))(100) # 

par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

# chiudo la finestra
dev.off()
#il monossido di azoto è andato diminuendo

#vedo la differenza tra EN13 e EN01
difno2 <- EN13-EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) # 
plot(difno2, col=cldif)

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 

#plot delle immagini
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

#plot di tutte le immagini insieme
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

#############################################################################################################################

###8. R_code_snow.r

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

#dal sito copernicus scarico i dati interessati e li importo su R

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

#ex. plot the snowcover whit the clpalette
plot(snowmay,col=cl)

#import snow data
#setto la nuova working directory
setwd("~/Documents/lab/snow")

#salvo il raster nella list
rlist <- list.files(pattern=".tif")
list_rast <- lapply(rlist, raster) # lapply applica una funzione di riferimento (es. raster) all'intera lista di file 
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

#faccio una previsione multitemporale nel 2025 scaricando il pacchetto prediction.r da IOL
source("prediction.r")

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")

plot(predicted.snow.2025.norm) # previsione della neve al 2025 fatta secondo una previsione lineare

#############################################################################################################################

### 9. R_code_ patches

#Settaggio working directory
setwd("~/Documents/lab")

#installo e richiamo pacchetto
install.packages("igraph")
library(igraph)

#richiamo libreria raster
library(raster)

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
#reclassify serve per riclassificare l'immagine raster ridonando certi valori
d1c.for <- reclassify(d1c,cbind(1,NA)) #elimino tutto ciò che non è foresta
par(mfrow=c(1,2))
plot(d1c, col=cl)
plot(d1c.for, col =cl)

par(mfrow=c(1,2))
plot(d1c)
plot(d2c)

#creating patches
d1c.for.patches <- clump(d1c.cat.for)
d2c.for.patches <- clump (d2c.cat.for)

writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaste(d2c.for.patches, "d2c.for.patches.tif")

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
       
##########################################################################################################################
       
### 10. R code crop - exam simulation

#set della working directory
setwd("~/Documents/lab/snow")
       
#carico libreria
library(raster)
       
#Exercise : upload the whol snow set
rlist <- list.files(pattern="snow") #prendiamo tutti i file che cominciano per "snow"
rlist
       
list_rast <- lapply(rlist, raster)
sow.multitemp <- stack(list_rast)

#per fare il plot scelgo prima la color ramp palette
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(snow.multitemp,col=clb)
       
#creo uno zoom sull'immagine della zona da me interessata
snow.multitemp #controllo prima i miei dati per intero
plot(snow.multitemp$snow2010r, col=clb)
       
#prima di tutto devo definire l'estenzione
#es. ci sono due modi per definire le estenzioni: 
#1. fare rettangolo dell'area interessata
     plot(snow.multitemp$snow2010r, col=clb)
     zoom(snow.multitemp$snow2010r, ext=drawExtent()) #una volta lanciata la funzione
     #partiamo in alto a sinistra, teniamo tenuto, una volta finito il rettangolo dobbiamo rilasciare e premere una seconda volta
       
#2. definire le nuove cordinate
     ext <- c(6,20,35,50)
zoom(snow.multitemp$snow2010r, ext=extension) 
       
# crop
extension <- c(6, 20 , 35 , 50)
snow2010r.italy <- crop (snow.multitemp$snow2010r, extension) #tramite la funzione crop creo direttamente un ritaglio della mia area interessata
#con la funzione crop non va dichiarata l'extension
plot(snow2010.r.italy, col=clb)
       
#Exercise : crop the Italy extent on the whole stack of snow layers
snow.multitemp.italy <- crop(snow.multitemp, extension)
       
#visualizzo ora la mappa finale
plot(snow.multitemp.italy, col= clb)
       
snowmultitemp.italy #vediamo i dati per scegliere i valori minimi e massimi
#facciamo variare i range per cambiare le legende e metterle tutte uguali
plot(snow.multitemp.italy, col=clb, zlim=c(20,200)) #zlim fa si che si definisca il limite della legenda
#boxplot per vedere come si comportano le variabili
boxplot(snow.multitemp.italy, horizontal=T,outline=F)
#c'è meno copertura nevosa e si envince dal valore massimo della copertura nevosa che è molto alto nella parte del 2000 e più basso nella parte del 2020

#############################################################################################################################

### 11. Species Distribuion Modeling
       
#scarico il pacchetto necessario
install.packages("sdm")
library(sdm)
       
#richiamo librerie necessarie
library(raster)
library(rgdal) #libreria capace di gestire meglio dati raster e vettoriali (file vettoriali = coordinate x,y e quindi dei punti)

#importo e utilizzo i file che trovo dentro il pacchetto sdm       
file <- system.file("external/species.shp", package="sdm")
       
#la funzione shapefile (della libreria rgdal) serve per caricare la parte grafica e relativa ai punti    
species <- shapefile(file)
       
#guardo com'è fatto il mio file       
species       
#abbiamo una sola variabile: occurrence -> ovvero se c'è o meno la specie
       
#per ogni punto spaziale abbiamo un dato che ci dice se è presente o meno la data specie      
species$Occurrence
       
#faccio un plot di species      
plot(species)
       
#visualizzo in modo differente la presenza/assenza della specie utilizzando la funzione plot ma prendendo il dataset species e all'interno del
#dataset che sono uguali a 1 mettiamo il colore blu e il point characters 16.       
plot(species[species$Occurrence == 1,],col='blue',pch=16)
       
#ora aggiungiamo anche i punti che erano uguali a zero quindi la funzione cisto che dobbiamo aggiungere sarà points
points(species[species$Occurrence == 0,],col='red',pch=16)       
       
#ora prendiamo in considerazione anche le variabili ambientali (predittori)     
path <- system.file("external", package="sdm")
       
#faccio la lista dei file nel percorso appena definito 
lst <- list.files(path=path,pattern="asc",full.names=T)
lst
       
#faccio uno stack delle variabili (4)
preds <- stack(lst)      
       
#scelgo un colorramppalette
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
       
#plot finale con le 4 variabili       
plot(preds, col=cl)  
       
#faccio un plot della variabile elevation con i punti dove è stata avvistata la specie       
plot(preds$elevation, col=cl)
points(species[species$Occurrence == 1,], pch=16) #sarà una specie che non ama trovarsi in altitudini elevate
       
#faccio un plot della variabile temperature con i punti dove è stata avvistata la specie  
plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16 #sarà una specie che preferisce alte temperature   
       
#faccio un plot della variabile precipitation con i punti dove è stata avvistata la specie  
plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16 #sarà una specie che preferisce situazioni intermedie   
       
#faccio un plot della variabile vegetation con i punti dove è stata avvistata la specie   
plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16 #sarà una specie non particolarmente eliofila
       
#creo un modello lineare generalizzato     
d <- sdmData(train=species, predictors=preds)
d
       
#Creo il modello m1 (per i modelli l'uguale si fa con la tilde)
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=d, methods='glm')
       
#previsione di dove si troverà data specie       
p1 <- predict(m1, newdata=preds)
plot(p1, col=cl) 
points(species[species$Occurrence == 1,], pch=16 #abbiamo preso le singole variabili, i singoli predittori, 
#le abbiamo messe in un modello creando la mappa previsionale della distribuzione della specie rispetto le variabili
#############################################################################################################################
       
### 12. EXAM PROJECT
