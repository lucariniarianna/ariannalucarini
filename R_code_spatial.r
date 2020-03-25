# R spatial : funzioni spaziali in Ecologia del paesaggio


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









