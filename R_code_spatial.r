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


