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






