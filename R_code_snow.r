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


