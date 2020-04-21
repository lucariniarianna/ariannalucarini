#R code land cover
#set working directory
setwd("~/Documents/lab")
#richiamare la libreria
library(raster)
library(RStoolbox)
install.packages ("RStoolbox")
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#landsat bands: 1b,2g,3r,4nir
#rgb
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")

p224r63_2011c <- unsuperClass(p224r63_2011,nClasses=4)
plot(p224r63_2011c$map)
clclass <- colorRampPalette(c("red","green","blue","black"))(100)
plot(p224r63_2011c$map, col=clclass)
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)

#in funzione del n di classi aumenta l'incertezza dell'algoritmo automatico di classificazione, riportando potenzialmente classi leggermente differenti
