## Load required packages
library(maptools)
library(raster)

# Download data from gadm.org using getData function 
admPAK <- getData('GADM', country='PAK', level=3)
plot(admPAK, bg="lightgreen", axes=T)
str(admPAK)
##Plot 
plot(admPAK, lwd=10, border="skyblue", add=T)
plot(admPAK,col="yellow", add=T)
grid()
box()

invisible(text(getSpPPolygonsLabptSlots(admPAK), labels=as.character(admPAK$NAME_3), cex=0.4, col="black", font=2))
mtext(side=3, line=1, "District Map of Pakistan", cex=2)
mtext(side=1, "Longitude", line=2.5, cex=1.1)
mtext(side=2, "Latitude", line=2.5, cex=1.1)