library(maps)
library(geosphere)

png(file="myMap.png",width=1200,height=616)

csvName='C:\\Users\\Jeremy\\Documents\\GitHub\\ADS_Collab_Map\\bibcodes\\2013ApJ...767..132H.csv'
affil <- read.csv(csvName, header=FALSE)

#Sets central point, this one is the CfA
base=affil[8,]

#These 3 lines draw a map limited by the maximum and minimum values for longitude and latitude within the data provided. Comment out and use other map line for full world map.
#xlim <- c((min(affil$V3)-5), (max(affil$V3)+5))
#ylim <- c((min(affil$V2)-5), (max(affil$V2)+5))
#map('world', col="#DCF2DC", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)

#Draws a full world map. Use preceding three lines for limited full map.
map('world', col="#DCF2DC", fill=TRUE, bg="white", lwd=0.05, wrap=TRUE)

#Draws lines from central point described
for (j in 1:length(affil$V1)) {
  inter <- gcIntermediate(c(base$V3, base$V2), c(affil[j,]$V3, affil[j,]$V2), n=100, breakAtDateLine=TRUE, addStartEnd=TRUE)
  if(length(inter)==2){
    lines(inter[[1]], col="black", lwd=1)
    lines(inter[[2]], col="black", lwd=1)
  }
  else {
    lines(inter, col="black", lwd=1)
  }
}
dev.off()