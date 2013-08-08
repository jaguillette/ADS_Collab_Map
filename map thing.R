library(maps)
library(geosphere)
library(ggplot2)
library(mapproj)

png(file="myMap.png",width=1200,height=616)

# added a line for me to easily access it at work, there might be a better way though... 
csvName='bibcodeSet.csv'
#csvName_erin_work='/Users/ebraswell/Documents/code/ADS_Collab_Map/bibcodes/2013ApJ...767..132H.csv'

affil <- read.csv(csvName, header=TRUE)
#affil <- read.csv(csvName_erin_work, header=FALSE)

# wanted to clean up the names a bit - 
# I have a terrible memory so can never remember V1 vs V2, etc :) 
#names(affil) <- c('institution','lat','long')


#Sets central point, this one is the CfA
base=affil[3,]

#These 3 lines draw a map limited by the maximum and minimum values for longitude and latitude within the data provided. 
#Comment out and use other map line for full world map.
#xlim <- c((min(affil$lng)-5), (max(affil$lng)+5))
#ylim <- c((min(affil$lat)-5), (max(affil$lat)+5))
#map('world', col="#DCF2DC", fill=TRUE, bg="#BBCBFA", lwd=0.05, xlim=xlim, ylim=ylim)

#Draws a full world map. Use preceding three lines for limited full map.
map('world', col="#DCF2DC", fill=TRUE, bg="#BBCBFA", lwd=0.05, wrap=TRUE)

#Draws lines from central point described
for (j in 1:length(affil$bibcode)) {
  inter <- gcIntermediate(c(base$lng, base$lat), c(affil[j,]$lng, affil[j,]$lat), n=100, breakAtDateLine=TRUE, addStartEnd=TRUE)
  if(length(inter)==2){
    lines(inter[[1]], col="black", lwd=.25)
    lines(inter[[2]], col="black", lwd=.25)
  }
  else {
    lines(inter, col="black", lwd=.25)
  }
}
dev.off()




## Mapping using ggplot2 so you can stack map layers on top of each other? ##
# adding a randomly generated size column to experiment with dots of different sizes
# and why not a randomly generated column for "type of institution" to experiment with colors?

affil$size <- NULL
for (i in 1:nrow(affil)) {
  affil$size[i] = sample(1:10, 1)
}

affil$type <- NULL
for (i in 1:nrow(affil)) {
  affil$type[i] = sample(1:5, 1)
}

world_data <- map_data("world", lwd=0.05)
world_map <- ggplot() + geom_polygon(data=world_data, aes(x=long, y=lat), fill='#DCF2DC', colour='#BBCBFA')
world_map <- world_map + geom_point(data=affil, aes(x=lng, y=lat, size=size, colour=type))
world_map + theme_bw() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank())


