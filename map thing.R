library(maps)
library(geosphere)
library(ggplot2)
library(mapproj)

png(file="myMap.png",width=2400,height=1232)
 
csvName='cfabibsaoref_-_20130514.csv'

affil <- read.csv(csvName, header=TRUE)

#Sets central point, this one is the CfA
base=affil[1,]
affil=affil[-1,]

#takes out outlier with 94 collaborations
affil=affil[-1,]

#orders results so less prominent affiliations are drawn first.
affil=affil[order(affil$totalCount),]

maxCount <- max(affil$totalCount)

pal <- colorRampPalette(c("#FFD7Dc", "#9E0004"))
colors <- pal(100)

#These lines draw a map limited by the maximum and minimum values for longitude and latitude within the data provided. 
#Comment out and use other map line for full world map.

LatLongView = 'United States'

switch(LatLongView,
       'United States'={
         xlim <- c(-130.07813, -60.82031)
         ylim <- c(25.16517, 49.15297)
         map('state', fill=TRUE, col="#DCF2DC", bg='#BBCBFA', lwd=.75)
       },
       'Europe'={
         xlim <- c(-21.621094, 43.769531)
         ylim <- c(35.460670, 75.501722)
         map('world', col="#DCF2DC", fill=TRUE, bg="#BBCBFA", lwd=0.05, xlim=xlim, ylim=ylim)
       },
       'World'={
         map('world', col="#DCF2DC", fill=TRUE, bg="#BBCBFA", lwd=0.05, wrap=TRUE)
       })

#Draws lines from central point described
for (j in 1:length(affil$lat)) {
  drawLat <- affil[j,]$lat
  drawLng <- affil[j,]$lng
  if((xlim[1]<=drawLng & drawLng<=xlim[2])&(ylim[1]<=drawLat & drawLat<=ylim[2])){
    inter <- gcIntermediate(c(base$lng, base$lat), c(drawLng, drawLat), n=100, breakAtDateLine=TRUE, addStartEnd=TRUE)
    colindex <- round((affil[j,]$totalCount / maxCount) * length(colors))
    if(length(inter)==2){
      lines(inter[[1]], col=colors[colindex], lwd=1.5)
      lines(inter[[2]], col=colors[colindex], lwd=1.5)
    }
    else {
      lines(inter, col=colors[colindex], lwd=1.5)
    }
  }
}
dev.off()




## Mapping using ggplot2 so you can stack map layers on top of each other? ##
# adding a randomly generated column for "type of institution" to experiment with colors?
# mapping size of dot to 'count' in affil 

affil$type <- NULL
for (i in 1:nrow(affil)) {
  affil$type[i] = sample(1:5, 1)
}

<<<<<<< HEAD
#world_data <- map_data("world", lwd=0.05)

#world_map <- ggplot() + 
#  geom_polygon(data=world_data, aes(x=long, y=lat), fill='#DCF2DC', colour='#BBCBFA')

#world_map <- world_map + 
#  geom_point(data=affil, aes(x=lng, y=lat, size=size, colour=type))
=======
world_data <- map_data("world")
world_map <- ggplot() + geom_polygon(data=world_data, aes(x=long, y=lat, group=group), fill='#DCF2DC', colour='grey')
world_map <- world_map + geom_point(data=affil, aes(x=lng, y=lat, size=totalCount, colour=factor(type))) + scale_colour_brewer(palette="Set1")
world_map <- world_map + theme(panel.grid.major.y = element_blank(), 
                               panel.grid.major.x = element_blank(), 
                               panel.background = element_rect(fill = '#BBCBFA'))
world_map

# now for a more globe-like map!
# A little off, especially at the N Pole, but pretty cool!

world_map + coord_map("ortho", orientation=c(30, -50, 0))
>>>>>>> efb97baff3459d9ebe9e2d15ff9bf5e6e86d0caa

#world_map + 
#  theme_bw() + 
#  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank())
