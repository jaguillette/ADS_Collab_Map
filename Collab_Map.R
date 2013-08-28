library(maps)
library(geosphere)
library(ggplot2)
library(mapproj)
library(tcltk)

setwd(tk_choose.dir(getwd(), "Choose the folder you would like to save your map to."))

#====START DATA====
#names and reads csv data source
#csvName <- readline(file.choose())

affil <- read.csv(file.choose(), header=TRUE)

#Sets central point, then removes it from data analysis.
base=affil[1,]
affil=affil[-1,]

#takes out outlier with 94 collaborations
#affil=affil[-1,]

#orders results so less prominent affiliations are drawn first.
#affil=affil[order(affil$totalCount),]

#sets maximum count
maxCount <- max(affil$totalCount)
#====END DATA====

#====START PARAMETERS====
PaletteChoices = c("Night", "Doodle", "Technical")
Palette <- select.list(PaletteChoices, preselect = NULL, multiple = FALSE, title = "Select Map View", graphics = TRUE)

LatLongChoices=c('World', 'United States', "New England", "Europe")
LatLongView = select.list(LatLongChoices, preselect = NULL, multiple = FALSE, title = "Select Map View", graphics = TRUE)

MapChoices = c("Dot", "Line")
MapType <- select.list(MapChoices, preselect = NULL, multiple = FALSE, title = "Select Map View", graphics = TRUE)
#====END PARAMETERS====

#====START COLORS====
#colors used in the map are defined here. Color sets can be switched.
switch(Palette,
       'Night'={
         low_collab <- "#736051"
         high_collab <- "#FEFEE6"
         land <- '#0A1640'
         sea <- '#01021E'
         border <- '#020727'
       },
       'Doodle'={
         low_collab <- "#d18f45"
         high_collab <- "#9d0b00"
         land <- '#fac263'
         sea <- '#6ab58e'
         border <- '#6a5a40'
       },
       "Technical"={
         low_collab <- "#D1ABD6"
         high_collab <- "#9A07AD"
         land <- '#d5f0d4'
         sea <- '#adbef9'
         border <- '#b1b1b1'
       })
pal <- colorRampPalette(c(low_collab, high_collab))
colors <- pal(100)
affil$colindex <- NULL
for (i in 1:nrow(affil)) {
  affil$colindex[i] = colors[round((affil[i,]$totalCount / maxCount) * length(colors))]
}
#====END COLORS====

#====START MAP====
#creates a base map of the world that includes US state boundaries.
state_data <- map_data("state")
world_data <- map_data("world")
world_map <- ggplot() + 
  geom_polygon(data=world_data, aes(x=long, y=lat, group=group), fill=land, colour=border, size=.04) +
  geom_polygon(data=state_data, aes(x=long, y=lat, group=group), fill=land, colour=border, size=.04) + 
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = sea),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
#====END MAP====

#====START VIEW====
#switch for different world views.
switch(LatLongView,
       'United States'={
         xlim <- c(-130.07813, -63.085938)
         ylim <- c(23.479182, 50.082393)
         asp_ratio <- (xlim[2]-xlim[1])/(ylim[2]-ylim[1])
         m_width <- 1000
         m_height <- round(1000/asp_ratio)
         world_map <- world_map + coord_fixed() + coord_cartesian(xlim=xlim, ylim=ylim)
       },
       'Europe'={
         xlim <- c(-21.621094, 43.769531)
         ylim <- c(35.460670, 75.501722)
         asp_ratio <- (xlim[2]-xlim[1])/(ylim[2]-ylim[1])
         m_width <- 1000
         m_height <- round(1000/asp_ratio)
         world_map <- world_map + coord_fixed() + coord_cartesian(xlim=xlim, ylim=ylim)
       },
       'World'={
         xlim <- c(-180, 180)
         ylim <- c(-90, 90)
         asp_ratio <- (xlim[2]-xlim[1])/(ylim[2]-ylim[1])
         m_width <- 1000
         m_height <- round(1000/asp_ratio)
       },
       'New England'={
         xlim <- c(-73.907471, -66.524658)
         ylim <- c(40.980573, 47.577128)
         asp_ratio <- (xlim[2]-xlim[1])/(ylim[2]-ylim[1])
         m_width <- 1000
         m_height <- round(1000/asp_ratio)
         world_map <- world_map + coord_fixed() + coord_cartesian(xlim=xlim, ylim=ylim)
       })
#====END VIEW====

#====START MAP SWITCH====
switch(MapType,
       'Line'={
         #=====START LINE MAP=====
         #Draws lines from central point described
         for (j in 1:length(affil$lat)) {
           drawLat <- affil[j,]$lat
           drawlong <- affil[j,]$long
           if((xlim[1]<=drawlong & drawlong<=xlim[2])&(ylim[1]<=drawLat & drawLat<=ylim[2])){
             inter <- gcIntermediate(c(base$long, base$lat), c(drawlong, drawLat), n=50, breakAtDateLine=TRUE, addStartEnd=TRUE)
             if(length(inter)==2){
               inter_01 <- as.data.frame(inter[[1]])
               inter_02 <- as.data.frame(inter[[2]])
               names(inter_01) <- c("long", "lat")
               names(inter_02) <- c("long", "lat")
               world_map <- world_map + geom_line(data=inter_01, aes(x=long, y=lat), color=affil$colindex[j], lwd=.25)
               world_map <- world_map + geom_line(data=inter_02, aes(x=long, y=lat), color=affil$colindex[j], lwd=.25)
             }
             else {
               #lines(inter, col=colors[colindex], lwd=1.5)
               inter <- as.data.frame(inter)
               names(inter) <- c("long", "lat")
               world_map <- world_map + geom_line(data=inter, aes(x=long, y=lat), color=affil$colindex[j], lwd=.25)
             }
           }
         }
         #====END LINE MAP====
       },
       'Dot'={
         #====START DOT MAP====
         affil_subset = subset(affil, lat>=ylim[1] & lat<=ylim[2] & long>=xlim[1] & long<=xlim[2])
         world_map <- world_map + 
           geom_point(data=affil_subset, aes(x=long, y=lat, size=totalCount, colour=totalCount)) + 
           scale_colour_gradient(low=low_collab, high=high_collab)
         #====END DOT MAP====
       })
#====END MAP SWITCH====

#====START MAKE FILE====
#Makes png file, draws map in it.
filename = paste(LatLongView, MapType, Palette, collapse="_")
filename = paste(filename, ".png", collapse='')
png(file=filename,width=m_width, height=m_height)
world_map
dev.off()
#====END MAKE FILE====