# This code shows how to generate colour-code maps displaying the NAIN results of Depthmap models

# Open R and install the libraries by typing what follows after #> in the Console:
#> install.packages('plyr')
#> install.packages('GISTools')
#> install.packages('rgeos')
#> install.packages('ggplot2')
#> install.packages('sampSurf')
#> install.packages('RColorBrewer')
#> install.packages('sp')
#> install.packages('gridExtra')
#> install.packages('grid')
#> install.packages('maps')
#> install.packages('mapdata')

# Load libraries
library(plyr)
library(GISTools)
library(rgeos)
library(ggplot2)
library(sampSurf) 
library(RColorBrewer)
library(sp)
library(gridExtra)
library(grid)
library(maps)
library(mapdata)

# If you open R or RStudio from the applications set your working directory to where you have your data
setwd("/Users/myUserName/directory/dataFolder")

# Geo-references (change accordingly depending on the area of study)
# To check the correspondent coordinate systems visit http://epsg.io
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

# Load data. Area of study shapefile
# Replace the name of the file accordingly
Boundary_shp <- readOGR("Boundary.shp")
proj4string(Boundary_shp) <- CRS(ukgrid)
Boundary_shp <- spTransform(Boundary_shp, CRS(latlong))
# Fortify
Boundary_fortify <- fortify(Boundary_shp)

# Load data. Depthmap results shapefile 
# Replace the name of the file accordingly
Depthmap_Results_shp <- readOGR("Depthmap_Results.shp")
proj4string(Depthmap_Results_shp) <- CRS(ukgrid)
Depthmap_Results_shp <- spTransform(Depthmap_Results_shp, CRS(latlong))
# Fortify
Depthmap_Results_shp@data$id<-rownames(Depthmap_Results_shp@data) #Create new column with unique ID 
Depthmap_Results_fortify <- fortify(Depthmap_Results_shp) #Creates the data, but only with the geometric features, still the attributes have to be added
Depthmap_Results_fortify <-join(Depthmap_Results_fortify ,Depthmap_Results_sh@data,by="id")

# Depthmap arrays
# Change columns according to the structure of the database. 
# In this case NAIN values for 400m, 2000m, 5000m & n are contained in columns 12:15
plot_maps <- list()
for(i in 12:15) {
  var.i <- data_merged[,i]
  # Quantile levels (8ths)
  highest <- quantile(var.i, c(.125, 1.0))[2]
  upp_high <- quantile(var.i,c(.125, .875))[2]
  upp_upp <- quantile(var.i,c(.125, .75))[2]
  upp_midd <- quantile(var.i,c(.125, .625))[2]
  midd_midd <- quantile(var.i,c(.125, .50))[2]
  midd_low <- quantile(var.i,c(.125, .375))[2]
  low_low <- quantile(var.i,c(.125, .25))[2]
  lowest <- quantile(var.i,c(.125, 1.0))[1]
  # Quantile ranges (Highest/descending order)
  quant1 <- (var.i <= highest & var.i > upp_high) 
  quant2 <- (var.i <= upp_high & var.i > upp_upp)
  quant3 <- (var.i <= upp_upp & var.i > upp_midd)
  quant4 <- (var.i <= upp_midd & var.i > midd_midd)
  quant5 <- (var.i <= midd_midd & var.i > midd_low)
  quant6 <- (var.i <= midd_low & var.i > low_low)
  quant7 <- (var.i <= low_low & var.i > lowest)
  quant8 <- (var.i <= lowest)
  # Colours
  cols <- rep("grey20", nrow(Bllrv_dpthm@data)) # Basemap
  # Lines colour-coded by quantile ranges
  cols[quant1] <- rgb(240,10,40, alpha=225, max=255)  # Red
  cols[quant2] <- rgb(240,114,12, alpha=225, max=255) # Dark orange
  cols[quant3] <- rgb(240,160,12, alpha=225, max=255) # Light orange
  cols[quant4] <- rgb(232,240,12, alpha=200, max=255) # Yellow green
  cols[quant5] <- rgb(111,240,12, alpha=200, max=255) # Light green
  cols[quant6] <- rgb(18,151,45, alpha=150, max=255) # Dark green
  cols[quant7] <- rgb(100,197,239, alpha=150, max=255) # Light blue
  cols[quant8] <- rgb(5,113,195, alpha=150, max=255) # Dark blue
  # Lines
  lwd.s <- rep(2.5, nrow(Bllrv_dpthm))
  # Plot loop
  pl <- ggplot() + geom_polygon(data=Boundary_fortify, aes(long, lat, group = group), colour = alpha("grey20", 1/2), 
  size = 0.7, fill = 'grey10', alpha = .7) + coord_map(xlim = bx[1,] ,ylim = bx[2,]) +
  geom_path(data=data_merged, size=1, aes(x=long,y=lat,group=group), color=factor(cols)) + 
  labs(title = colnames(data_merged[i]))
  plot_maps[[i]] <- pl
}
plot_maps <- plot_maps[plot_maps != "NULL"]

# Print plots (change/delete plot_maps[[i]] depending on the required graph)
grid.arrange(plot_maps[[1]], plot_maps[[2]],plot_maps[[3]],plot_maps[[4]], nrow=1)
grid.text("Depthmap array (NAIN)", just = "center" , x=.5, y=.75, vp=vplayout(1,1))