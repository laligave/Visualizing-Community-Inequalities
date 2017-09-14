# This code shows how to generate the following figures:
# 1) Segments of the road network that have the highest NAIN values (Q80) for local and global scales
# 2) Intersection between the segments of the road network that have the higuest NAIN values (Q80) in both local and global scales
# 3) Raster grid of the area of study cointaining data of total lenght of the overlapping segments in different Depthmap scales 
  #  Choropleth array based on vulnerability quantile ranges for specific local administrative subdivisions in the city

# Open R and install the libraries by typing what follows after #> in the Console:
#> install.packages('sp')
#> install.packages('raster')
#> install.packages('rgdal')
#> install.packages('spatstat')
#> install.packages('maptools')
#> install.packages('RColorBrewer')
#> install.packages('rgeos')
#> install.packages('GISTools')
#> install.packages('ggplot2')
#> install.packages('plyr')
#> install.packages('maps')

# Load libraries
library(sp)
library(raster)
library(rgdal)
library(spatstat)
library(maptools)
library(RColorBrewer)
library(rgeos)
library(GISTools)
library(ggplot2)
library(plyr)
library(maps)

# If you open R or RStudio from the applications set your working directory to where you have your data
setwd("/Users/myUserName/directory/dataFolder")

# Geo-references (change accordingly depending on the area of study)
# To check the correspondent coordinate systems visit http://epsg.io
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

# Load data. Polygons depicting the local administrative subdivisions with the attribute 'Score', which is the index of vulnerability associated to each subdivision 
# Replace the name of the file accordingly
Subdivisions_Vulnerability_shp <- readOGR("Subdivisions_Vulnerability.shp")
proj4string(Subdivisions_Vulnerability_shp) <- CRS(ukgrid)
Subdivisions_Vulnerability_shp <- spTransform(Subdivisions_Vulnerability_shp, CRS(latlong))
# Remove rows with no vulnerability 'Score'
Subdivisions_Vulnerability_shp@data <- Subdivisions_Vulnerability_shp@data[-which(is.na(Subdivisions_Vulnerability_shp@data$Score)), ]

# Load data. Polygon depicting the global boundary of the city of study
# Replace the name of the file accordingly
Study_area_shp <- readOGR("Study_area.shp")
proj4string(Study_area_shp) <- CRS(ukgrid)
bb <- bbox(Study_area_shp)

# Load data. Depthmap results shapefile
# Replace the name of the file accordingly
Depthmap_Results_shp <- readOGR("Depthmap_Results.shp")
proj4string(Depthmap_Results_shp) <- CRS(ukgrid)
Depthmap_Results_shp <- spTransform(Depthmap_Results_shp, CRS(latlong))

# Extract the top quintile Depthmap values (Q80)
# For the research NACH & NAIN values were calculated at 400m,2000m,5000m & n, which correspond to the columns 5:13 in the database. NAIN R400=plot[[6]]
# Change the values i accordingly if required 
plots <- list()
for(i in 5:13) {
  # Calculate top quintile for each NAIN and NACH at different radius
  var.i <- Depthmap_Results_shp@data[,i]
  upper.20 <- quantile(var.i, c(.20, .80))[2]
  # Add index column of top quintile: T/F
  index <- Depthmap_Results_shp@data[,i] >= upper.20
  # Update index column
  Depthmap_Results_shp@data$index <- c(index)
  # Subset by T and return sldf to list
  pl <- subset(Depthmap_Results_shp, Depthmap_Results_shp@data$index == "TRUE")
  plots[[i]] <- pl
  # Remove index column
  Depthmap_Results_shp@data$index <- NULL
}
# Remove null plots
plots <- plots[plots != "NULL"]

# Crop the road network with the bounding box that contains the city of study
b <- bbox(Depthmap_Results_shp)
area_clipped <- crop(Depthmap_Results_shp, extent(b))

# Plot 1 (Segments of the road network that have the highest NAIN values (Q80) for local and global scales)
# Change i in plots[[i]] depending on the graph to be generated
plot_Q80_NAIN_i<- ggplot() + geom_polygon(data=area_clipped, aes(long, lat, group = group), colour = alpha("black", 1/2), 
size = 0.4, fill = 'grey90', alpha = .7)+ geom_line(data=plots[[i]],aes(x=long,y=lat,group=group),colour = alpha("orange1", 1/2))+
theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.4))+
labs(x = " ", y= " ")+theme(axis.text.x= element_text (colour="black",size=20),axis.text.y=element_text(angle=90,hjust=0.5,vjust=0.5,colour="black",size=20))
png("plot_Q80_NAIN_i.png",width=800,height=1000); print(plot_Q80_NAIN_i)
dev.off()

###################################################################################################################

# Creation of the raster grid to store the length of segments intersecting at global and local NAIN scales
rst_gd <- raster(extent(Depthmap_Results_shp), ncol=40, nrow=40, crs=projection(ukgrid))
rst_gd[] <- 1:ncell(rst_gd) # Raster grid
rst_gdpl <- rasterToPolygons(rst_gd) # Polygon grid
proj4string(rst_gdpl) <- CRS(ukgrid)
rstpl_cl <- crop(rst_gdpl, area_clipped) # Clip grid (raster grid / map border)

# Find overlapping segments among NAIN plots
# Change i in plots[[i]] depending on the graphs to be overlapped
ovrlp_NAIN_Ri_Rn <- gIntersection(plots[[6]], plots[[9]]) # NAIN_400 with NAIN_Rn

# Plot 2 (Intersection between the segments of the road network that have the higuest NAIN values (Q80) in both local and global scales)

# Convert the intersection into a Spatial Lines Data Frame (requirement to to be plotted)
ovrlp_NAIN_Ri_Rn_line<-ovrlp_NAIN_Ri_Rn@lineobj 
df_Ri_Rn <- data.frame(len = sapply(1:length(ovrlp_NAIN_Ri_Rn_line), function(i) gLength(ovrlp_NAIN_Ri_Rn_line[i, ]))) 
rownames(df_Ri_Rn) <- sapply(1:length(ovrlp_NAIN_Ri_Rn_line), function(i) ovrlp_NAIN_Ri_Rn_line@lines[[i]]@ID) 
ovrlp_NAIN_Ri_Rn_line_df <- SpatialLinesDataFrame(ovrlp_NAIN_Ri_Rn_line, data = df_Ri_Rn) 

# Print the plot displaying the intersection between NAIN i and NAIN n (Q80)
plot_Intersection_Q80_NAIN_i_n<- ggplot() + geom_polygon(data=area_clipped, aes(long, lat, group = group), colour = alpha("black", 1/2), 
size = 0.4, fill = 'grey90', alpha = .7)+ geom_line(data=ovrlp_NAIN_Ri_Rn_line_df,aes(x=long,y=lat,group=group),colour = alpha("red", 1/2))+
theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.4))+
labs(x = " ", y= " ")+theme(axis.text.y=element_text(angle=90,hjust=0.5,vjust=0.5,size=20),axis.text.x=element_text(size=20))
png("plot_Intersection_Q80_NAIN_i_n.png",width=800,height=1000); print(plot_Intersection_Q80_NAIN_i_n)
dev.off()

# Generate shapefile displaying the intersection between NAIN i and NAIN n (Q80) (requirement to run the code "5_weights_distribution_array.R")
writeOGR(ovrlp_NAIN_Ri_Rn_line_df,dsn="/Users/myUserName/directory/dataFolder",layer ='ovrlp_NAIN_Ri_Rn', driver = 'ESRI Shapefile')

###################################################################################################################

# Find overlapping segments of NAIN i and NAIN n in each raster grid cell
sgmt_ints_1 <- intersect(ovrlp_NAIN_Ri_Rn@lineobj, rstpl_cl)  # Ri/global
sgmt_ints_4 <- intersect(plots[[6]], rstpl_cl) # NAIN R400=plots[[6]], change i=6 if necessary (other scale of analysis)

# Calculation of the length of overlapping segments in each raster grid cell
sgmt_ints_1$length <- round(gLength(sgmt_ints_1, byid=TRUE),2) 
sgmt_ints_4$length <- round(gLength(sgmt_ints_4, byid=TRUE),2) 

# Sum of segment lengths in each raster grid cell
x_1 <- round(tapply(sgmt_ints_1$length, sgmt_ints_1$layer, sum),2) # Ri/global
x_4 <- round(tapply(sgmt_ints_4$length, sgmt_ints_4$layer, sum),2) # Total Ri segments

# Associate calculated values to the raster grids (total lenght of the overlapping segments)
rast_grid_1 <- raster(rst_gd) # Ri/global
rast_grid_1[as.integer(names(x_1))] <- x_1
rast_grid_1@data@values[is.na(rast_grid_1@data@values)] <- 0

rast_grid_4 <- raster(rst_gd) # Total Ri segments
rast_grid_4[as.integer(names(x_4))] <- x_4
rast_grid_4@data@values[is.na(rast_grid_4@data@values)] <- 0

# Convert & crop for plotting
rst_pl_1 <- rasterToPolygons(rast_grid_1) # NAIN Ri overlap NAIN Rn
rst_pl_1 <- crop(rst_pl_1, area_clipped)
#
rst_pl_4 <- rasterToPolygons(rast_grid_4) # Total Ri segments
rst_pl_4 <- crop(rst_pl_4, area_clipped)

# Calculate %: overlaps by scale / total Ri segments (Normalization of the lenght values)

# Data frame of segment-length matrices
m1 <- matrix(rast_grid_1@data@values) #NAIN Ri overlap NAIN Rn
m4 <- matrix(rast_grid_4@data@values) #total Ri segments

# Data frame
m.df <- data.frame(m1,m4)
colnames(m.df) <- c("Ri_Rn","total_Ri")
#add cols with % values
m.df$Ri_Rn_pc <- round(c((m.df$Ri_Rn/m.df$total_Ri)*100),2) #Rn as % total Ri
m.df[is.na(m.df)] <- 0

# Attach % vectors to raster grids; clip rasters
# Overlap Ri/Rn as % of total Ri
rast_grid_8 <- rst_gdpl 
rast_grid_8@data$layer <- m.df$R400_Rn_pc
rast_grid_8 <- crop(rast_grid_8, area_clipped)

# Raster grid of the vulnerability index data
Index_clipped <- Subdivisions_Vulnerability_shp[rst_pl_1, ] 
Index_clipped <- Index_clipped[-which(is.na(Index_clipped@data$Score)), ] 

# List of quantile marker values (vulnerability index data)
q.list <- list()
qunt_seq <- c(seq(0.0, 0.8, 0.2)) # Quintile breaks
var.i <- Index_clipped@data$Score
for(i in qunt_seq) { 
  for(j in length(qunt_seq)) {
    q <- quantile(var.i, c(.20, i))[2]
    q.list[[j]] <- q }
  q.list <- q.list[q.list != "NULL"] }

# Calculate quantile ranges
ind.q5 <- (var.i >= q.list[[5]] & var.i <= max(var.i)) # Quintile 5 (top, most deprived)
ind.q4 <- (var.i >= q.list[[4]] & var.i < q.list[[5]]) #
ind.q3 <- (var.i >= q.list[[3]] & var.i < q.list[[4]]) # 
ind.q2 <- (var.i >= q.list[[2]] & var.i < q.list[[3]]) #
ind.q1 <- (var.i >= min(var.i) & var.i < q.list[[2]]) # Quintile 1 (bottom, least deprived)

# Subset and crop raster grid cells. This depending on the quantile associated to the area they fall within (vulnerability index) 
# Q5 (top quantile)
Index_clipped@data$ind.q5 <- c(ind.q5)
Index_clipped_q5 <- subset(Index_clipped, Index_clipped@data$ind.q5 == "TRUE")
Index_clipped@data$ind.q5 <- NULL
# Q4
Index_clipped@data$ind.q4 <- c(ind.q4)
Index_clipped_q4 <- subset(Index_clipped, Index_clipped@data$ind.q4 == "TRUE")
Index_clipped@data$ind.q4 <- NULL
# Q3
Index_clipped@data$ind.q3 <- c(ind.q3)
Index_clipped_q3 <- subset(Index_clipped, Index_clipped@data$ind.q3 == "TRUE")
Index_clipped@data$ind.q3 <- NULL
# Q2
Index_clipped@data$ind.q2 <- c(ind.q2)
Index_clipped_q2 <- subset(Index_clipped, Index_clipped@data$ind.q2 == "TRUE")
Index_clipped@data$ind.q2 <- NULL
# Q1 (bottom quantile)
Index_clipped@data$ind.q1 <- c(ind.q1)
Index_clipped_q1 <- subset(Index_clipped, Index_clipped@data$ind.q1 == "TRUE")
Index_clipped@data$ind.q1 <- NULL

# Plot 3 (Raster grid of the area of study cointaining data of total lenght of the overlapping segments in different Depthmap scales)
par(mfrow=c(2,3))
par(mar=c(1,1,2,1))
par(oma=c(1,1,2,1))

rast_grid_sample <- rast_grid_8

# Q1
Index_clipped_grid <- crop(rast_grid_sample, Index_clipped_q1) 
Index_clipped_q <- Index_clipped_q1 
# Stats
min.q <- round(min(Index_clipped_q@data$Score,3)) 
max.q <- round(max(Index_clipped_q@data$Score,3))
stdv.q <- round(sd(Index_clipped_grid@data$layer,2))
mean.q <- round(mean(Index_clipped_grid@data$layer,2))
# Create data frame
stats.df_Ri_Rn <- data.frame("q1", min.q, max.q, stdv.q, mean.q, stringsAsFactors = F) #
# Plot
shades <- auto.shading(Index_clipped_grid@data$layer, n=9, cutter = rangeCuts, cols = add.alpha(brewer.pal(9, "Blues"),0.8))
shades$cols[1] <- "transparent"
plot(area_clipped, border="transparent")
choropleth(Index_clipped_grid, Index_clipped_grid@data$layer, shading=shades, border=NA, lwd=0.4, bg="transparent", add=T)
plot(Index_clipped_grid, border="grey96", add=T)
plot(area_clipped, border="grey75", add=T)
# Title
t <- paste('Index scores: ', min.q, '-', max.q, '\n Mean segment length: ', mean.q,  '; SD: ', stdv.q, sep='')
title(t, line=-0.1, cex =0.2)
t <- NULL
# Erase values to plot next quantile
Index_clipped_grid <- NULL
Index_clipped_q <- NULL

# Q2
Index_clipped_grid <- crop(rast_grid_sample, Index_clipped_q2) 
Index_clipped_q<- Index_clipped_q2
# Stats
min.q <- round(min(Index_clipped_q@data$Score),3) 
max.q <- round(max(Index_clipped_q@data$Score),3) 
stdv.q <- round(sd(Index_clipped_grid@data$layer),2)
mean.q <- round(mean(Index_clipped_grid@data$layer),2)
# Create data frame
stats.df_Ri_Rn[2,] <- c("q2", min.q, max.q, stdv.q, mean.q) 
# Plot
shades <- auto.shading(Index_clipped_grid@data$layer, n=9, cutter = rangeCuts, cols = add.alpha(brewer.pal(9, "Blues"),0.8))
shades$cols[1] <- "transparent"
plot(area_clipped, border="transparent")
choropleth(Index_clipped_grid, Index_clipped_grid@data$layer, shading=shades, border=NA, lwd=0.4, bg="transparent", add=T)
plot(Index_clipped_grid, border="grey96", add=T)
plot(area_clipped, border="grey75", add=T)
# Title
t <- paste('Index scores: ', min.q, '-', max.q, '\n Mean segment length: ', mean.q,  '; SD: ', stdv.q, sep='')
title(t, line=-0.1, cex =0.2)
t <- NULL
# Erase values to plot next quantile
Index_clipped_grid <- NULL
Index_clipped_q <- NULL

# Q3
Index_clipped_grid <- crop(rast_grid_sample, Index_clipped_q3) 
Index_clipped_q <- Index_clipped_q3
# Stats
min.q <- round(min(Index_clipped_q@data$Score),3) 
max.q <- round(max(Index_clipped_q@data$Score),3) 
stdv.q <- round(sd(Index_clipped_grid@data$layer),2)
mean.q <- round(mean(Index_clipped_grid@data$layer),2)
# Create data frame
stats.df_Ri_Rn[3,] <- c("q3", min.q, max.q, stdv.q, mean.q)
# Plot
shades <- auto.shading(Index_clipped_grid@data$layer, n=9, cutter = rangeCuts, cols = add.alpha(brewer.pal(9, "Blues"),0.8))
shades$cols[1] <- "transparent"
plot(area_clipped, border="transparent")
choropleth(Index_clipped_grid, Index_clipped_grid@data$layer, shading=shades, border=NA, lwd=0.4, bg="transparent", add=T)
plot(Index_clipped_grid, border="grey96", add=T)
plot(area_clipped, border="grey75", add=T)
# Title
t <- paste('Index scores: ', min.q, '-', max.q, '\n Mean segment length: ', mean.q,  '; SD: ', stdv.q, sep='')
title(t, line=-0.1, cex =0.2)
t <- NULL
# Erase values to plot next quantile 
Index_clipped_grid <- NULL
Index_clipped_q <- NULL

# Q4
Index_clipped_grid <- crop(rast_grid_sample, Index_clipped_q4) 
Index_clipped_q <- Index_clipped_q4
# Stats
min.q <- round(min(Index_clipped_q@data$Score),3)
max.q <- round(max(Index_clipped_q@data$Score),3) 
stdv.q <- round(sd(Index_clipped_grid@data$layer),2)
mean.q <- round(mean(Index_clipped_grid@data$layer),2)
# Create data frame
stats.df_Ri_Rn[4,] <- c("q4", min.q, max.q, stdv.q, mean.q)
# Plot
shades <- auto.shading(Index_clipped_grid@data$layer, n=9, cutter = rangeCuts, cols = add.alpha(brewer.pal(9, "Blues"),0.8))
shades$cols[1] <- "transparent"
plot(area_clipped, border="transparent")
choropleth(Index_clipped_grid, Index_clipped_grid@data$layer, shading=shades, border=NA, lwd=0.4, bg="transparent", add=T)
plot(Index_clipped_grid, border="grey96", add=T)
plot(area_clipped, border="grey75", add=T)
# Title
t <- paste('Index scores: ', min.q, '-', max.q, '\n Mean segment length: ', mean.q,  '; SD: ', stdv.q, sep='')
title(t, line=-0.1, cex =0.2)
t <- NULL
# Erase values to plot next quantile
Index_clipped_grid <- NULL
Index_clipped_q <- NULL

# Q5
Index_clipped_grid <- crop(rast_grid_sample, Index_clipped_q5) 
Index_clipped_q <- Index_clipped_q5
#stats
min.q <- round(min(Index_clipped_q@data$Score),3) 
max.q <- round(max(Index_clipped_q@data$Score),3) 
stdv.q <- round(sd(Index_clipped_grid@data$layer),2)
mean.q <- round(mean(Index_clipped_grid@data$layer),2)
# Create data frame
stats.df_R400_Rn[5,] <- c("q5", min.q, max.q, stdv.q, mean.q) #
# Plot
shades <- auto.shading(Index_clipped_grid@data$layer, n=9, cutter = rangeCuts, cols = add.alpha(brewer.pal(9, "Blues"),0.8))
shades$cols[1] <- "transparent"
plot(area_clipped, border="transparent")
choropleth(Index_clipped_grid, Index_clipped_grid@data$layer, shading=shades, border=NA, lwd=0.4, bg="transparent", add=T)
plot(Index_clipped_grid, border="grey96", add=T)
plot(area_clipped, border="grey75", add=T)
# Title
t <- paste('Index scores: ', min.q, '-', max.q, '\n Mean segment length: ', mean.q,  '; SD: ', stdv.q, sep='') #--CHANGE!!!!!!!!!! 
title(t, line=-0.1, cex =0.2)
t <- NULL
# Erase values
Index_clipped_grid <- NULL
Index_clipped_q <- NULL

# Legend
plot(area_clipped, border="transparent")
i <- area_clipped@bbox[1]
j <- area_clipped@bbox[4]
choro.legend(i, j, shades, title = "Ration Ri / overlaps (%)", cex = 1.7)

# Export plot 
dev.off()