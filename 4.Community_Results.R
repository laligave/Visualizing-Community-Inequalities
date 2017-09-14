# This code shows how to generate the final maps, which integrate: 
# 1) Array of positive and negative field data points located by the participants on top of movement infraestructures, such as: Roads, Junctions, Roundabouts
# 2) Intersection between the segments of the road network that have the higuest NAIN values (Q80) in both local and global scales
# 3) Road street network
# 4) Vulnerability decile ranges for specific local administrative subdivisions in the city (greyscale)

# Open R and install the libraries by typing what follows after #> in the Console:
#> install.packages('GISTools')
#> install.packages('RColorBrewer')
#> install.packages('maps')
#> install.packages('sampSurf')
#> install.packages('gridExtra')
#> install.packages('grid')
#> install.packages('ggplot2')
#> install.packages('plyr')
#> install.packages('rgdal')

# Load libraries
library(GISTools)
library(RColorBrewer)
library(maps)
library(sampSurf)
library(gridExtra)
library(grid)
library(ggplot2)
library(plyr)
library(rgdal)

# If you open R or RStudio from the applications set your working directory to where you have your data
setwd("/Users/myUserName/directory/dataFolder")

# Geo-references (change accordingly depending on the area of study)
# To check the correspondent coordinate systems visit http://epsg.io
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

# Load data. Depthmap results shapefile
# Replace the name of the file accordingly
Depthmap_Results_shp <- readOGR("Depthmap_Results.shp")
proj4string(Depthmap_Results_shp) <- CRS(ukgrid)
Depthmap_Results_shp <- spTransform(Depthmap_Results_shp, CRS(latlong))

# Load data. Intersection between the segments of the road network that have the higuest NAIN values (Q80) in both local and global scales
# Generate file using the code "4.NAIN_global&local_Intersection.R" and locate the resulting shapefile in the current working directory
Ri_ovrlp_Rn <- readShapeLines("ovrlp_NAIN_Ri_Rn.shp")
proj4string(Ri_ovrlp_Rn) <- CRS(ukgrid)
Ri_ovrlp_Rn <- spTransform(Ri_ovrlp_Rn, CRS(latlong))

# Load data. Polygons depicting the local administrative subdivisions with the attribute 'Score', which is the index of vulnerability associated to each subdivision 
# Replace the name of the file accordingly
Subdivisions_Vulnerability_shp <- readOGR("Subdivisions_Vulnerability.shp")
proj4string(Subdivisions_Vulnerability_shp) <- CRS(ukgrid)
Subdivisions_Vulnerability_shp <- spTransform(Subdivisions_Vulnerability_shp, CRS(latlong))
# Remove rows with no vulnerability 'Score'
Subdivisions_Vulnerability_shp@data <- Subdivisions_Vulnerability_shp@data[-which(is.na(Subdivisions_Vulnerability_shp@data$Score)), ]
 
# Load data. Results of the workshops run in each of the areas of study (In this case, 10 schools)
# Specifically the attributes of the data gathered in the workshops corresponds to:
# location X of the sticker, location Y of the sticker, ID of the workshop, ID of the Participant, 
# Age, Gender,Icon (positive/negative perception),Interpretation Icon, Type, Place 
# The attribute Type allows to classify the places into categories: hospital, roads, etcetera
# The attribute Place should be consistent in the database, so therefore it is possible to establish the relevance of the landmark (amount of times it was pointed out)
School_1.df <- read.csv("School_1.csv", skip = 1, header = F)[,1:10]
School_2.df <- read.csv("School_2.csv", skip = 1, header = F)[,1:10]
School_3.df <- read.csv("School_3.csv", skip = 1, header = F)[,1:10]
School_4.df <- read.csv("School_4.csv", skip = 1, header = F)[,1:10] 
School_5.df <- read.csv("School_5.csv", skip = 1, header = F)[,1:10] 
School_6.df <- read.csv("School_6.csv", skip = 1, header = F)[,1:10]
School_7.df <- read.csv("School_7.csv", skip = 1, header = F)[,1:10]
School_8.df <- read.csv("School_8.csv", skip = 1, header = F)[,1:10]
School_9.df <- read.csv("School_9.csv", skip = 1, header = F)[,1:10]
School_10.df <- read.csv("School_10.csv", skip = 1, header = F)[,1:10]

# Compile files in a global database and name columns accordingly
collated_data <- rbind(School_1.df, School_2.df, School_3.df, School_4.df, School_5.df, School_6.df, School_7.df, School_8.df, School_9.df, School_10.df)
colnames(collated_data) <- c("X","Y","ID_Workshop","ID_Participant","Age","Gender","Icon","Interpretation_Icon","Type","Place")

# List of the dataframes containing the field data collected in each school
d.list <- list(School_1.df, School_2.df, School_3.df, School_4.df, School_5.df, School_6.df, School_7.df, School_8.df, School_9.df, School_10.df)
nm.list <- list("School_1","School_2","School_3","School_4","School_5","School_6","School_7","School_8","School_9","School_10") 

# Spatial database containing the landmarks pointed out by the participants
collated_data.spdf <- data.frame(collated_data$X, collated_data$Y, collated_data$Icon, collated_data$Type)
colnames(collated_data.spdf) <- c("X", "Y", "Icon", "Type")
coordinates(collated_data.spdf) <- ~X+Y
proj4string(collated_data.spdf)  <- CRS(latlong)

# Subset of the spatial database (Movement infraestructures)
# The selection depends on the type of infraestructure tipologies retrieved during the workshops (If necessary change "road" "junction" or "roundabout" accordingly)
collated_urb_infrst.spdf <- subset(collated_data.spdf, collated_data.spdf@data$Type %in% c("road","junction","roundabout"))
# Classification of emoticons in the classes positive or negative. Change "x" depending on the notations utilized to distinguish the different icons
collated_urb_infrst.spdf@data$emots_v <-  ifelse(collated_urb_infrst.spdf@data$emoticon %in% c("x","x","x","x","x"), "pos",
                                          ifelse(collated_urb_infrst.spdf@data$emoticon %in% c("x","x","x","x","x","x","x"),"neg"))
# Subset of data (positive or negative perceptions associated to movements infraestructures)
emots_neg.spdf <- subset(collated_urb_infrst.spdf, emots_v %in% "neg")
emots_pos.spdf <- subset(collated_urb_infrst.spdf, emots_v %in% "pos")

# Colours for plotting vulnerability decile ranges for specific local administrative subdivisions in the city (greyscale)
q.list <- list()
qunt_seq <- c(seq(0.0, 0.9, 0.1)) # Decile breaks
var.i <- Subdivisions_Vulnerability_shp@data$Score
for(i in qunt_seq) { 
  for(j in length(qunt_seq)) {
    q <- quantile(var.i, c(.1, i))[2]
    q.list[[j]] <- q }
  q.list <- q.list[q.list != "NULL"] }

# Greyscale colour ramp
col_rmp <- colorRamp(c("white", "black"))
greys <- rgb(col_rmp(seq(0,1, length=10)), max=255)

# Apply greyscale range based on Vulnerability Index deciles
Subdivisions_Vulnerability_shp@data$cols <- ifelse(var.i >= q.list[[10]] & var.i <= max(var.i), greys[10],
                                            ifelse(var.i >= q.list[[9]] & var.i < q.list[[10]], greys[9],
                                            ifelse(var.i >= q.list[[8]] & var.i < q.list[[9]], greys[8],
                                            ifelse(var.i >= q.list[[7]] & var.i < q.list[[8]], greys[7],
                                            ifelse(var.i >= q.list[[6]] & var.i < q.list[[7]], greys[6],
                                            ifelse(var.i >= q.list[[5]] & var.i < q.list[[6]],  greys[5],
                                            ifelse(var.i >= q.list[[4]] & var.i < q.list[[5]], greys[4],
                                            ifelse(var.i >= q.list[[3]] & var.i < q.list[[4]], greys[3],
                                            ifelse(var.i >= q.list[[2]] & var.i < q.list[[3]], greys[2],
                                            ifelse(var.i >= min(var.i) & var.i < q.list[[2]], greys[1], 
                                            NA))))))))))

# Fortify & join in order to plot (Vulnerability decile ranges for specific local administrative subdivisions in the city)
Subdivisions_Vulnerability_shp.df <- data.frame(id=rownames(Subdivisions_Vulnerability_shp@data), values=sample(1:10,length(Subdivisions_Vulnerability_shp),replace=T), 
                                                Subdivisions_Vulnerability_shp@data, stringsAsFactors=F)
Subdivisions_Vulnerability_shp.ft   <- fortify(Subdivisions_Vulnerability_shp)
Subdivisions_Vulnerability_shp_join <- join(Subdivisions_Vulnerability_shp.ft, Subdivisions_Vulnerability_shp.df, by="id")

# List of spatial points data frames (Positive and negative field data points located by the participants on top of movement infraestructures)
sp.list <- list()
  for(i in 1:length(d.list)) {
  spdf1 <- d.list[[i]]
  colnames(spdf1) <- c("X","Y","ID_Workshop","ID_Participant","Age","Gender","Icon","Interpretation_Icon","Type","Place")
  coordinates(spdf1) <- ~X+Y
  proj4string(spdf1) <- CRS(latlong)
  sp.list[[i]] <- spdf1
  }

# List of bounding boxes (containing the spatial points retreived in each of the workshops)
bx.list <- list()
for(j in 1:length(sp.list)) {
  h <- gConvexHull(sp.list[[j]], byid=FALSE, id = NULL)
  bbx <- bbox(h)
  bbx.s <- bboxToPoly(bbx)
  proj4string(bbx.s)  <-  CRS(latlong)
  bx.list[[j]] <- bbx.s
  bx.list[[j]]$label <- nm.list[[j]]
}

# List of negative and positive spatial points data frames cropped by each of the bounding boxes (bx.list)
neg_wgts.list <- list()
for(n in 1:length(bx.list)) {
  neg_wgts.spdf <- crop(emots_neg.spdf, bx.list[[n]]) 
  neg_wgts.list[[n]] <- neg_wgts.spdf
}

pos_wgts.list <- list()
for(n in 1:length(bx.list)) {
  pos_wgts.spdf <- crop(emots_pos.spdf, bx.list[[n]])
  pos_wgts.list[[n]] <- pos_wgts.spdf
}

# List of the road network within each bounding box 
netw.list <- list()
for(q in 1:length(bx.list)) {
  netw_crop <- crop(Depthmap_Results_shp, bx.list[[q]])
  netw.ft <- fortify(netw_crop)
  netw.df <- data.frame(id=rownames(netw_crop@data), values=sample(1:10,length(netw_crop),replace=T), 
                        netw_crop@data, stringsAsFactors=F)
  netw_join <- join(netw.ft, netw.df, by="id")
  netw.list[[q]] <- netw_join
}

# List of the intersection between the segments of the road network that have the higuest NAIN values (Q80) in both local and global scales 
# For each bounding box (The file Ri_ovrlp_Rn is generated running the script "4.NAIN_global&local_Intersection.R")
ovrlp.list <- list() 
for(q in 1:length(bx.list)) {
  ovrlp_crop <- crop(Ri_ovrlp_Rn, bx.list[[q]]) 
  ovrlp.ft <- fortify(ovrlp_crop)
  ovrlp.df <- data.frame(id=rownames(ovrlp_crop@data), values=sample(1:10,length(ovrlp_crop),replace=T), 
                         ovrlp_crop@data, stringsAsFactors=F)
  ovrlp_join <- join(ovrlp.ft, ovrlp.df, by="id")
  ovrlp.list[[q]] <- ovrlp_join
}

# List of administrative subdivisions within each bounding box
join.list <- list()
for(n in 1:length(bx.list)) {
bkgd_Subdivisions_Vulnerability_shp <- crop(Subdivisions_Vulnerability_shp, bx.list[[n]])
bkgd_Subdivisions_Vulnerability_shp.ft <- fortify(bkgd_Subdivisions_Vulnerability_shp)
bk_join.df <- data.frame(id=rownames(bkgd_Subdivisions_Vulnerability_shp@data), values=sample(1:10,length(bkgd_Subdivisions_Vulnerability_shp),replace=T), 
                         bkgd_Subdivisions_Vulnerability_shp@data, stringsAsFactors=F)
bkgd_Subdivisions_Vulnerability_shp_join <- join(bkgd_Subdivisions_Vulnerability_shp.ft, bk_join.df, by="id")
join.list[[n]] <- bkgd_Subdivisions_Vulnerability_shp_join
}

# List of plots (for each area of study)
plot.list <- list()
for(i in 1:10) {
  pl <- ggplot() + geom_polygon(data=join.list[[i]], aes(long, lat, group=group), fill=join.list[[i]]$cols)+
      geom_line(data=netw.list[[i]], aes(long, lat, group=group), colour="wheat", size=0.3) +
      geom_point(aes(x = X, y = Y), data = data.frame(coordinates(pos_wgts.list[[i]])),
                 alpha = .7, color="orange", size = 3) +
      geom_point(aes(x = X, y = Y), data = data.frame(coordinates(neg_wgts.list[[i]])),
                 alpha = .7, color="skyblue", size = 3) +
      geom_line(data=ovrlp.list[[i]], aes(long, lat, group=group), alpha = .7, colour="red", size=0.8) +
      scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
      ggtitle(nm.list[[i]])
  plot.list[[i]] <- pl
}

# Plots (change i depending on the graphs that should be generated. Specify areas of study to plot)
par(mfrow=c(4,3))
grid.arrange(plot.list[[1]],plot.list[[2]],plot.list[[3]],plot.list[[4]],
plot.list[[5]],plot.list[[6]],plot.list[[7]],plot.list[[8]],plot.list[[9]],plot.list[[10]])