# This code shows how to generate the knn maps of the most relevant landmarks identified by the community
# In detail:
# 1) Each landmark is connected to the 4 closest landmarks in the area of study (knn)
# 2) Each landmark is coloured according to its typology (school, road, hospital...)
# 3) Each landmark has a size associated to its importance (Amount of times it was selected during the workshops) 
# 4) Each landmark has a label specifying its location

# Open R and install the libraries by typing what follows after #> in the Console:
#> install.packages('sp')
#> install.packages('spdep')
#> install.packages('rgdal')
#> install.packages('plyr')
#> install.packages('GISTools')
#> install.packages('igraph')
#> install.packages('sampSurf')
#> install.packages('RgoogleMaps')
#> install.packages('ggmap')
#> install.packages('ggplot2')
#> install.packages('qdapRegex')
#> install.packages('maps')
#> install.packages('maptools')
#> install.packages('RColorBrewer')
#> install.packages('visNetwork')

# Load libraries
library(sp)
library(spdep)
library(rgdal)
library(plyr)
library(GISTools)
library(igraph)
library(sampSurf)
library(RgoogleMaps)
library(ggmap) 
library(ggplot2)
library(qdapRegex)
library(maps)
library(maptools)
library(RColorBrewer)
library(regexr)
library(visNetwork)

# If you open R or RStudio from the applications set your working directory to where you have your data
setwd("/Users/myUserName/directory/dataFolder")

# Geo-references (change accordingly depending on the area of study)
# To check the correspondent coordinate systems visit http://epsg.io
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

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
#In order to display results for all the schools, a global dataframe is built
Global.df<-rbind(alsop.df, bllrv.df, blvdr.df, BlCt.df, gtcr.df, hlsd.df, NLvAc.df, SvSl.df, StHld.df, StJls.df)

# Compile files in a global database and name columns accordingly
collated_data <- rbind(School_1.df, School_2.df, School_3.df, School_4.df, School_5.df, School_6.df, School_7.df, School_8.df, School_9.df, School_10.df)
colnames(collated_data) <- c("X","Y","ID_Workshop","ID_Participant","Age","Gender","Icon","Interpretation_Icon","Type","Place")

# List of the dataframes containing the field data collected in each school
d.list <- list(School_1.df, School_2.df, School_3.df, School_4.df, School_5.df, School_6.df, School_7.df, School_8.df, School_9.df, School_10.df, Global.df)
nm.list <- list("School_1","School_2","School_3","School_4","School_5","School_6","School_7","School_8","School_9","School_10","Global") 

# List of spatial points data frames
sp.list <- list()
for(i in 1:length(d.list)) {
  spdf1 <- d.list[[i]]
  colnames(spdf1) <- c("X","Y","ID_Workshop","ID_Participant","Age","Gender","Icon","Interpretation_Icon","Type","Place")
  coordinates(spdf1) <- ~X+Y
  proj4string(spdf1) <- CRS(latlong)
  sp.list[[i]] <- spdf1
}

# List of bounding boxes containing the area of study associated to each workshop
bx.list <- list()
for(j in 1:length(sp.list)) {
  h <- gConvexHull(sp.list[[j]], byid=FALSE, id = NULL)
  bbx <- bbox(h)
  bbx.s <- bboxToPoly(bbx)
  proj4string(bbx.s)  <-  CRS(latlong)
  bx.list[[j]] <- bbx.s
  bx.list[[j]]$label <- nm.list[[j]]
}

# Colours associated to each landmark according to its typology

colours <- adjustcolor(c("darkblue", "darkorchid3","violet","red", "steelblue", "dark orange",
                         "golden rod","yellow green","yellow","green","forest green", "khaki","spring green",
                         "dark turquoise", "deep sky blue", "wheat2", "medium blue","dodger blue","dark magenta",
                         "sandy brown","linen","moccasin","chart reuse","olivedrab2",
                         "medium violet red"), alpha=0.65)

type <- collated_data$Type

collated_data$cols <-ifelse(grepl("(police )|(emergency services)", type), colours[1],
                 ifelse(type == "crossing", colours[2],
                  ifelse(grepl("(school)|(college)|(nursery)",type), colours[3],
                  ifelse(grepl("(bus stop)|(buses)|(transport)|(railway)", type), colours[5],
                  ifelse(grepl("(jnct_)|(hill)|(road)|(underpass)|(route)|(roundabout)|(junction)|(flyover)|(underpass)|(pathway)|(alleyway)|(cycle)|(bridge)", type), colours[4],
                  ifelse(type == "cafe/restaurant", colours[7],
                  ifelse(type == "cemetery", colours[6],
                  ifelse(type == "shops", colours[8],
                  ifelse(grepl("(open space)|(woodland)|(allotments)|(gardens)", type), colours[9],
                  ifelse(grepl("(leisure)|(sports)|(sport)", type), colours[10],
                  ifelse(type == "area", colours[11],
                  ifelse(type == "residential", colours[16],
                  ifelse(type == "bus route", colours[13], 
                  ifelse(type == "library", colours[14],
                  ifelse(grepl(" worship", type), colours[15],
                  ifelse(type == "cemetery", colours[25],
                  ifelse(type == "health centre", colours[17],
                  ifelse(type == "residence", colours[18],
                  ifelse(type == "stadium", colours[25],
                  ifelse(type == "industry", colours[24],
                  NA))))))))))))))))))))

# Find any missing values (use this line of code to verify that all the landmarks in the dataset are classified according to a specific typology: schools, hospitals...)
tmp.sub <- subset(collated_data, is.na(collated_data$cols))

# Add to the database the number of times each landmark was pointed out during the workshops (attribute "count")
collated_data  <- plyr::ddply(collated_data, .(Place), transform, count = length(Place))
collated_data  <-subset(collated_data, count > 3) # This line should only be run if the author wants to display landmarks above a specific number of selections

# New database (simplification of the field work data)
# Each landmark only appears once in the database (centroid). However, the number of times it was pointed out (count) is saved. 
# Other attributes stored in this database are: type of landmark, the color associated for its representation, etcetera
spdf1 <- collated_data
coordinates(spdf1) <- ~X+Y
proj4string(spdf1) <- CRS(ukgrid)
b <- bbox(spdf1)

# Grouping by factor(Place)
spdf2.gp <-  split(spdf1, spdf1@data$Place, drop=T)

# Collapse spdf1 groups to top row
s <- spdf1[ !duplicated(spdf1@data$Place), ]

# Calculate group hulls & centroids
x <- length(spdf2.gp)
h <- sapply(spdf2.gp[1:x], gConvexHull, byid=FALSE, id = NULL)
c <- sapply(h, gCentroid, byid=FALSE, id = NULL)

# Create df of hull centroids
lc <- length(c)
all_data_cntrds.df <- data.frame(ncol = 3, nrow=lc)
colnames(all_data_cntrds.df) <- c("X", "Y")
for (i in 1:length(c)) { 
  all_data_cntrds.df[i,] <- c[[i]]@coords
}

# Add attributes to each of the landmarks of the database 
all_data_cntrds.df$name <- s@data$Place
all_data_cntrds.df$count <- s@data$count
all_data_cntrds.df$type <- s@data$Type
all_data_cntrds.df$cols <- s@data$cols

# Add an attribute 'perc' to represent the point size (ratio: number of times a specific landmark was selected/ total number of selections)
sum.pt <- nrow(collated_data)
for(i in length(all_data_cntrds.df)) {
  all_data_cntrds.df$perc <- c(all_data_cntrds.df$count/sum.pt)*100
  all_data_cntrds.df$perc <- round(all_data_cntrds.df$perc, digits = 2)
} 

# Creation of the Spatial Point DataFrame to be plotted with the package ggplot. 
# Only the landmarks that were selected more than 3 times are included in the database
all_data_cntrds.spdf <- data.frame(all_data_cntrds.df)
coordinates(all_data_cntrds.spdf) <- ~X+Y
proj4string(all_data_cntrds.spdf) <- CRS(latlong)
spTransform(all_data_cntrds.spdf, CRS(latlong))
all_data_cntrds.spdf <- subset(all_data_cntrds.spdf, all_data_cntrds.spdf@data$count > 3) # This line should only be run if the author wants to display landmarks above a specific number of selections

# For plotting purposes: 
# 1) The landmarks within each of the areas of study are extracted using the bbox list (which contains the associated boundaries)
# 2) The nearest 4 locations (k) to each landmark are determined
# 3) The place descriptions are stored in a geographical database. This allows to display them as labels over the correspondent nodes or landmarks.

# smp_list & knn_list contain the information required to plot as an image the nodes and links for each area of study 
smp_list <- list()
knn_list <- list()
# node_list & edge_list contain the required rearrangement of the information to be plotted as a html file (networks allowing interactivity)
nodes_list<-list() 
edges_list<-list() 
for(i in 1:length(bx.list)) {
  smp <- crop(all_data_cntrds.spdf, bx.list[[i]])
  smp_list[[i]] <- smp
  knn <- knearneigh(coordinates(smp_list[[i]]), k=4) # Change number k accordingly if necessary
  knn_list[[i]] <- knn
  nodes_list[[i]]<-data.frame(1:length(smp@data$Place),smp@data$Place,rep(0.8,length(smp@data$Place)),smp@data$count,smp@data$cols,smp@data$Type,smp@coords[,1],-1*smp@coords[,2])
  colnames(nodes_list[[i]])<-c("id","label","label.cex","value","color","group","x","y") 
  edges_list[[i]]<-data.frame(rep(1:length(smp@data$name),4),as.vector(knn$nn),rep("C1CDCD",length(smp@data$name))) 
  colnames(edges_list[[i]])<-c("from","to","color")
}

# g_list & lyt_list contain the geographical and text data required to display the labels corresponding to the nodes or landmarks
g_list <- list()
lyt_list <- list()
for(i in 1:length(smp_list)) {
smp <- data.frame(smp_list[[i]])
# Geo-coords needed to plotting labels
df.tmp <- data.frame("from" = as.character(smp$name), 
                     "to" = as.character(smp$name))
# Graph data base (geo-coords)
meta <- data.frame("name"=c(as.character(smp$name)),
                   "type"=c(as.character(smp$type)),
                   "lon"=c(smp$X), 
                   "lat"=c(smp$Y),
                   "count"=c(smp$count),
                   "cols"=c(as.character(smp$cols)))
lyt_list[[i]] <- as.matrix(meta[,3:4]) #layout
# Add to graph list
g_list[[i]] <- graph.data.frame(df.tmp, directed = F, vertices = meta)
E(g_list[[i]])$color="transparent"
V(g_list[[i]])$size = V(g_list[[i]])$perc/500 #adjusted for size
}

# Plot for each of the schools (change i depending on the graphs that should be generated. Specify areas of study to plot)
par(mfrow=c(4,3))
par(mar=c(0,2,2,2))
par(oma=c(1,3,0,0))
par(bg="white")
for(i in 1:10) {
  plot(knn2nb(knn_list[[i]]), coordinates(smp_list[[i]]), col="grey70",pch='.')
  plot.igraph(g_list[[i]], layout=lyt_list[[i]], vertex.color="white", vertex.label.color= "transparent",rescale=F,add=T) #white vertices for colour contrast
  plot.igraph(g_list[[i]], layout=lyt_list[[i]], vertex.color=V(g_list[[i]])$cols, vertex.label.family="Arial", vertex.label.color= "grey30",vertex.label.cex = 0.7, rescale=F,add=T)
  title(nm.list[[i]])
}

# Global plot 
plot.new()
i<-11
plot(knn2nb(knn_list[[i]]), coordinates(smp_list[[i]]), col="grey70",pch='.')
plot.igraph(g_list[[i]], layout=lyt_list[[i]], vertex.color="white", vertex.label.color= "transparent",rescale=F,add=T) 
plot.igraph(g_list[[i]], layout=lyt_list[[i]], vertex.color=V(g_list[[i]])$cols, vertex.label.family="Arial", vertex.label.color= "grey30",vertex.label.cex = 0.7, rescale=F,add=T)
title(nm.list[[i]])
# Use the next script to add legends explaining the colour code associated to the landmarks, and point size (number of selections in each landmark)
par(mai=c(0,0,0,0)) 
plot.new() 
Temporal_legend_type<-all_data_cntrds.df[ !duplicated(all_data_cntrds.df$type), ]
legend(0,1, legend=Temporal_legend_type$type, pch=16, col=Temporal_legend_type$cols,bty="n",ncol=4,cex=0.85,title="Type of Local Features")
quantile<-quantile(Temporal_legend$count)
quantiles<-c(paste("  ",quantile[1],"-",quantile[2]),paste("  ",quantile[2],"-",quantile[3]),paste("  ",quantile[3],"-",quantile[4]),paste("  ",quantile[4],"-",quantile[5]))
legend(0.87,1, legend=quantiles,col = "black" ,pt.cex=c(0.5,1,1.5,2,2.5),pch=1,cex=0.85,bty="n",ncol=1,y.intersp=1.2,title="Count of Icons")

# Html files(change i depending on the graphs that should be generated.Specify areas of study to plot)
for(i in 1:11) {
g <- graph.data.frame(edges_list[[i]], directed=F, vertices=nodes_list[[i]])
g<-visIgraph(g,idToLabel=FALSE,smooth=TRUE,physics=TRUE) %>% 
visOptions(width="1200px", height="1200px",selectedBy="group") # Change size if required
visSave(g, file=paste("School_",i,".html",sep=""), selfcontained = TRUE, background = "white") 
}