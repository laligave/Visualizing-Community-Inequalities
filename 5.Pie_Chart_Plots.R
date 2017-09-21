#Generates geo-located plots of pie charts based on counts of pos/neg icons

#Compile data (github repository):
#RUN Community_Results.R as far as >colnames(collated_data) (line 76)

library(scatterpie)

#make a tmp copy of all data
tmp_coll_data <- collated_data
#substitute icons for catg pos/neg
tmp_coll_data$icon <- sub("br|hz|sh|an|sd|hbd|dm|sp", "neg", tmp_coll_data$icon)
tmp_coll_data$icon <- sub("lv|hp|hgd|hOK|int", "pos", tmp_coll_data$icon)

#list of unique factors from struct_spec
l <- unique(tmp_coll_data$strct_spec) 
#df
pie.df <- data.frame()[1:6,]

#iterate over list of factors
#add counts pos/neg
for(i in 1:length(l)) {
  s <- subset(tmp_coll_data, tmp_coll_data$strct_spec %in% l[[i]]) #subsets of each unique struct_spec
  #s  <- plyr::ddply(s, .(icon), transform, cnt_all = length(icon)) #use to plot radius in proportion if required
  s  <- plyr::ddply(s, .(icon), transform, cnt_pos = length(icon[icon=="pos"]))
  s  <- plyr::ddply(s, .(icon), transform, cnt_neg = length(icon[icon=="neg"]))
  #bind values to df (set radius as proportion, if needed)
  r <- data.frame(mean(s$X), mean(s$Y), as.character(s$strct_spec), as.character(s$strct_type), max(s$cnt_pos), max(s$cnt_neg), as.numeric(max(s$cnt_pos) + max(s$cnt_neg))/10000)
  pie.df <- rbind(pie.df, r)
}
#names
colnames(pie.df) <- c("Lat", "Long", "name", "urb_typ", "Positive", "Negative", "radius")

#spatial
pie.spdf <- pie.df
coordinates(pie.spdf) <- ~Lat+Long
proj4string(pie.spdf) <- CRS(ukgrid)


#hull and bbox map
plot_hull <- gConvexHull(pie.spdf, byid=FALSE, id = NULL)
bx  <- bbox(plot_hull)

#subset (example)
d <- subset(pie.df, pie.df$urb_typ %in% "open space")
#rmv zero counts
d <- d[!(d$Positive == 0 & d$Negative == 0),]
#collapse duplicates
d <- d[ !duplicated(d$name), ]
#spatial
ds <- d
coordinates(ds) <- ~Lat+Long
proj4string(ds) <- CRS(ukgrid)
h <- gConvexHull(ds, byid=FALSE, id = NULL)
bf <- gBuffer(h, width=0.01)
bx <- bbox(bf)

#plot 
ggplot() + geom_scatterpie(aes(x=Lat, y=Long, r = 0.005), data=d, cols=c("Positive", "Negative"), color="grey30", lwd=0.1, alpha=.8) +
  geom_text(data=d,size=3, aes(x=Lat, y=Long, label=name)) +
  scale_fill_manual("Icon \ntype",values=c("lightblue","lightgreen"))