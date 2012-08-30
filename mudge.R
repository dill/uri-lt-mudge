# URI SAMP data -- line transects

# load the data, do some processing and then save as RData

### Preamble

# use the sample centre of survey area as Louise Burt's analysis
# of the ship and strip data
lat.0 <- 41.17
lon.0 <- -71.34

# load the dsm library for latlon2km
library(dsm)

### Observations

# load the observations
obs <- read.csv("csv/lt-obs.csv",as.is=TRUE)
#> str(obs)
#'data.frame': 8577 obs. of  21 variables:
# $ OBJECTID  : int  1 2 3 4 5 6 7 8 9 10 ...
# $ FID_Aerial: int  13 14 15 17 22 23 30 31 32 33 ...
# $ Date      : chr  "28/01/2012" "06/11/2011" "06/11/2011" "07/04/2011" ...
# $ Transect  : int  10 10 10 6 6 6 6 6 6 6 ...
# $ Time      : int  18991230 18991230 18991230 18991230 18991230 18991230 18991230 18991230 18991230 18991230 ...
# $ Species   : chr  "DOVE" "NOFU" "Shearwater spp." "NOGA" ...
# $ Group     : chr  "Alcid" "Fulmar" "Shearwater" "Gannet" ...
# $ Number    : int  1 1 1 1 1 1 1 1 12 1 ...
# $ Location  : chr  "On Water" "Flying" "Flying" "Flying" ...
# $ Bin       : chr  "A" "A" "A" "A" ...
# $ Lat       : num  40.8 40.8 40.8 40.8 40.8 ...
# $ Long      : num  -71.5 -71.5 -71.5 -71.6 -71.6 ...
# $ Comments  : chr  "" "" "" "" ...
# $ Observer  : chr  "Ellen" "KW" "KW" "KW" ...
# $ FID_aeri_1: int  135 135 135 87 87 87 87 87 87 87 ...
# $ Id        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ z         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Length    : int  7448 7448 7448 7448 7448 7448 7448 7448 7448 7448 ...
# $ Transect_1: int  10 10 10 6 6 6 6 6 6 6 ...
# $ Segment   : int  1026 1026 1026 625 625 625 625 625 625 625 ...
# $ BUFF_DIST : int  5000 5000 5000 5000 5000 5000 5000 5000 5000 5000 ...

# first off, drop those columns that we're not interested in
obs$z <- NULL # all 0
obs$Id <- NULL # all 0
obs$BUFF_DIST <- NULL # all 5000
obs$Comments <- NULL # probably not going to be useful for the analysis
obs$FID_Aerial <- NULL # from GIS, not important (KW)
obs$FID_aeri_1 <- NULL # from GIS, not important (KW)

# rename "Number" to "size"
obs$size <- obs$Number
obs$Number <- NULL

# rename "OBJECTID" to "object"
obs$object <- obs$OBJECTID
obs$OBJECTID <- NULL

# convert lat long to km from the centre
kmc <- latlong2km(lon=obs$Long,lat=obs$Lat,lon0=lon.0,lat0=lat.0)
obs$x <- kmc$km.e
obs$y <- kmc$km.n
obs$Lat <- NULL
obs$Long <- NULL

# make the columns that should be factors into factors
obs$Group <- as.factor(obs$Group)
obs$Species <- as.factor(obs$Species)
obs$Observer <- as.factor(obs$Observer)
obs$Location <- as.factor(obs$Location)

# bins
obs$Bin[obs$Bin=="a"] <- "A"
obs$Bin[obs$Bin=="c"] <- "C"
#obs <- obs[obs$Bin!="Not recorded",] # PENDING: drop records with no distance?
obs$Bin <- as.factor(obs$Bin)

# add distbegin distend fields
# bin A 44–163m 
# bin B 164–432m 
# bin C 433–1000m

obs$distbegin <- rep(NA, nrow(obs))
obs$distend <- rep(NA, nrow(obs))

obs$distbegin[obs$Bin=="A"] <- 44
obs$distend[obs$Bin=="A"] <- 163
obs$distbegin[obs$Bin=="B"] <- 164
obs$distend[obs$Bin=="B"] <- 432
obs$distbegin[obs$Bin=="C"] <- 433
obs$distend[obs$Bin=="C"] <- 1000

# distance field is the midpoint
obs$distance <- obs$distend - obs$distbegin


# build the seasons
# split up the date and make a matrix from it
split.date <- t(matrix(as.numeric(unlist(strsplit(obs$Date,"/"))),nrow=3))
# allocate months to seasons
month <- split.date[,2]
month[month %in% c(6,7,8)] <- "Summer"
month[month %in% c(9,10,11)] <- "Fall"
month[month %in% c(12,1,2)] <- "Winter"
month[month %in% c(3,4,5)] <- "Spring"
obs$Season <- as.factor(month)
# pull out the year
obs$Year <- as.factor(split.date[,3])


# PENDING: 
# difference between Transect and Transect_1?
obs$Transect <- NULL 
obs$Transect.Label <- obs$Transect_1
obs$Transect_1 <- NULL

# Re-label "Segment" as "Sample.Label"
obs$Sample.Label <- obs$Segment
obs$Segment <- NULL

# Re-label "Length" as "Effort"
obs$Effort <- obs$Length
obs$Length <- NULL


### coastline!
# load the coastline data (from Louise Burt)
coast <- read.table("geo/coast.dat",as.is=TRUE,header=TRUE)

# convert to to Northings/Eastings
coast <- as.data.frame(latlong2km(lon=coast[,1],
                                  lat=coast[,2],
                                  lon0=lon.0,lat0=lat.0))
names(coast) <- c("x","y")

# make the coastline join up so we can plot as polygons...
nas <- which(is.na(coast[,1]))
j <- 1
new.coast<-c()
for(i in nas){
  i<-i-1
  # if the beginning node is the same as the last  
  if(coast[i,1]==coast[j,1] & coast[i,2]==coast[j,2]){
    new.coast <- rbind(new.coast,
                       c(NA,NA),
                       coast[j:i,])
#  }else{
#    cat("aack -- ",j,i,"\n")
  }

  j <- i+2

}
# now join everything up
new.coast <- rbind(coast[1:105,],coast[1,],c(NA,NA),
                   coast[107:388,],coast[107,],c(NA,NA),
                   coast[390:2979,], # RI coastline 
                   coast[rev(4647:6433),], # MA coastline 
                   c(coast[4647,1],coast[7693,2]), # join point
                   coast[rev(6874:7693),], # MA coastline - top
                   c(coast[390,1],coast[6874,2]), # join point
                   coast[390,],c(NA,NA), # back to the start
                   coast[6435:6443,],coast[6435,],c(NA,NA), 
                   coast[6445:6872,],coast[6445,],c(NA,NA),  
                   coast[7695:7719,],coast[7695,],
                   new.coast)

coast <- new.coast
rm(new.coast)

# put in groups rather than using NAs to separate parts of the map...
nas <- which(is.na(coast[,1]))
coast.groups <- rep(1:(length(nas)+1),diff(c(0,nas,nrow(coast)+1))-1)

coast <- cbind(coast[-nas,],
                   group=coast.groups)


### Segment data
library(maptools)

trans <- readShapeSpatial("geo/transects/aerialtransects082112")

# the segment ids are stored separately, the coding (I think!) is
# transect number (1/2 digit) then segment number (1/2 digit)
segids <- trans@data$Segment
effort <- trans@data$Length
transids <- trans@data$Transect

trans.dat <- c()
for(i in 1:length(trans@lines)){
  this.line <- trans@lines[[i]]@Lines[[1]]@coords

  this.km <- latlong2km(lon=this.line[,1],
                        lat=this.line[,2],
                        lat0 = lat.0, lon0=lon.0)

  # format:  (one line per segment)
  #   "top" x coordinate
  #   "top" y coordinate
  #   "bottom" x coordinate
  #   "bottom" y coordinate
  #   centre x
  #   centre y
  #   segment label
  #   effort (length of transect)
  #   transect label
  # --- "top" means "end we visited first"
  trans.dat <- rbind(trans.dat,
                     c(this.km$km.e[1],this.km$km.n[1],
                       this.km$km.e[2],this.km$km.n[2],
                       this.km$km.e[1]+(this.km$km.e[2]-this.km$km.e[1])/2,
                       this.km$km.n[1]+(this.km$km.n[2]-this.km$km.n[1])/2,
                       segids[i],
                       effort[i],
                       transids[i]))

}

seg <- as.data.frame(trans.dat)
names(seg) <- c("tx","ty","bx","by","x","y","Sample.Label",
                       "Effort","Transect.Label")

rm(trans.dat,segids,effort,transids)

#### check it's all there
#plot(coast$x,coast$y,type="n",xlim=c(-45,45),ylim=c(-40,40))
#points(obs$x,obs$y,cex=0.1,pch=19,col="red")
#
#for(i in unique(coast$group)){
#  coasty <- coast[coast$group==i,]
#  lines(coasty$x,coasty$y,lwd=2)
#}
#
#points(transects$x,transects$y,col="blue",pch=19,cex=0.3)


### Effort data
# load the effort data
#effort <- read.csv("csv/lt-effort.csv",as.is=TRUE)

# PENDING -- read in the other effort data and allocate properly!
# need to find a proper "average" per segment or decide via occasion


### Save!
save(coast, obs, seg, file="uri-lt-data.RData")



