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
obs$Bin <- as.factor(obs$Bin)

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
obs$Transect <- obs$Transect_1
obs$Transect_1 <- NULL


### Effort data
# load the effort data
#effort <- read.csv("csv/lt-effort.csv",as.is=TRUE)

# PENDING: pull this in with the GIS data that Kris will give me


### coastline!
# load the coastline data (from Louise Burt)
coast <- read.table("geo/coast.dat",as.is=TRUE,header=TRUE)

# convert to to Northings/Eastings
coast <- as.data.frame(latlong2km(lon=coast[,1],
                                  lat=coast[,2],
                                  lon0=lon.0,lat0=lat.0))
names(coast) <- c("x","y")


### check it's all there
#plot(coast,type="l",xlim=c(-45,45),ylim=c(-40,40))
#points(obs$x,obs$y,cex=0.3,pch=19)

### Save!
#save(coast,obs,effort,file="uri-lt-data.RData")
save(coast,obs,file="uri-lt-data.RData")



