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
obs$Year <- split.date[,3]
# SeasonYear as in the effort data, below
month <- split.date[,2]
obs$SeasonYear <- paste(obs$Season,obs$Year,sep="") 
obs$SeasonYear[month==12 & obs$Year==2010] <- "Winter1011"
obs$SeasonYear[(month%in%c(1,2)) & obs$Year==2011] <- "Winter1011"
obs$SeasonYear[month==12 & obs$Year==2011] <- "Winter1112"
obs$SeasonYear[(month%in%c(1,2)) & obs$Year==2012] <- "Winter1112"

obs$Year <- as.factor(split.date[,3])
obs$SeasonYear <- as.factor(obs$SeasonYear)

# PENDING: 
# difference between Transect and Transect_1?
obs$Transect <- NULL 
obs$Transect.Label <- obs$Transect_1
obs$Transect_1 <- NULL

# Re-label "Segment" as "Sample.Label"
obs$Sample.Label <- obs$Segment
obs$Segment <- NULL

# Re-label "Length" as "Effort" (and convert to km)
#obs$Effort <- obs$Length/1000
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

# add in the covariate data for the segments
#   Sample.Label - segment id
#   depth        - depth in feet
#   depthm       - depth in metres
#   slope        - slope
#   roughness    - roughness
#   phimedian    - sediment size
#   chl_fall     - mean chlorophyll A, Fall 2011/2012
#   chl_winter   - ...
#   chl_spring   - ...
#   chl_summer   - ...
# chl data is from NOAA - http://coastwatch.pfeg.noaa.gov/erddap/index.html
covars <- readShapeSpatial("geo/covariates/aerial250_segmdpts_091112")

# save the chlorophyll data to put into the effort table
chl_dat <- covars@data[,c("Fall2010", "Winter1011", "Spring2011", "Summer2011",
                         "Fall2011", "Winter1112", "Spring2012","SEGMENT")]

# save the covariate data
covars <- covars@data[,c("SEGMENT","RASTERVALU","bathy__m_","SLOPE_DEG_",
                         "roughness","phimedian","Fall_Mean","Winter_mea",
                         "Spring_mea","Summer_mea","NEAR_DIST",
                         "Sep_2010","Sep_2011","Oct_2010","Oct_2011","Nov_2010",
                         "Nov_2011","Dec_2010","Dec_2011","Jan_2011","Jan_2012",
                         "Feb_2011","Feb_2012","March_2011","March_2012",
                         "April_2011","April_2012","May_2011","May_2012",
                         "June_2011","June_2012","July_2011","Aug_2011")]
names(covars) <- c("SEGMENT", "depth", "depthm","slope","roughness",
                   "phimedian","chl_fall","chl_winter","chl_spring",
                   "chl_summer","distancelandkm",
                         "Sep_2010","Sep_2011","Oct_2010","Oct_2011","Nov_2010",
                         "Nov_2011","Dec_2010","Dec_2011","Jan_2011","Jan_2012",
                         "Feb_2011","Feb_2012","March_2011","March_2012",
                         "April_2011","April_2012","May_2011","May_2012",
                         "June_2011","June_2012","July_2011","Aug_2011")

# convert distance to land to km
covars$distancelandkm <- (covars$distancelandkm*0.3048)/1000
# values of slope which are 999 are "not available"
covars$slope[covars$slope==999] <- NA

# merge that in
seg <- merge(seg, covars, by.x="Sample.Label",by.y="SEGMENT")

rm(trans.dat,segids,effort,transids,covars)


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


### "Effort" data
# load the effort data
effort <- read.csv("csv/lt-effort.csv",as.is=TRUE)
# change some names around
effort$Transect.Label <- effort$Transect
effort$Transect <- NULL

effort$Sample.Label <- effort$Segment
effort$Segment <- NULL

# build the seasons
# split up the date and make a matrix from it
split.date <- t(matrix(as.numeric(unlist(strsplit(effort$Date,"/"))),nrow=3))
# allocate months to seasons
month <- split.date[,2]
month[month %in% c(6,7,8)] <- "Summer"
month[month %in% c(9,10,11)] <- "Fall"
month[month %in% c(12,1,2)] <- "Winter"
month[month %in% c(3,4,5)] <- "Spring"
effort$Season <- as.factor(month)
# pull out the year
effort$Year <- as.factor(split.date[,3])
# code season and year together
month <- split.date[,2]
effort$SeasonYear <- paste(effort$Season,effort$Year,sep="") 
effort$SeasonYear[month==12 & effort$Year==2010] <- "Winter1011"
effort$SeasonYear[(month%in%c(1,2)) & effort$Year==2011] <- "Winter1011"
effort$SeasonYear[month==12 & effort$Year==2011] <- "Winter1112"
effort$SeasonYear[(month%in%c(1,2)) & effort$Year==2012] <- "Winter1112"
effort$SeasonYear <- as.factor(effort$SeasonYear)

# put in the chlorophyll data
library(reshape2)
chl_dat <- melt(chl_dat,id.vars="SEGMENT")
names(chl_dat) <- c("segid","sy","chl_season")
chl_dat$sy <- as.factor(chl_dat$sy)
effort <- merge(effort, chl_dat,
                by.x=c("SeasonYear","Sample.Label"),
                by.y=c("sy","segid"))

## merge this into the segment data
#effort<-merge(seg,effort,by.x="Sample.Label",by.y="Sample.Label")
#
## only need one Transect.Label
#effort$Transect.Label <- effort$Transect.Label.x
#effort$Transect.Label.x <- NULL
#effort$Transect.Label.y <- NULL

# factor whether we were on survey
effort$Onsurvey_Left <- as.factor(effort$Onsurvey_Left)
effort$Onsurvey_Right <- as.factor(effort$Onsurvey_Right)


# Observer conditions have a "fair" and a "Fair"
effort$Observerconditions_Left[effort$Observerconditions_Left=="fair"] <- "Fair"
effort$Observerconditions_Right[effort$Observerconditions_Right=="fair"] <- "Fair"
effort$Observerconditions_Left <- as.factor(effort$Observerconditions_Left)
effort$Observerconditions_Right <- as.factor(effort$Observerconditions_Right)

# check that worked
#mm<-merge(seg,effort,all=TRUE)
#mm$width <- rep(2,nrow(mm))
#mm$height <- rep(2,nrow(mm))
#p <- ggplot(mm,aes(x=x,y=y))
#p <- p + geom_tile(aes(fill=chl_season,width=width,height=height)) 
#p <- p + scale_fill_continuous(trans="sqrt",limits=c(0.5,61))
#p <- p + facet_wrap(~SeasonYear)
#p



# PENDING -- read in the other effort data and allocate properly!
# need to find a proper "average" per segment or decide via occasion


# Prediction grid
predgr <- readShapeSpatial("geo/covariates/aerial250_predgrid_envcov_091112")
# > str(predgr)
# Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots
#   ..@ data       :'data.frame': 920 obs. of  60 variables:
#   .. ..$ OBJECTID_1: int [1:920] 1 2 3 4 5 6 7 8 9 10 ...
#   .. ..$ Join_Count: int [1:920] 1 1 1 1 1 1 1 1 1 1 ...
#   .. ..$ TARGET_FID: int [1:920] 0 1 2 3 4 5 6 7 8 9 ...
#   .. ..$ OBJECTID  : int [1:920] 1 2 3 4 5 6 7 8 9 10 ...
#   .. ..$ Join_Cou_1: int [1:920] 1 1 1 1 1 1 1 1 1 1 ...
#   .. ..$ TARGET_F_1: int [1:920] 0 1 2 3 4 5 6 7 8 9 ...
#   .. ..$ ID        : num [1:920] 1 2 3 4 5 6 7 8 9 10 ...
#   .. ..$ Distancela: num [1:920] 71513 76542 81773 81745 81651 ...
#   .. ..$ LONGITUDE : num [1:920] -71.7 -71.7 -71.6 -71.6 -71.6 ...
#   .. ..$ LATITUDE  : num [1:920] 40.9 40.9 40.9 40.9 40.9 ...
#   .. ..$ CELLAREA  : num [1:920] 4e+06 4e+06 4e+06 4e+06 4e+06 4e+06 4e+06 4e+06 4e+06 4e+06 ...
#   .. ..$ bathy__m  : num [1:920] -50.4 -50.4 -51.3 -52.8 -53.2 ...
#   .. ..$ SLOPE_DEG_: num [1:920] 0.1277 0.0333 0.0586 0.0208 0.0671 ...
#   .. ..$ roughness : num [1:920] 0.0824 0.0503 0.0561 0.043 0.0366 ...
#   .. ..$ phimedian : num [1:920] 2.22 2.34 2.44 2.5 2.54 ...
#   .. ..$ SSTautSTD : num [1:920] 2.99 3.04 3.02 3 2.96 ...
#   .. ..$ SSTsprSTD : num [1:920] 4.45 4.47 4.51 4.52 4.58 ...
#   .. ..$ SSTsumSTD : num [1:920] 4.45 4.47 4.51 4.52 4.58 ...
#   .. ..$ SSTwinSTD : num [1:920] 1.85 1.79 1.81 1.87 1.91 ...
#   .. ..$ latitude_1: num [1:920] 40.9 40.9 40.9 40.9 40.9 ...
#   .. ..$ longitude_: num [1:920] -71.7 -71.7 -71.6 -71.6 -71.6 ...
#   .. ..$ Fall_Mean : num [1:920] 2.45 2.46 2.15 2.14 1.81 ...
#   .. ..$ Winter_mea: num [1:920] 2.25 2.34 2.93 2.82 2.61 ...
#   .. ..$ Spring_mea: num [1:920] 2.46 2.35 2.31 2.58 2.54 ...
#   .. ..$ Summer_mea: num [1:920] 1.264 1.282 1.075 0.887 0.872 ...
#   .. ..$ latitude_2: num [1:920] 40.9 40.9 40.9 40.9 40.9 ...
#   .. ..$ longitude1: num [1:920] -71.7 -71.7 -71.6 -71.6 -71.6 ...
#   .. ..$ Sep_2010  : num [1:920] 2.12 2.25 2.38 2.6 1.7 ...
#   .. ..$ Sep_2011  : num [1:920] 1.69 1.58 1.5 1.39 1.31 ...
#   .. ..$ Oct_2010  : num [1:920] 1.52 1.48 1.58 1.74 1.59 ...
#   .. ..$ Oct_2011  : num [1:920] 4.67 4.81 2.39 1.86 2.15 ...
#   .. ..$ Nov_2010  : num [1:920] 2.56 2.37 2.67 2.63 2.45 ...
#   .. ..$ Nov_2011  : num [1:920] 2.12 2.25 2.38 2.6 1.7 ...
#   .. ..$ Fall_Mean_: num [1:920] 2.45 2.46 2.15 2.14 1.81 ...
#   .. ..$ Dec_2010  : num [1:920] 1.77 1.76 1.67 1.68 1.89 ...
#   .. ..$ Dec_2011  : num [1:920] 1.17 1.23 1.4 1.38 1.44 ...
#   .. ..$ Jan_2011  : num [1:920] 2.17 2.37 2.77 2.26 1.83 ...
#   .. ..$ Jan_2012  : num [1:920] 4.04 4.11 3.41 3.29 4.8 ...
#   .. ..$ Feb_2011  : num [1:920] 1.94 2.01 1.87 1.84 1.9 ...
#   .. ..$ Feb_2012  : num [1:920] 2.42 2.56 6.48 6.5 3.79 ...
#   .. ..$ Winter_m_1: num [1:920] 2.25 2.34 2.93 2.82 2.61 ...
#   .. ..$ March_2011: num [1:920] 2.96 2.77 3.24 3.74 3.72 ...
#   .. ..$ March_2012: num [1:920] 1.77 1.76 1.67 1.68 1.89 ...
#   .. ..$ April_2011: num [1:920] 3.27 2.52 2.26 3.2 3.38 ...
#   .. ..$ April_2012: num [1:920] 2.73 2.96 2.72 2.79 2.48 ...
#   .. ..$ May_2011  : num [1:920] 1.28 1.15 1.22 1.26 1.3 ...
#   .. ..$ May_2012  : num [1:920] 2.73 2.96 2.72 2.79 2.48 ...
#   .. ..$ Spring_m_1: num [1:920] 2.46 2.35 2.31 2.58 2.54 ...
#   .. ..$ June_2011 : num [1:920] 1.985 2.081 1.638 0.958 0.99 ...
#   .. ..$ June_2012 : num [1:920] 1.455 1.409 1.066 1.008 0.968 ...
#   .. ..$ July_2011 : num [1:920] 0.689 0.705 0.679 0.673 0.622 ...
#   .. ..$ Aug_2011  : num [1:920] 0.926 0.931 0.918 0.91 0.909 ...
#   .. ..$ Summer_m_1: num [1:920] 1.264 1.282 1.075 0.887 0.872 ...
#   .. ..$ Fall2010  : num [1:920] 2.07 2.03 2.21 2.32 1.91 ...
#   .. ..$ Winter1011: num [1:920] 1.96 2.05 2.11 1.93 1.87 ...
#   .. ..$ Spring2011: num [1:920] 2.5 2.15 2.24 2.74 2.8 ...
#   .. ..$ Summer2011: num [1:920] 1.2 1.239 1.078 0.847 0.84 ...
#   .. ..$ Fall2011  : num [1:920] 2.83 2.88 2.09 1.95 1.72 ...
#   .. ..$ Winter1112: num [1:920] 2.54 2.63 3.76 3.72 3.34 ...
#   .. ..$ Spring2012: num [1:920] 2.41 2.56 2.37 2.42 2.28 ...
#   .. ..- attr(*, "data_types")= chr [1:60] "N" "N" "N" "N" ...
#   ..@ coords.nrs : num(0) 
#   ..@ coords     : num [1:920, 1:2] 278339 284901 291463 298025 304587 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : chr [1:920] "0" "1" "2" "3" ...
#   .. .. ..$ : chr [1:2] "coords.x1" "coords.x2"
#   ..@ bbox       : num [1:2, 1:2] 219281 -58722 501447 138138
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : chr [1:2] "coords.x1" "coords.x2"
#   .. .. ..$ : chr [1:2] "min" "max"
#   ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
#   .. .. ..@ projargs: chr NA

# pull out the data
predgr <- predgr@data

# convert lat/long
ne <- latlong2km(lon=predgr$LONGITUDE,lat=predgr$LATITUDE,lon0=lon.0,lat0=lat.0)
predgr$x <- ne$km.e
predgr$y <- ne$km.n
predgr$LONGITUDE <- NULL
predgr$LATITUDE <- NULL
rm(ne)

# kill those things that don't matter
predgr$OBJECTID <- NULL
predgr$Join_Count <- NULL
predgr$TARGET_FID <- NULL
predgr$ID <- NULL
predgr$latitude_1 <- NULL
predgr$SSTautSTD <- NULL
predgr$SSTsprSTD <- NULL
predgr$SSTsumSTD <- NULL
predgr$SSTwinSTD <- NULL
predgr$longitude_ <- NULL

#   depth        - depth in feet
#   depthm       - depth in metres
#   slope        - slope
#   roughness    - roughness
#   phimedian    - sediment size
#   chl_fall     - mean chlorophyll A, Fall 2011/2012
#   chl_winter   - ...
#   chl_spring   - ...
#   chl_summer   - ...

# convert distance to land to km 
#  (according to Louise's code)
predgr$distancelandkm <- predgr$Distancela/1000
predgr$Distancela <- NULL
# cell area is in metres
predgr$cellaream <- predgr$CELLAREA
predgr$CELLAREA <- NULL
# chlorophyll - seasonal mean across years
predgr$chl_fall <- predgr$Fall_Mean
predgr$chl_winter <- predgr$Winter_mea
predgr$chl_spring <- predgr$Spring_mea
predgr$chl_summer <- predgr$Summer_mea
predgr$Fall_Mean <- NULL
predgr$Winter_mea <- NULL
predgr$Spring_mea <- NULL
predgr$Summer_mea <- NULL
# chlorophyll - season means
predgr$Fall_Mean_  <- NULL
predgr$Winter_m_1 <- NULL
predgr$Spring_m_1  <- NULL
predgr$Summer_m_1  <- NULL


# save the monthly's for the FCPI calculation
mpred <- predgr

# now remove for the other data sets
# chlorophyll - monthly mean
predgr$Sep_2010  <- NULL
predgr$Sep_2011  <- NULL
predgr$Oct_2010  <- NULL
predgr$Oct_2011  <- NULL
predgr$Nov_2010  <- NULL
predgr$Nov_2011  <- NULL
predgr$Dec_2010  <- NULL
predgr$Dec_2011  <- NULL
predgr$Jan_2011  <- NULL
predgr$Jan_2012  <- NULL
predgr$Feb_2011  <- NULL
predgr$Feb_2012  <- NULL
predgr$March_2011  <- NULL
predgr$March_2012  <- NULL
predgr$April_2011  <- NULL
predgr$April_2012  <- NULL
predgr$May_2011  <- NULL
predgr$May_2012  <- NULL
predgr$June_2011  <- NULL
predgr$June_2012  <- NULL
predgr$July_2011  <- NULL
predgr$Aug_2011  <- NULL
# pull out the seasonal Chlorophyll A per year, store in a separate data set
#  so we don't mess up the structure of the other data
tpred <- predgr[,c("Fall2010","Winter1011","Spring2011","Summer2011",
                   "Fall2011","Winter1112","Spring2012","OBJECTID_1")]
predgr$Fall2010 <- NULL
predgr$Winter1011 <- NULL
predgr$Spring2011 <- NULL
predgr$Summer2011 <- NULL
predgr$Fall2011 <- NULL
predgr$Winter1112 <- NULL
predgr$Spring2012 <- NULL

# depth
predgr$depthm <- predgr$bathy__m
predgr$bathy__m <- NULL
predgr$depth <- predgr$depthm*3.28084
# slope
predgr$slope <- predgr$SLOPE_DEG_
predgr$SLOPE_DEG_ <- NULL
predgr$slope[predgr$slope==999] <- NA

pred <- predgr
rm(predgr)


# now to deal with the tpred seasonal chlorophyll data
tpred <- melt(tpred,id.vars="OBJECTID_1")
names(tpred) <- c("OBJECTID","SeasonYear","chl_season")

# check that works
#mm <- merge(pred,tpred,
#            by.x="OBJECTID_1",
#            by.y="OBJECTID")
#mm$width <- rep(2, nrow(mm))
#mm$height <- rep(2, nrow(mm))
#p <- ggplot(mm,aes(x=x,y=y))
#p <- p + geom_tile(aes(fill=chl_season,width=width,height=height)) 
#p <- p + scale_fill_continuous(trans="sqrt",limits=c(0.5,61))
#p <- p + facet_wrap(~SeasonYear)
#p



##### match up the extra pred data with to the segments
#
## find the nearest prediction cell to each segment
#dists <- as.matrix(dist(rbind(pred[,c("x","y")],seg[,c("x","y")]),
#                        diag=TRUE,upper=TRUE))
#dists[lower.tri(dists)] <- NA
#diag(dists) <- NA
## only take the pred -> seg part of the matrix
#dists <- dists[1:nrow(pred),(nrow(pred)+1):ncol(dists)]
## what is the nearest cell?
#nearest <- apply(dists,2,which.min)
#
#pick.names <- c("distancela","depth","sstaut","stdaut","sstwin","stdwin",
#                "sstsum","stdsum","sstspr","stdspr","slope_deg_","roughness",
#                "phimedian","vtr_otf","vtr_otf_wi","vtr_otf_sp","vtr_otf_su",
#                "vtr_otf_au")
#seg <- cbind(seg, pred[nearest,pick.names])


# remove segments that don't have covariate data
drop.segs <- c(624,819,1025,1226,1427,625,820,1026,1227,1629,1729,1930,1730)
seg <- seg[!(seg$Sample.Label %in% drop.segs),]
obs <- obs[!(obs$Sample.Label %in% drop.segs),]
effort <- effort[!(effort$Sample.Label %in% drop.segs),]



### Save!
save(coast, obs, seg, effort, pred, tpred, mpred, file="uri-lt-data.RData")



