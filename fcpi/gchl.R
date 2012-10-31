# geometric mean of chl from the 10 year MODIS data

# lat and long references
lat.0 <- 41.17
lon.0 <- -71.34

# load MODIS data
chl<-read.csv("chl_alldata_monthly_102612.csv")
# drop first line
chl <- chl[-1,]

# make the data.frame wide rather than long, one column per time period
library(reshape2)
chl.cast <- dcast(chl,latitude+longitude ~ time)

# convert lat long into km.n, km.e
library(dsm)
noreast <- latlong2km(as.numeric(as.character(chl.cast$longitude)),
                      as.numeric(as.character(chl.cast$latitude)),lon.0,lat.0)
chl.cast$y <- noreast$km.n
chl.cast$x <- noreast$km.e

### nearest points
load("../uri-lt-data.RData")

find_chl <- function(pdat,newdat){
  # calculate the distances
  dd <- rbind(pdat,newdat[,c("x","y")])
  dists <- as.matrix(dist(dd,diag=TRUE,upper=TRUE))

  # the matrix is nrow(MODIS points)+nrow(newdat) square
  # cut that down
  dists <- dists[1:nrow(pdat),]
  dists <- dists[,(nrow(pdat)+1):ncol(dists)]

  # find the nearest points to the segs
  min.ind <- apply(dists,2,which.min)

  return(min.ind)
}

chl_data <- as.numeric(unlist(chl.cast[,-c(1,2,123,124)]))
chl_data[is.nan(chl_data)]<-NA
chl_data <- matrix(chl_data,nrow(chl.cast),ncol(chl.cast)-4)
chl_xy <- chl.cast[,c(123,124)]

geom_m <- function(x){
  prod(x,na.rm=TRUE)^(1/length(x[!is.na(x)]))
}

# for the segments
seg_ind <- find_chl(chl_xy,seg)
gchl_seg <- apply(chl_data[seg_ind,],1,geom_m)


# for the pred grid
pred_ind <- find_chl(chl_xy,pred)
gchl_pred <- apply(chl_data[pred_ind,],1,geom_m)



p.opts.geo <- opts(panel.grid.major=theme_blank(),
                   panel.grid.minor=theme_blank(),
                   panel.background=theme_blank(),
                   strip.background=theme_blank(),
                   legend.key=theme_blank(),
                   aspect.ratio=1
                  )


# plot on lat/long
pdat <- cbind(gchl_pred,pred$x,pred$y)
pdat <- as.data.frame(pdat)
names(pdat) <- c("gchl","x","y")
#p <- ggplot(pdat,aes(x=x,y=y,fill=gchl,width=0.0125,height=0.0125))
p <- ggplot(pdat,aes(x=x,y=y,fill=gchl,width=2,height=2))
#p <- p + coord_map()
p <- p + geom_tile()
p <- p + p.opts.geo
print(p)

# plot on northings/eastings
pdat <- cbind(fcpi,locs.xy)
pdat <- as.data.frame(pdat)
names(pdat) <- c("fcpi","y","x")
#p <- ggplot(pdat,aes(x=x,y=y,fill=fcpi,width=1.03,height=1.38))
#p <- p + geom_tile()
#p <- p + p.opts.geo + opts(title="FCPI")
#p <- p + coord_cartesian(xlim = c(-40,40),ylim = c(-40,40))
#print(p)

save(gchl_seg,gchl_pred,file="gchl.RData")

