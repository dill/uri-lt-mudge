# build the FCPI from the 10 year MODIS data

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

# get only those points inside the samp region
load("../uri-lt-data.RData")
x <- chl.cast$x
y <- chl.cast$y
ind <- inSide(as.list(samp),x,y)
chl.cast <- chl.cast[ind,]

### suryan steps

# separate locations and chl for now
chl.cast <- as.data.frame(chl.cast)
locs.xy <- cbind(as.numeric(as.character(chl.cast[,123])),
                 as.numeric(as.character(chl.cast[,124])))
locs.latlong <- cbind(as.numeric(as.character(chl.cast[,1])),
                      as.numeric(as.character(chl.cast[,2])))
dat <- chl.cast[,-c(1,2,123,124)]
dat[dat=="NaN"]<-NA
dat <- matrix(as.double(as.matrix(dat)),nrow=nrow(dat),ncol=ncol(dat))


# log the chlorophyll
dat <- log(dat)

# standardize
zdat <- (dat-colMeans(dat,na.rm=TRUE))/apply(dat,2,sd,na.rm=TRUE)

# "spatial" mean for th monthly data
#chl_{m} = \beta_0 + \beta_1 \sin(2\pi f_1 t)+ \beta_2 \cos(2\pi f_1 t)
#                  + \beta_3 \sin(2\pi f_2 t)+ \beta_4 \cos(2\pi f_2 t)
#                  + \beta_5 t

f1 <- 1/12
f2 <- 1/6
t <- 1:120

mdat <- data.frame(mchl  = colMeans(dat,na.rm=TRUE),
                   t     = t,
                   sinf1 = sin(2*pi*f1*t),
                   cosf1 = cos(2*pi*f1*t),
                   sinf2 = sin(2*pi*f2*t),
                   cosf2 = cos(2*pi*f2*t))


m.lm <- lm(mchl~sinf1+cosf1+sinf2+cosf2+t,data=mdat)

plot(sort(t),mdat$mchl[order(t)],type="l",ylab="Monthly chl_a",
     main="Monthly Chlorophyll A\n(data=black, model=red)")
lines(sort(t),predict(m.lm)[order(t)],col="red")

m.lm.pred <- matrix(predict(m.lm),1)


# step 3
# proportion of time that a pixel is >1 SD above the monthly average
fcpi <- rowSums(zdat-as.vector(m.lm.pred) >
                apply(zdat,2,sd,na.rm=TRUE),na.rm=TRUE)/120

p.opts.geo <- opts(panel.grid.major=theme_blank(),
                   panel.grid.minor=theme_blank(),
                   panel.background=theme_blank(),
                   strip.background=theme_blank(),
                   legend.key=theme_blank(),
                   aspect.ratio=1
                  )


# plot on lat/long
pdat <- cbind(fcpi,locs.latlong)
pdat <- as.data.frame(pdat)
names(pdat) <- c("fcpi","y","x")
p <- ggplot(pdat,aes(x=x,y=y,fill=fcpi,width=0.0125,height=0.0125))
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

save(pdat,file="fcpi.RData")

