# associate predicitons and segments with their nearest FCPI

library(ggplot2)

load("../uri-lt-data.RData")
load("fcpi.RData")

find_chl <- function(pdat,newdat){
  # calculate the distances
  dd <- rbind(pdat[,2:3],newdat[,c("x","y")])
  dists <- as.matrix(dist(dd,diag=TRUE,upper=TRUE))

  # the matrix is nrow(MODIS points)+nrow(newdat) square
  # cut that down
  dists <- dists[1:nrow(pdat),]
  dists <- dists[,(nrow(pdat)+1):ncol(dists)]

  # find the nearest points to the segs
  min.ind <- apply(dists,2,which.min)

  return(data.frame(x=newdat$x,
                    y=newdat$y,
                    fcpi=pdat$fcpi[min.ind]))
}


# run for segments
chl_segs <- find_chl(pdat,seg)

# check!
#p<-ggplot(chl_segs)
#p<-p+geom_tile(aes(x=x,y=y,fill=fcpi,width=2,height=2))
#print(p)

quartz()

# run for segments
chl_pred <- find_chl(pdat,pred)

# check!
#p<-ggplot(chl_pred)
#p<-p+geom_tile(aes(x=x,y=y,fill=fcpi,width=2,height=2))
#print(p)

save(chl_pred,chl_segs,file="fcpi-predseg.RData")
