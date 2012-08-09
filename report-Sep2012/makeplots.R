# make the plots for the report

load("../uri-lt-data.RData")

library(ggplot2)

### preamble

# general stuff that gets used by everthing goes here

# plotting options
p.opts <- opts(panel.grid.major=theme_blank(), 
               panel.grid.minor=theme_blank(), 
               panel.background=theme_rect(fill=rgb(43,140,190,max=255),
                                           colour=NA),
               strip.background=theme_blank(),
               legend.key=theme_blank(),
               aspect.ratio=1
              ) 
coord <- coord_cartesian(xlim = c(-40,40),ylim = c(-40,40))

# build a coastline object
coast.geom <- geom_polygon(aes(x=x,y=y),colour=NA,fill=rgb(44, 162, 95,max=255),data=coast) 

### all species all seasons
p.points <- ggplot(obs) + geom_point(aes(x=x,y=y,size=size),
                                     colour=I(rgb(206, 18, 86,max=255)))
p <- p.points 
p <- p + coast.geom
p <- p + p.opts + xlab("") + ylab("") + coord

print(p)

ggsave(file="all-seasons-all-groups.pdf",width=15,height=15)


### all seasons by species group
p.points <- ggplot(obs) + geom_point(aes(x=x,y=y,size=size),colour=I("blue"))
p <- p.points + facet_wrap(~Group,nrow=5)
p <- p + coast.geom
p <- p + p.opts + xlab("") + ylab("") + coord

print(p)

ggsave(file="all-seasons-by-group.pdf",width=15,height=20)

# clean up
rm(p)
gc()


### all species by season
p <- p.points + facet_wrap(~Season,nrow=2)
p <- p + coast.geom
p <- p + p.opts + xlab("") + ylab("") + coord

#print(p)

ggsave(file="all-groups-by-season.pdf",width=15,height=20)


### groups by season
p <- p.points + facet_grid(Group~Season)
p <- p + coast.geom
p <- p + p.opts + xlab("") + ylab("") + coord

# this takes AAAAAAAAAGES
#print(p)

ggsave(file="groups-by-season.pdf",width=15,height=40,plot=p)


rm(p)
gc()

# rowSums(table(obs[,c("Group","Species")])>0)
#     Alcid      Brant  Cormorant      Egret      Eider     Fulmar     Gannet 
#         5          1          3          1          1          1          1 
#      Gull     Jaeger  Kittiwake       Loon       LTDU  Merganser  Passerine 
#         6          1          1          3          1          1          1 
#    Petrel  Phalarope     Scoter Shearwater  Shorebird       Tern 
#         2          2          4          6          1          2 




#### Non-geographical plots
# plotting options
p.opts <- opts(panel.grid.major=theme_blank(), 
               panel.grid.minor=theme_blank(), 
               panel.background=theme_rect(),
               strip.background=theme_blank(),
               legend.key=theme_blank()
              )

### histogram of distances per group
levels(obs$Bin)[4] <- "Not\nrecorded" # so that it fits
p <- ggplot(obs)
p <- p + geom_histogram(aes(x=Bin))
p <- p + facet_wrap(~Group, nrow=5, scales="free_y")
p <- p + p.opts + xlab("bin") + ylab("Number of groups")

print(p)
ggsave(file="groups-dist-hist.pdf",width=15,height=20)


### boxplots of size per group -- all seasons
p <- ggplot(obs)
p <- p + geom_boxplot(aes(x=Group,y=size))
p <- p + p.opts + xlab("Species group") + ylab("Observed group size")

print(p)
ggsave(file="groups-size-boxplot.pdf",width=15,height=8)


### boxplots of size per group -- per season
p <- ggplot(obs)
p <- p + geom_boxplot(aes(x=Group,y=size))
p <- p + facet_wrap(~Season,ncol=1,scales="free_y")
p <- p + p.opts + xlab("Species group") + ylab("Observed group size")

print(p)
ggsave(file="groups-size-per-season-boxplot.pdf",width=15,height=20)


### histogram of size per group -- per season
#p <- ggplot(obs)
#p <- p + geom_histogram(aes(x=size))
#p <- p + facet_grid(Group~Season,scales="free_x")
#p <- p + p.opts + xlab("Frequency") + xlab("Observed group size")
#
#print(p)
#ggsave(file="groups-size-per-season-hist.pdf",width=15,height=20)




