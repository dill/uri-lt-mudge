# make some tables for the report


load("../uri-lt-data.RData")


# counts here means "sightings" not # of birds!

# group
table(obs[,c("Group")])

## year, season
table(obs[,c("Season","Year")])



## group, season, year
table(obs[,c("Season","Year","Group")])


