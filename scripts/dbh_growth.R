source("scripts/functions.R")


dbh<- read.csv("raw_data/dbh_growth_basic.csv")
dbh$site <- as.factor(dbh$site)
dbh$species <- as.factor(dbh$species)
dbh$Date <- as.Date(dbh$date, "%m/%d/%Y")
dbh$uniqueID <- paste(dbh$species, dbh$site, sep = "-")


maple <- dbh[dbh$species == "m",]
dogwood <- dbh[dbh$species == "d",]
hawthorn <- dbh[dbh$species == "h",] 

mean(dbh$growth_mm)

#stats-------

growthmaple <- t.test(maple[maple$site=="c", "growth_mm"],maple[maple$site=="p", "growth_mm"])
growthdog <- t.test(dogwood[dogwood$site=="c", "growth_mm"],dogwood[dogwood$site=="p", "growth_mm"])
growthhawthorn <- t.test(hawthorn[hawthorn$site=="c", "growth_mm"],hawthorn[hawthorn$site=="p", "growth_mm"])
