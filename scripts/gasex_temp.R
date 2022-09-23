## leaf temp
source("scripts/functions.R")
source("scripts/plot_objects.R")

#read and format leaf temps
leaft <- read.csv("raw_data/leaf_temperature.csv")
leaft$site <- as.factor(leaft$site)
leaft$species <- as.factor(leaft$species)
leaft$Date <- as.Date(leaft$date, "%m/%d/%Y")
leaft$uniqueID <- paste(leaft$species, leaft$site, sep = "-")

leaft$top_c <- f2c(leaft$top_F)
leaft$bottom_c <- f2c(leaft$bottom_F)

leaft_plantman <- doBy::summaryBy(top_c + bottom_c ~ site + species + Date + replicate,
                  FUN=c(mean2), data=leaft, keep.names = TRUE)


##read and format gase exchang
gasex <- read.csv("raw_data/gasexchange_master.csv")
gasex$site <- as.factor(gasex$site)
gasex$treatment <- as.factor(gasex$treatment)
gasex$date2 <- as.POSIXct(strptime(gasex$date, "%Y%m%d %H:%M:%S"))
gasex$Date <- as.Date(gasex$date2, "%d-%m-%Y")
gasex$ITE <- with(gasex, A/(E*1000))
gasex$uniqueID <- paste(gasex$treatment, gasex$site, sep = "-")

gasex_simple <- gasex[]