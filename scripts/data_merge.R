element <- read.csv("raw_data/isotope_data.csv")
element$species <- as.factor(element$species)
element$site <- as.factor(element$site)
element$uniqueid <- as.factor(paste(element$species, element$site, sep="-"))
element$Date <- as.Date(element$date, "%m/%d/%Y")


gasex <- read.csv("raw_data/gasexchange_master.csv")
gasex$site <- as.factor(gasex$site)
gasex$species <- as.factor(gasex$treatment)
gasex$date2 <- as.POSIXct(strptime(gasex$date, "%Y%m%d %H:%M:%S"))
gasex$Date <- as.Date(gasex$date2, "%d-%m-%Y")
gasex$uniqueid <- paste(gasex$treatment, gasex$site, sep = "-")


gasnitro <- merge(element, gasex, by = c("site", "species", "replicate", "week"))

thick <- read.csv("raw_data/leafthick.csv")
thick$site <- as.factor(thick$site)
thick$species <- as.factor(thick$species)
thick$Date <- as.Date(thick$date, "%m/%d/%Y")
thick$uniqueid <- paste(thick$species, thick$site, sep = "-")
thick$sla_cm2g <- with(thick, area_cm2/drymass_g)
thick$lma_gm2 <- with(thick, drymass_g/(area_cm2*0.0001))

pnue <- merge(gasnitro, thick, by = c("site", "species", "replicate", "week"))
pnue$amass <- with(pnue, (A*1000) * (sla_cm2g/1000)) ####nmols CO2 g s
pnue$nmass <- with(pnue, nitro_perc*10) #mg g-1 (g g-1 = .01 (1%) and 1000)
pnue$nue <- with(pnue, amass/nmass)

write.csv(pnue, "calculated_data/alldata.csv", row.names=FALSE)