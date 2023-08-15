# plot objects

parkcol <- "forestgreen"
citycol <- "gray25"

trtcols <- c(citycol, parkcol)
trtcols2 <- scales::alpha(trtcols, .6)
trtcols3 <- scales::alpha(trtcols, .3)

pchs = c(15,17,16)

ratio_lab <- "Root to Shoot ratio"
biomasslab <- "Total Biomass (g)"
boxlabs <- c("Aquaponics", "Containerized")
photolab <- expression(Photosynthesis~~(mu*mol~m^-2~s^-1))
condlab <- expression(Stomatal~Conductance~~(mol~m^-2~s^-1))
itelab <- expression(ITE~~(mmol~CO[2]~mol~H[2]*O^-1))
denslab <- expression(Stomatal~Density~~(mm^-2))
treelab <- c( "Shoots", "Roots")
tminlab <- expression(T[min])
tmaxlab <- expression(T[max])
denslab <- expression(Stomatal~Density~~(mm^-2))
slalab<- expression(Specific~Leaf~Area~~(cm^2~g^-1))
lmalab<- expression(Leaf~Mass~Area~~(g^1~m^2))
stomsizelab <- expression(Stomatal~size~~(um^2))
c13lab <-expression(paste(delta^{13}, "C (\u2030)"))

pchs <- c(15, 16, 17)
speciesnames <- c("Dogwood", "Maple", "Hawthorn")

uniqueID_label <- c("dogwood-city", "dogwood-park", "hawthorn-city", "hawthorn-park", "maple-city", "maple-park")

##time objects
startdate <- as.Date("2022-05-25")
startweek <- as.Date("2022-05-23")
startweek2 <- as.Date("MAY-23", format = "%B-%d")
axistime <- seq.Date(startweek, by="week", length=11,format = "%m-%d-%Y")
axistime2 <- seq.Date(startweek2, by="week", length=11,format = "%B-%d")
xlim1 <- as.Date(strptime("05-23-2022", format = "%m-%d-%Y", tz=""))
xlim2 <- as.Date(strptime("07-30-2022", format = "%m-%d-%Y", tz=""))
xlimdays <- c(xlim1, xlim2)
