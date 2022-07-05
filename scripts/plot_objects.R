# plot objects

parkcol <- "forestgreen"
citycol <- "gray25"

trtcols <- c(citycol, parkcol)
trtcols2 <- scales::alpha(trtcols, .6)

ratio_lab <- "Root to Shoot ratio"
biomasslab <- "Total Biomass (g)"
boxlabs <- c("Aquaponics", "Containerized")
photolab <- expression(italic(A)~~(mu*mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
itelab <- expression(ITE~~(mmol~CO[2]~mol~H[2]*O^-1))
denslab <- expression(Stomatal~Density~~(mm^-2))
treelab <- c( "Shoots", "Roots")

pchs <- c(15, 16, 17)
speciesnames <- c("Dogwood", "Maple", "Hawthorn")
