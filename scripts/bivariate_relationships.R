source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$site <- as.factor(gasex$site)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$date2 <- as.POSIXct(strptime(gasex$date, "%Y%m%d %H:%M:%S"))
  gasex$Date <- as.Date(gasex$date2, "%d-%m-%Y")
  gasex$ITE <- with(gasex, A/(E*1000))
  gasex$uniqueID <- paste(gasex$treatment, gasex$site, sep = "-")


maple <- gasex[gasex$treatment == "m",]
dogwood <- gasex[gasex$treatment == "d",]
hawthorn <- gasex[gasex$treatment == "h",] 

#simple models ------
maple_mod <- lm(A ~ gsw, data=maple)
dog_mod <- lm(A ~ gsw, data=dogwood)
hawthorn_mod <- lm(A ~ gsw, data=hawthorn)

allmod <- lm(A ~ gsw,data = gasex)

jpeg(filename = "figures/photosyn_vs_conductance.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
plot(A ~ gsw, data=gasex, ylab=photolab, pch=pchs[treatment],col=trtcols2[site],
     xlab=condlab, cex=1.25, ylim=c(0,30))

predline(allmod, col="black", lwd=2, lty=2)

text(.125, 20, expression(paste(R^{"2"}," = "," 0.74")), 1.5)
legend("topleft", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.25)
legend("bottomright", pch=pchs,legend=speciesnames, bty='n', cex=1.25)

dev.off()