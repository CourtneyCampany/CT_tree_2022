source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$site <- as.factor(gasex$site)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$date2 <- as.POSIXct(strptime(gasex$date, "%Y%m%d %H:%M:%S"))
  gasex$Date <- as.Date(gasex$date2, "%d-%m-%Y")
  gasex$ITE <- with(gasex, A/(E*1000))
  gasex$uniqueID <- paste(gasex$treatment, gasex$site, sep = "-")

city <- 
park

maple <- gasex[gasex$treatment == "m",]
dogwood <- gasex[gasex$treatment == "d",]
hawthorn <- gasex[gasex$treatment == "h",] 

#simple models ------
terr_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=terr)
hemi_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=hemi)
epi_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=epi)
  
plot(A ~ gsw, data=gasex, ylab=photolab, pch=16,col=trtcols[site],xlab=condlab)
  axis(2, labels=FALSE, tcl=-.25)
  axis(2, labels=FALSE, tcl=.25)
  predline(terr_mod_xa, col=trtcols[1], lwd=2, lty=2)
  predline(hemi_mod_xa, col=trtcols[2], lwd=2, lty=2)
  predline(epi_mod_xa, col=trtcols[3], lwd=2, lty=2)
  points(stipe_length_cm ~ xylem_area_mm2, data=alldata3, col=trtcols2[niche2],
         pch=16,cex=1.25)
  legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01,1.15)
  text(.8, 0, "B", cex=1.25)
  text(.85, 16, expression(paste(R[cond]^{"2"}," = "," 0.33")), 1.25)
  text(.85, 9, expression(paste(R[marg]^{"2"}," = "," 0.88")), 1.25)