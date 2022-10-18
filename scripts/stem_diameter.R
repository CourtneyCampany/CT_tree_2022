source("scripts/functions.R")
source("scripts/plot_objects.R")

dbh<- read.csv("raw_data/dbh.csv")
  dbh$site <- as.factor(dbh$site)
  dbh$species <- as.factor(dbh$species)
  dbh$Date <- as.Date(dbh$date, "%m/%d/%Y")
  dbh$uniqueID <- paste(dbh$species, dbh$site, sep = "-")

  dbh_agg <- doBy::summaryBy(dbh_mm ~ Date+species+site, data=dbh, 
                            FUN=c(mean2, se))
  
  maple <- dbh_agg[dbh_agg$species == "m",]
  dogwood <- dbh_agg[dbh_agg$species == "d",]
  hawthorn <- dbh_agg[dbh_agg$species == "h",] 
  
#stats-------
start <- dbh[dbh$date == "5/24/2022", c("site", "species", "dbh_mm")]
finish <- dbh[dbh$date == "7/27/2022",c("site", "species", "dbh_mm")]
growth <- finish$dbh_mm - start$dbh_mm
  citymean <- mean(ground_mean[ground_mean$site=="c", "temp_c.mean2"])
  parkmean <- mean(ground_mean[ground_mean$site=="p", "temp_c.mean2"])
  
  groundtest <- t.test(ground_mean[ground_mean$site=="c", "temp_c.mean2"],
                       ground_mean[ground_mean$site=="p", "temp_c.mean2"])
  
#2 panel stem diameter
jpeg(filename = "figures/dbh_time.jpeg",
       width = 8, height = 6, units = "in", res= 500)
  
par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
      omi=c(.5,0,0.1,0.1))
  
par(mar=c(0,5,1,1))
plot (dbh_mm.mean2 ~ Date, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', cex=1.5,
        ylim=c(17,60), xlab="", xaxt='n', ylab="", xlim = xlimdays)
axis.Date(1, at=axistime, labels=FALSE)
  
with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2-dbh_mm.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2+dbh_mm.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))
  
points(dbh_mm.mean2 ~ Date, data = maple[maple$site=='c',], col=citycol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='c',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2-dbh_mm.se, angle=90,length=0.05,
                                       col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2+dbh_mm.se, angle=90,length=0.05,
                                       col=citycol, lwd=1))
  
points(dbh_mm.mean2 ~ Date, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2-dbh_mm.se, angle=90,length=0.05,
                                             col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2+dbh_mm.se, angle=90,length=0.05,
                                             col=citycol, lwd=1))
  
legend("bottomright", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=startdate, y=21, "City", font=3, cex=1.25)
  
par(mar=c(1,5,0,1))
plot(dbh_mm.mean2 ~ Date, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b',
       ylim=c(20,60), xlab="",  ylab="", cex=1.5, xlim = xlimdays, xaxt='n')
axis.Date(1, at=axistime, labels=FALSE)
text(x=axistime, y= par("usr")[3] - 5, labels = axistime, xpd=NA, srt=30, adj=.565, 
       cex=1)
  
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2-dbh_mm.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2+dbh_mm.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
  
points(dbh_mm.mean2 ~ Date, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='p',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2-dbh_mm.se, angle=90,length=0.05,
                                       col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2+dbh_mm.se, angle=90,length=0.05,
                                       col=parkcol, lwd=1))
  
points(dbh_mm.mean2 ~ Date, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2-dbh_mm.se, angle=90,length=0.05,
                                             col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=dbh_mm.mean2, y1=dbh_mm.mean2+dbh_mm.se, angle=90,length=0.05,
                                             col=parkcol, lwd=1))
mtext("Stem Diameter (mm)", side=2, las=3, line=2.5, at=60, cex=1.25)
text(x=startdate, y=25, "Park", font=3, cex=1.25)
dev.off()
  