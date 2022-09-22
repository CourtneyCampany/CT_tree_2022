### lma

source("scripts/functions.R")
source("scripts/plot_objects.R")

thick <- read.csv("raw_data/leafthick.csv")
  thick$site <- as.factor(thick$site)
  thick$species <- as.factor(thick$species)
  thick$Date <- as.Date(thick$date, "%m/%d/%Y")
  thick$uniqueID <- paste(thick$species, thick$site, sep = "-")
  thick$sla_cm2g <- with(thick, area_cm2/drymass_g)
  thick$lma_gm2 <- with(thick, drymass_g/(area_cm2*0.0001))


#lma------
jpeg(filename = "figures/lma_pooled.jpeg",
       width = 8, height = 6, units = "in", res= 500)
  
par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(lma_gm2 ~ uniqueID, data=thick,varwidth=TRUE,xaxt='n',
          ylab=lmalab,border=trtcols,ylim=c(0,230),xlab="",outline=FALSE,
          boxlwd=2, whisklwd=2,staplelwd=2)
  axis(1, labels=FALSE)
  text(x=1:6, y= par("usr")[3] - 15, labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
  stripchart(lma_gm2 ~ uniqueID, data=thick,
             vertical = TRUE, method = "jitter",cex=1.25,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
legend("topright", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
         bty='n', cex=1.5)
dev.off()
  
#sla----
jpeg(filename = "figures/sla_pooled.jpeg",
       width = 8, height = 6, units = "in", res= 500)
  
par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(sla_cm2g ~ uniqueID, data=thick,varwidth=TRUE,xaxt='n',
          ylab=slalab,border=trtcols,ylim=c(50,285),xlab="",outline=FALSE,
          boxlwd=2, whisklwd=2,staplelwd=2)
  axis(1, labels=FALSE)
  text(x=1:6, y= par("usr")[3] - 25, labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
  stripchart(sla_cm2g ~ uniqueID, data=thick,
             vertical = TRUE, method = "jitter",cex=1.25,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
legend("topleft", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
         bty='n', cex=1.5)
dev.off()


##lma time-----
lma_agg <- doBy::summaryBy(lma_gm2 ~ site + species + Date + 
                                 uniqueID,FUN=c(mean2,se), data=thick)

maple <- lma_agg[lma_agg$species == "m",]
dogwood <- lma_agg[lma_agg$species == "d",]
hawthorn <- lma_agg[lma_agg$species == "h",]       

jpeg(filename = "figures/lma_time.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

par(mar=c(0,5,1,1))
plot (lma_gm2.mean2 ~ Date, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', cex=1.5,
      ylim=c(0,230), xlab="", xaxt='n', ylab="", xlim = xlimdays)
axis.Date(1, at=axistime, labels=FALSE)

with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2-lma_gm2.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2+lma_gm2.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))

points(lma_gm2.mean2 ~ Date, data = maple[maple$site=='c',], col=citycol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='c',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2-lma_gm2.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2+lma_gm2.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))


points(lma_gm2.mean2 ~ Date, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2-lma_gm2.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2+lma_gm2.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))

legend("bottomright", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=startdate, y=145, "City", font=3, cex=1.25)

par(mar=c(1,5,0,1))
plot(lma_gm2.mean2 ~ Date, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b',
     ylim=c(0,230), xlab="",  ylab="", cex=1.5, xlim = xlimdays, xaxt='n')
axis.Date(1, at=axistime, labels=FALSE)
text(x=axistime2, y= par("usr")[3] - 25, labels = axistime, xpd=NA, srt=30, adj=.565, 
     cex=1)

with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2-lma_gm2.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2+lma_gm2.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))

points(lma_gm2.mean2 ~ Date, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='p',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2-lma_gm2.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2+lma_gm2.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))

points(lma_gm2.mean2 ~ Date, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2-lma_gm2.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=lma_gm2.mean2, y1=lma_gm2.mean2+lma_gm2.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
mtext(lmalab, side=2, las=3, line=2.5, at=230, cex=1.25)
text(x=startdate, y=145, "Park", font=3, cex=1.25)
dev.off()

