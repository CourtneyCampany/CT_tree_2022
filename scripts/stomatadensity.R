# stomata density
 
source("scripts/functions.R")
source("scripts/plot_objects.R")

stomata <- read.csv("raw_data/stomata_density.csv")
  stomata$site <- as.factor(stomata$site)
  stomata$species <- as.factor(stomata$species)
  stomata$Date <- as.Date(stomata$date, "%m/%d/%Y")
  stomata$uniqueID <- paste(stomata$species, stomata$site, sep = "-")
  
stomata$stomatadens_mm2 <- with(stomata, (stomata_count/(pi * (fov_diam_mm/2)^2)))

stomata_plantmean <- doBy::summaryBy(stomatadens_mm2 ~ site + species + plant_num + Date + 
                                       uniqueID,FUN=mean2, keep.names = TRUE, data=stomata)

#stomata pooled-----
jpeg(filename = "figures/stomatadensity_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(stomatadens_mm2 ~ uniqueID, data=stomata_plantmean,varwidth=TRUE,xaxt='n',
        ylab=denslab,border=trtcols,ylim=c(0,850),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels=FALSE)
text(x=1:6, y= par("usr")[3] - 85, labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(stomatadens_mm2 ~ uniqueID, data=stomata_plantmean,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
legend("topleft", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.5)
dev.off()

##stomata density through time-----

stomata_agg <- doBy::summaryBy(stomatadens_mm2 ~ site + species + Date + 
                uniqueID,FUN=c(mean2,se), data=stomata_plantmean)
                
maple <- stomata_agg[stomata_agg$species == "m",]
dogwood <- stomata_agg[stomata_agg$species == "d",]
hawthorn <- stomata_agg[stomata_agg$species == "h",]       

jpeg(filename = "figures/stomata_time.jpeg",
     width = 8, height = 8, units = "in", res= 500)

par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

par(mar=c(0,5,1,1))
plot (stomatadens_mm2.mean2 ~ Date, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', cex=1.5,
      ylim=c(0,850), xlab="", xaxt='n', ylab="", xlim = xlimdays)
axis.Date(1, at=axistime, labels=FALSE)

with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2-stomatadens_mm2.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2+stomatadens_mm2.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))

points(stomatadens_mm2.mean2 ~ Date, data = maple[maple$site=='c',], col=citycol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='c',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2-stomatadens_mm2.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2+stomatadens_mm2.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))


points(stomatadens_mm2.mean2 ~ Date, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2-stomatadens_mm2.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2+stomatadens_mm2.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))

legend("bottomright", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=startdate, y=750, "City", font=3, cex=1.25)

par(mar=c(1,5,0,1))
plot(stomatadens_mm2.mean2 ~ Date, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b',
     ylim=c(0,800), xlab="",  ylab="", cex=1.5, xlim = xlimdays, xaxt='n')
axis.Date(1, at=axistime, labels=FALSE)
text(x=axistime2, y= par("usr")[3] - 70, labels = axistime, xpd=NA, srt=30, adj=.565, 
     cex=1)

with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2-stomatadens_mm2.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2+stomatadens_mm2.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))

points(stomatadens_mm2.mean2 ~ Date, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='p',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2-stomatadens_mm2.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2+stomatadens_mm2.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))

points(stomatadens_mm2.mean2 ~ Date, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2-stomatadens_mm2.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=stomatadens_mm2.mean2, y1=stomatadens_mm2.mean2+stomatadens_mm2.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
mtext(denslab, side=2, las=3, line=2.5, at=800, cex=1.25)
text(x=startdate, y=750, "Park", font=3, cex=1.25)
dev.off()



                