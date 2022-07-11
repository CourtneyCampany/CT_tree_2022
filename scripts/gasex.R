source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
        gasex$site <- as.factor(gasex$site)
        gasex$treatment <- as.factor(gasex$treatment)
        gasex$date2 <- as.POSIXct(strptime(gasex$date, "%Y%m%d %H:%M:%S"))
        gasex$Date <- as.Date(gasex$date2, "%d-%m-%Y")
        gasex$ITE <- with(gasex, A/(E*1000))
        gasex$uniqueID <- paste(gasex$treatment, gasex$site, sep = "-")

#extract species ---------
#boxplot, with species by site pooled (no time)

#photosynthesis
jpeg(filename = "figures/photosynthesis_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(A ~ uniqueID, data=gasex,varwidth=TRUE,xaxt='n',
        ylab=photolab,border=trtcols,ylim=c(0,25),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels=FALSE)
text(x=1:6, y= par("usr")[3] - 2, labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(A ~ uniqueID, data=gasex,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
legend("topright", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.5)
dev.off()

#stomatal conductance
jpeg(filename = "figures/stomatalconductance_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)
par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(gsw ~ uniqueID, data=gasex,varwidth=TRUE,xaxt='n',
        ylab=condlab,border=trtcols,ylim=c(0,.45),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels=FALSE)
text(x=1:6, y= par("usr")[3]-.04 , labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(gsw ~ uniqueID, data=gasex,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
legend("topright", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.5)
dev.off()

#ITE
jpeg(filename = "figures/wateruseefficiency_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)
par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(ITE ~ uniqueID, data=gasex,varwidth=TRUE,xaxt='n',
        ylab=itelab,border=trtcols,ylim=c(1,7),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels=FALSE)
text(x=1:6, y= par("usr")[3]-.5 , labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(ITE ~ uniqueID, data=gasex,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
legend("topright", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.5)
dev.off()

## species through time-----------------
gasex_agg <- doBy::summaryBy(A + gsw + ITE ~ treatment + site + Date, data=gasex, 
                             FUN=c(mean, se))

maple <- gasex_agg[gasex_agg$treatment == "m",]
dogwood <- gasex_agg[gasex_agg$treatment == "d",]
hawthorn <- gasex_agg[gasex_agg$treatment == "h",] 

##oneplot-----------------
plot (A ~ Date, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', 
      ylim=c(0,20))
points(A ~ Date, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b')

points(A ~ Date, data = maple[maple$site=='c',], col=citycol, pch=16, type='b')
points(A ~ Date, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b')

points(A ~ Date, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b')
points(A ~ Date, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b')

legend("bottomright",pt.bg=trtcols,pch=21,legend=trtlab,inset=.01,  bty='n',
       cex=1)

with(ratio_agg, arrows(x0=log10(fineroot.mean), y0=log10(leafmass.mean), y1=log10(leafmass.mean-leafmass.se), angle=90, 
                       length=0.05,col=palette(), lwd=1))

#2 panel photosynth----------------

# windows()
jpeg(filename = "figures/photosyn_time.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

par(mar=c(0,5,1,1))
plot (A.mean ~ Date, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', cex=1.5,
      ylim=c(0,20.5), xlab="", xaxt='n', ylab="", xlim = xlimdays)
axis.Date(1, at=axistime, labels=FALSE)

with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=A.mean, y1=A.mean-A.se, angle=90,length=0.05,
                     col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=A.mean, y1=A.mean+A.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))

points(A.mean ~ Date, data = maple[maple$site=='c',], col=citycol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='c',], arrows(x0=Date, y0=A.mean, y1=A.mean-A.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=Date, y0=A.mean, y1=A.mean+A.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))


points(A.mean ~ Date, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=A.mean, y1=A.mean-A.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=A.mean, y1=A.mean+A.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))

legend("bottomleft", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=startdate, y=19.5, "City", font=3, cex=1.25)

par(mar=c(1,5,0,1))
plot(A.mean ~ Date, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b',
     ylim=c(0,20.5), xlab="",  ylab="", cex=1.5, xlim = xlimdays, xaxt='n')
axis.Date(1, at=axistime, labels=FALSE)
text(x=axistime2, y= par("usr")[3] - 2, labels = axistime, xpd=NA, srt=30, adj=.565, 
     cex=1)

with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=A.mean, y1=A.mean-A.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=A.mean, y1=A.mean+A.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))

points(A.mean ~ Date, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='p',], arrows(x0=Date, y0=A.mean, y1=A.mean-A.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=Date, y0=A.mean, y1=A.mean+A.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))

points(A.mean ~ Date, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=A.mean, y1=A.mean-A.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=A.mean, y1=A.mean+A.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
mtext(photolab, side=2, las=3, line=2.5, at=20, cex=1.25)
text(x=startdate, y=19.5, "Park", font=3, cex=1.25)
dev.off()

# 2 panel gsw--------------------


jpeg(filename = "figures/gsw_time.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

par(mar=c(0,5,1,1))
plot (gsw.mean ~ Date, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', cex=1.5,
      ylim=c(0,.3), xlab="", xaxt='n', ylab="", xlim = xlimdays)
axis.Date(1, at=axistime, labels=FALSE)

with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean-gsw.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean+gsw.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))

points(gsw.mean ~ Date, data = maple[maple$site=='c',], col=citycol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='c',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean-gsw.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean+gsw.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))


points(gsw.mean ~ Date, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean-gsw.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean+gsw.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))

legend("bottomleft", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=startdate, y=.28, "City", font=3, cex=1.25)

par(mar=c(1,5,0,1))
plot(gsw.mean ~ Date, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b',
     ylim=c(0,.3), xlab="",  ylab="", cex=1.5, xlim = xlimdays, xaxt='n')
axis.Date(1, at=axistime, labels=FALSE)
text(x=axistime2, y= par("usr")[3] - .035, labels = axistime, xpd=NA, srt=30, adj=.565, 
     cex=1)

with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean-gsw.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean+gsw.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))

points(gsw.mean ~ Date, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='p',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean-gsw.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean+gsw.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))

points(gsw.mean ~ Date, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean-gsw.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=gsw.mean, y1=gsw.mean+gsw.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
mtext(condlab, side=2, las=3, line=3, at=.3, cex=1.25)
text(x=startdate, y=.28, "Park", font=3, cex=1.25)
dev.off()


