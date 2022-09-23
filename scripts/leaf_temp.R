## leaf temp
source("scripts/functions.R")
source("scripts/plot_objects.R")

leaft <- read.csv("raw_data/leaf_temperature.csv")
leaft$site <- as.factor(leaft$site)
leaft$species <- as.factor(leaft$species)
leaft$Date <- as.Date(leaft$date, "%m/%d/%Y")
leaft$uniqueID <- paste(leaft$species, leaft$site, sep = "-")

leaft$top_c <- f2c(leaft$top_F)
leaft$bottom_c <- f2c(leaft$bottom_F)

#leaftop temp_pooled
jpeg(filename = "figures/leaftop_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(top_c ~ uniqueID, data=leaft,varwidth=TRUE,xaxt='n',
        ylab="Top Leaf Temperature  (C)",border=trtcols,ylim=c(7,45),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels=FALSE)
text(x=1:6, y= par("usr")[3] - 3, labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(top_c ~ uniqueID, data=leaft,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols3, xaxt='n', add=TRUE) 
legend("bottomleft", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.5)
dev.off()

#leafbottom temp_pooled
jpeg(filename = "figures/leafbottom_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(bottom_c ~ uniqueID, data=leaft,varwidth=TRUE,xaxt='n',
        ylab="Top Leaf Temperature  (C)",border=trtcols,ylim=c(7,45),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels=FALSE)
text(x=1:6, y= par("usr")[3] - 3, labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(bottom_c ~ uniqueID, data=leaft,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols3, xaxt='n', add=TRUE) 
legend("bottomleft", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.5)
dev.off()

##comparison of top and bottom of leaf
jpeg(filename = "figures/leaftemp_topbottom.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
plot(top_c ~ bottom_c, data=leaft, ylim=c(10, 40), xlim=c(10, 40), col=trtcols2[site], 
     pch=pchs[species], ylab="Top Leaf Temperature (C)",xlab="Bottom Leaf Temperature (C)",
     cex=1.25)
legend("topleft", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.25)
legend("bottomright", pch=pchs,legend=speciesnames, bty='n', cex=1)
abline(a=0, b=1, lwd =3, lty=2)
dev.off()

##stomata density through time

leaft_agg <- doBy::summaryBy(top_c + bottom_c ~ site + species + Date + 
                                 uniqueID,FUN=c(mean2,se), data=leaft)

maple <- leaft_agg[leaft_agg$species == "m",]
dogwood <- leaft_agg[leaft_agg$species == "d",]
hawthorn <- leaft_agg[leaft_agg$species == "h",]       

jpeg(filename = "figures/topc_temp_time.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

par(mar=c(0,5,1,1))
plot (top_c.mean2 ~ Date, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', cex=1.5,
      ylim=c(10,45), xlab="", xaxt='n', ylab="", xlim = xlimdays)
axis.Date(1, at=axistime, labels=FALSE)

with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2-top_c.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2+top_c.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))

points(top_c.mean2 ~ Date, data = maple[maple$site=='c',], col=citycol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='c',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2-top_c.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2+top_c.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))

points(top_c.mean2 ~ Date, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2-top_c.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2+top_c.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))

legend("bottomright", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=startdate, y=43, "City", font=3, cex=1.25)

par(mar=c(1,5,0,1))
plot(top_c.mean2 ~ Date, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b',
     ylim=c(10,45), xlab="",  ylab="", cex=1.5, xlim = xlimdays, xaxt='n')
axis.Date(1, at=axistime, labels=FALSE)
text(x=axistime2, y= par("usr")[3] - 4, labels = axistime, xpd=NA, srt=30, adj=.565, 
     cex=1)

with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2-top_c.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2+top_c.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))

points(top_c.mean2 ~ Date, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='p',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2-top_c.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2+top_c.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))

points(top_c.mean2 ~ Date, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2-top_c.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=top_c.mean2, y1=top_c.mean2+top_c.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
mtext("leaft Temperature  (C)", side=2, las=3, line=2.5, at=45, cex=1.25)
text(x=startdate, y=43, "Park", font=3, cex=1.25)
dev.off()



