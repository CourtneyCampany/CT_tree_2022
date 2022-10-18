## ground temp

source("scripts/functions.R")
source("scripts/plot_objects.R")

ground <- read.csv("raw_data/ground_temperature.csv")
  ground$site <- as.factor(ground$site)
  ground$species <- as.factor(ground$species)
  ground$Date <- as.Date(ground$date, "%m/%d/%Y")
  ground$uniqueID <- paste(ground$species, ground$site, sep = "-")

ground$temp_c <- f2c(ground$temperature_F)

#stats
ground_mean <- doBy::summaryBy(temp_c ~ site + species + Date + replicate,
                                FUN=mean2, data=ground)

citymean <- mean(ground_mean[ground_mean$site=="c", "temp_c.mean2"])
parkmean <- mean(ground_mean[ground_mean$site=="p", "temp_c.mean2"])

groundtest <- t.test(ground_mean[ground_mean$site=="c", "temp_c.mean2"],
                     ground_mean[ground_mean$site=="p", "temp_c.mean2"])


#ground temp_pooled
jpeg(filename = "figures/ground_temp_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(temp_c ~ uniqueID, data=ground,varwidth=TRUE,xaxt='n',
        ylab="Ground Temperature  (C)",border=trtcols,ylim=c(7,45),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels=FALSE)
text(x=1:6, y= par("usr")[3] - 5, labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(temp_c ~ uniqueID, data=ground,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols3, xaxt='n', add=TRUE) 
legend("bottomleft", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.5)
dev.off()

##ground temp through time

ground_agg <- doBy::summaryBy(temp_c ~ site + species + Date + 
                                 uniqueID,FUN=c(mean2,se), data=ground)



maple <- ground_agg[ground_agg$species == "m",]
dogwood <- ground_agg[ground_agg$species == "d",]
hawthorn <- ground_agg[ground_agg$species == "h",]       

jpeg(filename = "figures/ground_temp_time.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

par(mar=c(0,5,1,1))
plot (temp_c.mean2 ~ Date, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', cex=1.5,
      ylim=c(10,45), xlab="", xaxt='n', ylab="", xlim = xlimdays)
axis.Date(1, at=axistime, labels=FALSE)

with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2-temp_c.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2+temp_c.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))

points(temp_c.mean2 ~ Date, data = maple[maple$site=='c',], col=citycol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='c',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2-temp_c.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2+temp_c.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))

points(temp_c.mean2 ~ Date, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2-temp_c.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2+temp_c.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))

legend("bottomright", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=startdate, y=43, "City", font=3, cex=1.25)

par(mar=c(1,5,0,1))
plot(temp_c.mean2 ~ Date, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b',
     ylim=c(10,45), xlab="",  ylab="", cex=1.5, xlim = xlimdays, xaxt='n')
axis.Date(1, at=axistime, labels=FALSE)
text(x=axistime2, y= par("usr")[3] - 4, labels = axistime, xpd=NA, srt=30, adj=.565, 
     cex=1)

with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2-temp_c.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2+temp_c.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))

points(temp_c.mean2 ~ Date, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='p',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2-temp_c.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2+temp_c.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))

points(temp_c.mean2 ~ Date, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2-temp_c.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=temp_c.mean2, y1=temp_c.mean2+temp_c.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
mtext("Ground Temperature  (C)", side=2, las=3, line=2.5, at=45, cex=1.25)
text(x=startdate, y=43, "Park", font=3, cex=1.25)
dev.off()


#ground temp_pooled by site
jpeg(filename = "figures/ground_temp_site.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(temp_c ~ site, data=ground,varwidth=TRUE,xlab="",xaxt='n',
        ylab="Ground Temperature  (C)",border=trtcols,ylim=c(7,45),outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(side=1, at = 1:2, labels = c("City", "Park"), cex.axis=1.25)
stripchart(temp_c ~ site, data=ground,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols3, xaxt='n', add=TRUE) 
# legend("bottomleft", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
#        bty='n', cex=1.5)
dev.off()


