source("scripts/functions.R")
source("scripts/plot_objects.R")

waterpot <- read.csv("raw_data/water_potentials.csv")
  waterpot$site <- as.factor(waterpot$site)
  waterpot$species <- as.factor(waterpot$species)
  waterpot$wp_mp <- with(waterpot, ((wp_bar/10)*-1))
  waterpot$Date <- as.Date(waterpot$date, "%m/%d/%Y")
  waterpot$uniqueID <- paste(waterpot$species, waterpot$site, sep = "-")
  
wp_agg <- doBy::summaryBy(wp_mp ~ Date+species+site, data=waterpot, 
                          FUN=c(mean2, se))

maple <- wp_agg[wp_agg$species == "m",]
dogwood <- wp_agg[wp_agg$species == "d",]
hawthorn <- wp_agg[wp_agg$species == "h",] 


jpeg(filename = "figures/waterpotential_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.25)
boxplot(wp_mp ~ uniqueID, data=waterpot,varwidth=TRUE,xaxt='n',
        ylab="Midday Leaf Water Potential (mPa)",border=trtcols,ylim=c(-3.5,0),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels=FALSE)
text(x=1:6, y= par("usr")[3] - .3, labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(wp_mp ~ uniqueID, data=waterpot,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
legend("bottomleft", legend = c("City", "Park"), lty =1, lwd=3, col=trtcols, inset=.01, 
       bty='n', cex=1.5)
dev.off()


### waterpotential through time (2 panel)

jpeg(filename = "figures/waterpot_time.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

par(mar=c(0,5,1,1))
plot (wp_mp.mean2 ~ Date, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, cex=1.5,
      ylim=c(-3.7,0), xlab="", xaxt='n', ylab = "", xlim = xlimdays, type='b')
axis.Date(1, at=axistime, labels=FALSE)

with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2-wp_mp.se, 
                                         angle=90,length=0.05,col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2+wp_mp.se,
                                         angle=90,length=0.05,col=citycol, lwd=1))

points(wp_mp.mean2 ~ Date, data = maple[maple$site=='c',], col=citycol, pch=16, cex=1.5, type='b')
with(maple[maple$site=='c',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2-wp_mp.se, 
                                     angle=90,length=0.05, col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2+wp_mp.se, 
                                     angle=90,length=0.05, col=citycol, lwd=1))

points(wp_mp.mean2 ~ Date, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, 
       cex=1.5, type='b')
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2-wp_mp.se,
                                           angle=90,length=0.05,col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2+wp_mp.se,
                                           angle=90,length=0.05,col=citycol, lwd=1))

legend("bottomleft", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=startdate, y=-0.25, "City", font=3, cex=1.25)

par(mar=c(1,5,0,1))
plot(wp_mp.mean2 ~ Date, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15,
     ylim=c(-3.7,0), xlab="",  ylab="", cex=1.5, xlim = xlimdays, xaxt='n', type='b')
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2-wp_mp.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2+wp_mp.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))

points(wp_mp.mean2 ~ Date, data = maple[maple$site=='p',], col=parkcol, pch=16, cex=1.5,type='b')
with(maple[maple$site=='p',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2-wp_mp.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2+wp_mp.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))

points(wp_mp.mean2 ~ Date, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, 
       cex=1.5,type='b')
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2-wp_mp.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=wp_mp.mean2, y1=wp_mp.mean2+wp_mp.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
text(x=axistime, y= par("usr")[3] - .4, labels = axistime, xpd=NA, srt=30, adj=.565, 
     cex=1)
axis.Date(1, at=axistime, labels=FALSE)
text(x=startdate, y=-.25, "Park", font=3, cex=1.25)
mtext("Midday Leaf Water Potential (mPa)", side=2, las=3, line=3, at=0, cex=1.25)

dev.off()


###weather data
library(plantecophys)

airvars <- read.csv("raw_data/weather_data_wunderground_hagerstown.csv")
  airvars$date <- as.Date(airvars$date, format = "%m/%d/%Y")
  airvars$temp_max_c <- with(airvars, f2c(temp_max))
  airvars$temp_avg_c <- with(airvars, f2c(temp_avg))
  airvars$temp_min_c <- with(airvars, f2c(temp_min))
  airvars$vpd_max <- RHtoVPD(airvars$humidity_max, airvars$temp_max_c)
  airvars$precipt_total_mm <- airvars$precip_total * 25.4
  
#plots
  
#temperature
jpeg(filename = "figures/temperature.jpeg",width = 8, height = 8, units = "in", res= 500)
par(mgp=c(2.5,.75,0), mar=c(5,5,1,2), cex.lab=1.5)
with(airvars, {plot(date, temp_max_c , type='l', col="red",ylim=c(10,37),lwd=3,
         xlab="",axes=FALSE,ylab=expression(T[min]~and~T[max]~(degree*C)),xlim=xlimdays,
         panel.first={
           addpoly(date, temp_min_c, temp_max_c ,col="grey95")
         })
    lines(date, temp_min_c, col="blue", lwd=2)
})
axis(2, cex=1.5)
axis.Date(1, at=axistime, labels=FALSE)
legend("bottomright",col=c("red","blue"),lty=1,lwd=2,legend=c(tmaxlab,tminlab), inset=.01, 
       cex=1.5, bty='n')
box()
text(x=axistime, y= par("usr")[3] - 1.5, labels = axistime, xpd=NA, srt=30, adj=.565, 
     cex=1.25)
dev.off()

#rain
jpeg(filename = "figures/precipitation.jpeg",width = 8, height = 8, units = "in", res= 500)
par(mgp=c(2.5,.75,0), mar=c(5,5,1,2), cex.lab=1.5)
plot(precipt_total_mm~date, type="l",col="black",xlab="",ylim=c(0,35),lwd=3,
     xlim=xlimdays,ylab="Daily Preciptiation (mm)",data=airvars, axes=FALSE)
axis(2, cex=1.5)
axis.Date(1, at=axistime, label=FALSE)
box()
text(x=axistime, y= par("usr")[3] - 3, labels = axistime, xpd=NA, srt=30, adj=.565, 
     cex=1.25)
dev.off()

#vpd

jpeg(filename = "figures/vpd.jpeg",
     width = 8, height = 8, units = "in", res= 500)
par(mgp=c(2.5,.75,0), mar=c(5,5,1,2), cex.lab=1.5)
plot(vpd_max~date, type="l",col="forestgreen",xlab="",lwd=2,ylim=c(0,3.5),xlim=xlimdays,
     ylab=expression(VPD[max]~~(kPa)),data=airvars, axes=FALSE)
axis(2)
axis.Date(1, at=axistime, label=FALSE)
box()
text(x=axistime, y= par("usr")[3] - .3, labels = axistime, xpd=NA, srt=30, adj=.565, 
     cex=1.25)

dev.off()
