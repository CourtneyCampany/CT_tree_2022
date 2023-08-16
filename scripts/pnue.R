source("scripts/functions.R")
source("scripts/plot_objects.R")

##does nitrogen use efficiency change through growing season?

alldata <- read.csv("calculated_data/alldata.csv")

#pnue stats---------------------
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(visreg)

ratio_mod <- aov(nue ~ species * site, data=alldata)
car::qqPlot(residuals(ratio_mod))#pretty good
plot(ratio_mod)
summary(ratio_mod) #species & site, no interaction
visreg(ratio_mod, "nue", by="species")
visreg(ratio_mod, "nue", by="site")

#nue overall by treatment (no time) -----------------------------
jpeg(filename = "figures/nue_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(nue ~ uniqueid, data=alldata,varwidth=TRUE,xaxt='n',
        ylab=nuelab2,border=trtcols,ylim=c(0,135),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels = FALSE)
text(x=1:6, y= par("usr")[3]-12 , labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(nue ~ uniqueid, data=alldata,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
legend("topright", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.25)
dev.off()


## species through time-----------------
nue_agg <- doBy::summaryBy(nue ~ species + site + week, data=alldata,
                               FUN=c(mean2, se))

maple <- nue_agg[nue_agg$species == "m",]
dogwood <- nue_agg[nue_agg$species == "d",]
hawthorn <- nue_agg[nue_agg$species == "h",] 

##13c through time-----------------
jpeg(filename = "figures/pnue_time.jpeg",
     width = 8, height = 6, units = "in", res= 500)
par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

par(mar=c(0,5,1,1))
#city
plot (nue.mean2 ~ week, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', cex=1.5,
      ylim=c(0,135), xlab="", ylab="", xaxt='n')
axis(1, labels=FALSE, at=1:10)

with(dogwood[dogwood$site=='c',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2-nue.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2+nue.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))

points(nue.mean2 ~ week, data = maple[maple$site=='c',], col=citycol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='c',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2-nue.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2+nue.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))

points(nue.mean2 ~ week, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='c',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2-nue.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2+nue.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))

legend("topright", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=1, y=10, "City", font=3, cex=1.25)

#park
par(mar=c(1,5,0,1))
plot(nue.mean2 ~ week, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b',
     ylim=c(0,130), xlab="",  ylab="", cex=1.5, xaxt='n')
axis(1, at=1:10, labels = TRUE)

with(dogwood[dogwood$site=='p',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2-nue.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2+nue.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))

points(nue.mean2 ~ week, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='p',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2-nue.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2+nue.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))

points(nue.mean2 ~ week, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='p',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2-nue.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=week, y0=nue.mean2, y1=nue.mean2+nue.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
mtext(nuelab2, side=2, las=3, line=2.5, at=130, cex=1.25)
mtext("Week", side=1, at=5, line=2.5, cex=1.25)
text(x=1, y=10, "Park", font=3, cex=1.25)
dev.off()

