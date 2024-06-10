source("scripts/functions.R")
source("scripts/plot_objects.R")

#Does 13C shift through time?

alldata <- read.csv("calculated_data/alldata.csv")
  
##stats-----------------------------------------------------------
library(multcomp)
library(smatr)
library(emmeans)
library(car)
  
isotopes_mod <- aov(c13 ~ species * site, data=alldata)
car::qqPlot(residuals(isotopes_mod))#pretty good
plot(isotopes_mod)
summary(isotopes_mod) #species x site interaction (species main effect)
interaction <- TukeyHSD(isotopes_mod)
  
#13C overall by treatment (no time) -----------------------------
jpeg(filename = "figures/c13_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(c13 ~ uniqueid, data=alldata,varwidth=TRUE,xaxt='n',
        ylab=c13lab,border=trtcols,ylim=c(-30,-23),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels=FALSE)
text(x=1:6, y= par("usr")[3]-.5 , labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(c13 ~ uniqueid, data=alldata,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
legend("topright", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.5)
dev.off()


## species through time-----------------
element_agg <- doBy::summaryBy(c13 ~ species + site + week, data=alldata,
                             FUN=c(mean, se))

maple <- element_agg[element_agg$species == "m",]
dogwood <- element_agg[element_agg$species == "d",]
hawthorn <- element_agg[element_agg$species == "h",] 

##13c through time-----------------
jpeg(filename = "figures/isotopes_time.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

par(mar=c(0,5,1,1))
#city
plot (c13.mean ~ week, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', cex=1.5,
      ylim=c(-30,-23), xlab="", ylab="", xaxt='n')
axis(1, labels=FALSE, at=1:10)
with(dogwood[dogwood$site=='c',], arrows(x0=week, y0=c13.mean, y1=c13.mean-c13.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=week, y0=c13.mean, y1=c13.mean+c13.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))

points(c13.mean ~ week, data = maple[maple$site=='c',], col=citycol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='c',], arrows(x0=week, y0=c13.mean, y1=c13.mean-c13.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=week, y0=c13.mean, y1=c13.mean+c13.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))

points(c13.mean ~ week, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='c',], arrows(x0=week, y0=c13.mean, y1=c13.mean-c13.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=week, y0=c13.mean, y1=c13.mean+c13.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))

legend("topright", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=1, y=-30, "City", font=3, cex=1.25)

#park
par(mar=c(1,5,0,1))
plot(c13.mean ~ week, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b',
     ylim=c(-30,-23), xlab="",  ylab="", cex=1.5)
# text(x=axistime2, y= par("usr")[3] - 4, labels = axistime, xpd=NA, srt=30, adj=.565, 
#      cex=1)

with(dogwood[dogwood$site=='p',], arrows(x0=week, y0=c13.mean, y1=c13.mean-c13.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=week, y0=c13.mean, y1=c13.mean+c13.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))

points(c13.mean ~ week, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='p',], arrows(x0=week, y0=c13.mean, y1=c13.mean-c13.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=week, y0=c13.mean, y1=c13.mean+c13.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))

points(c13.mean ~ week, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='p',], arrows(x0=week, y0=c13.mean, y1=c13.mean-c13.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=week, y0=c13.mean, y1=c13.mean+c13.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
mtext(c13lab, side=2, las=3, line=2.5, at=-23.5, cex=1.25)
mtext("Week", side=1, at=5, line=2.5, cex=1.25)
text(x=1, y=-30, "Park", font=3, cex=1.25)

dev.off()
  