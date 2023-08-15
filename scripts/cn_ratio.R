source("scripts/functions.R")
source("scripts/plot_objects.R")

element <- read.csv("raw_data/isotope_data.csv")
element$species <- as.factor(element$species)
element$site <- as.factor(element$site)
element$uniqueid <- as.factor(paste(element$species, element$site, sep="-"))
element$Date <- as.Date(element$date, "%m/%d/%Y")
element$cnratio <- with(element, carbon_perc/nitro_perc )

##stats-----------------------------------------------------------
library(multcomp)
library(smatr)
library(emmeans)
library(car)

ratio_mod <- aov(cnratio ~ species * site, data=element)
car::qqPlot(residuals(ratio_mod))#pretty good
plot(ratio_mod)
summary(ratio_mod) #species 


#13C overall by treatment (no time) -----------------------------
jpeg(filename = "figures/cnratio_pooled.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(cnratio ~ uniqueid, data=element,varwidth=TRUE,xaxt='n',
        ylab="Foliar C:N ratio",border=trtcols,ylim=c(10,55),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, labels=FALSE)
text(x=1:6, y= par("usr")[3]-2.5 , labels = uniqueID_label, xpd=NA, srt=20, adj=.565, cex=1.25)
stripchart(cnratio ~ uniqueid, data=element,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
legend("topright", legend = c("City", "Park"), lty =1, lwd=4, col=trtcols, inset=.01, 
       bty='n', cex=1.5)
dev.off()


## species through time-----------------
element_agg <- doBy::summaryBy(cnratio ~ species + site + Date, data=element,
                               FUN=c(mean, se))

maple <- element_agg[element_agg$species == "m",]
dogwood <- element_agg[element_agg$species == "d",]
hawthorn <- element_agg[element_agg$species == "h",] 

##13c through time-----------------
par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

par(mar=c(0,5,1,1))
#city
plot (cnratio.mean ~ Date, data = dogwood[dogwood$site=='c',], col=citycol, pch=15, type='b', cex=1.5,
      ylim=c(10,55), xlab="", ylab="")

with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean-cnratio.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))
with(dogwood[dogwood$site=='c',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean+cnratio.se, angle=90,length=0.05,
                                         col=citycol, lwd=1))

points(cnratio.mean ~ Date, data = maple[maple$site=='c',], col=citycol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='c',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean-cnratio.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))
with(maple[maple$site=='c',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean+cnratio.se, angle=90,length=0.05,
                                     col=citycol, lwd=1))

points(cnratio.mean ~ Date, data = hawthorn[hawthorn$site=='c',], col=citycol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean-cnratio.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))
with(hawthorn[hawthorn$site=='c',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean+cnratio.se, angle=90,length=0.05,
                                           col=citycol, lwd=1))

legend("topright", pch=pchs,legend=speciesnames, bty='n', cex=1)
text(x=startdate, y=50, "City", font=3, cex=1.25)

#park
par(mar=c(1,5,0,1))
plot(cnratio.mean ~ Date, data = dogwood[dogwood$site=='p',], col=parkcol, pch=15, type='b',
     ylim=c(10,55), xlab="",  ylab="", cex=1.5)
# text(x=axistime2, y= par("usr")[3] - 4, labels = axistime, xpd=NA, srt=30, adj=.565, 
#      cex=1)

with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean-cnratio.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))
with(dogwood[dogwood$site=='p',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean+cnratio.se, angle=90,length=0.05,
                                         col=parkcol, lwd=1))

points(cnratio.mean ~ Date, data = maple[maple$site=='p',], col=parkcol, pch=16, type='b', cex=1.5)
with(maple[maple$site=='p',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean-cnratio.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))
with(maple[maple$site=='p',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean+cnratio.se, angle=90,length=0.05,
                                     col=parkcol, lwd=1))

points(cnratio.mean ~ Date, data = hawthorn[hawthorn$site=='p',], col=parkcol, pch=17, type='b', cex=1.5)
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean-cnratio.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
with(hawthorn[hawthorn$site=='p',], arrows(x0=Date, y0=cnratio.mean, y1=cnratio.mean+cnratio.se, angle=90,length=0.05,
                                           col=parkcol, lwd=1))
mtext("Foliar C:N ratio", side=2, las=3, line=2.5, at=55, cex=1.25)
text(x=startdate, y=50, "Park", font=3, cex=1.25)


