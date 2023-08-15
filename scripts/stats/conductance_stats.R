source("scripts/functions.R")
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(mass)

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$site <- as.factor(gasex$site)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$date2 <- as.POSIXct(strptime(gasex$date, "%Y%m%d %H:%M:%S"))
  gasex$Date <- as.Date(gasex$date2, "%d-%m-%Y")
  gasex$ITE <- with(gasex, A/(E*1000))


##stats-----
test <- aov(gsw ~ treatment * site, data=gasex)
car::qqPlot(residuals(test))#pretty good
plot(test)
summary(test)
# 
# site <- TukeyHSD(test, which = "site")
species <- TukeyHSD(test, which = "treatment")
interaction <- TukeyHSD(test)

cld(glht(test, linfct=mcp(treatment = "Tukey")))

species_agg<- doBy::summaryBy(gsw ~ treatment + site, 
                              data =gasex, FUN=mean2, keep.names=TRUE)