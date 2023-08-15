source("scripts/functions.R")

library(visreg)
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(lmerTest)
library(LMERConvenienceFunctions)

dbh<- read.csv("raw_data/dbh_growth_basic.csv")
  dbh$site <- as.factor(dbh$site)
  dbh$species <- as.factor(dbh$species)
  dbh$Date <- as.Date(dbh$date, "%m/%d/%Y")
  dbh$uniqueID <- paste(dbh$species, dbh$site, sep = "-")


maple <- dbh[dbh$species == "m",]
dogwood <- dbh[dbh$species == "d",]
hawthorn <- dbh[dbh$species == "h",] 

mean(dbh$growth_mm)

#stats-------

growthmaple <- t.test(maple[maple$site=="c", "growth_mm"],maple[maple$site=="p", "growth_mm"])
growthdog <- t.test(dogwood[dogwood$site=="c", "growth_mm"],dogwood[dogwood$site=="p", "growth_mm"])
growthhawthorn <- t.test(hawthorn[hawthorn$site=="c", "growth_mm"],hawthorn[hawthorn$site=="p", "growth_mm"])

test <- aov(sqrt(growth_mm) ~ species * site, data=dbh)
qqPlot(residuals(test))#pretty good
plot(test)
summary(test)

TukeyHSD(test, which = "site")
TukeyHSD(test, which = "species")
TukeyHSD(test)

#growth - site not different