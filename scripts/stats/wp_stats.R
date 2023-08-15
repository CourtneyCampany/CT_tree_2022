source("scripts/functions.R")
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(MASS)

waterpot <- read.csv("raw_data/water_potentials.csv")
waterpot$site <- as.factor(waterpot$site)
waterpot$species <- as.factor(waterpot$species)
waterpot$wp_mp <- with(waterpot, ((wp_bar/10)*-1))
waterpot$Date <- as.Date(waterpot$date, "%m/%d/%Y")
waterpot$uniqueID <- paste(waterpot$species, waterpot$site, sep = "-")


##stats-----
test <- aov(wp_mp ~ species * site, data=waterpot)
car::qqPlot(residuals(test))#pretty good
plot(test)
summary(test)
# 
# site <- TukeyHSD(test, which = "site")
species <- TukeyHSD(test, which = "species")
# interaction <- TukeyHSD(test)

cld(glht(test, linfct=mcp(species = "Tukey")))

doBy::summaryBy(wp_mp ~ species,  data =waterpot, FUN=mean2, keep.names=TRUE)
