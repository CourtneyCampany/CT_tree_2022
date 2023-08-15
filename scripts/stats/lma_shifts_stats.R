#lma shifts

source("scripts/functions.R")

thick <- read.csv("raw_data/lma_shifts.csv")
  thick$site <- as.factor(thick$site)
  thick$species <- as.factor(thick$species)


##stats-----
test <- aov(lma_change ~ species * site, data=thick)
qqPlot(residuals(test))#pretty good
plot(test)
summary(test)

site <- TukeyHSD(test, which = "site")
species <- TukeyHSD(test, which = "species")
interaction <- TukeyHSD(test)

cld(glht(test, linfct=mcp(species = "Tukey")))

species_agg<- doBy::summaryBy(lma_change ~ species , 
                              data =thick, FUN=mean2, keep.names=TRUE)
