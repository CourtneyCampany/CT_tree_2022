source("scripts/functions.R")
source("scripts/plot_objects.R")

##Step 1: read and prepare the data

dbh<- read.csv("raw_data/dbh_clean.csv")
  dbh$site <- as.factor(dbh$site)
  dbh$species <- as.factor(dbh$species)
  dbh$date <- as.Date(dbh$date, "%m/%d/%Y")
  dbh$replicate <- factor(dbh$replicate)
  dbh$tree_id  <- paste(dbh$site, dbh$species, dbh$replicate, sep = "-")
  
dbh <- dbh[order(dbh$tree_id , dbh$date), ]  
#make a week variable
dbh$week <- ave(dbh$dbh_mm, dbh$tree_id , FUN = seq_along)
  

##Step 2: calculate change from initial DBH (actual and %)

dbh$initial_dbh <- ave(dbh$dbh_mm, dbh$tree_id, FUN = function(x) x[1])
  dbh$dbh_change_from_initial <- dbh$dbh_mm - dbh$initial_dbh
  dbh$dbh_percent_change <- 100 * (dbh$dbh_mm - dbh$initial_dbh) / dbh$initial_dbh


## PLOTTING

mean_growth <- aggregate(
  dbh_change_from_initial ~ site + species + week,
  data = dbh,
  FUN = mean
)

se_growth <- aggregate(
  dbh_change_from_initial ~ site + species + week,
  data = dbh,
  FUN = function(x) sd(x) / sqrt(length(x))
)

names(mean_growth)[4] <- "mean_change"
names(se_growth)[4] <- "se_change"

growth_summary <- merge(
  mean_growth,
  se_growth,
  by = c("site", "species", "week")
)

growth_summary <- growth_summary[order(growth_summary$site,
                                       growth_summary$species,
                                       growth_summary$week), ]
#plot

growth_summary$group <- with(
  growth_summary,
  paste(site, species, sep = " / ")
)

group_order <- c("c / d", "c / h", "c / m",
                 "p / d", "p / h", "p / m")

growth_summary$group <- factor(
  growth_summary$group,
  levels = group_order
)

windows()
plot(
  NA,
  xlim = range(growth_summary$week),
  ylim = range(
    growth_summary$mean_change - growth_summary$se_change,
    growth_summary$mean_change + growth_summary$se_change
  ),
  xlab = "Measurement week",
  ylab = "DBH change from initial measurement (mm)",
  main = "Stem diameter growth over the 10-week measurement period"
)

abline(h = 0, lty = 2)

cols <- 1:6
pchs <- rep(16, 6)

for(i in seq_along(group_order)) {
  
  temp <- growth_summary[growth_summary$group == group_order[i], ]
  temp <- temp[order(temp$week), ]
  
  lines(
    temp$week,
    temp$mean_change,
    type = "b",
    col = cols[i],
    pch = pchs[i],
    lwd = 2
  )
  
  arrows(
    x0 = temp$week,
    y0 = temp$mean_change - temp$se_change,
    x1 = temp$week,
    y1 = temp$mean_change + temp$se_change,
    angle = 90,
    code = 3,
    length = 0.05,
    col = cols[i]
  )
}

legend(
  "topleft",
  legend = group_order,
  col = cols,
  pch = pchs,
  lty = 1,
  lwd = 2,
  bty = "n",
  title = "Site / species"
)

#plot ggplot

library(ggplot2)

windows()
ggplot(growth_summary,
       aes(x = week,
           y = mean_change,
           color = species,
           shape = species,
           group = species)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean_change - se_change,
                    ymax = mean_change + se_change),
                width = 0.15,
                linewidth = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ site) +
  labs(
    x = "Measurement week",
    y = "DBH change from initial measurement (mm)",
    color = "Species",
    shape = "Species"
  ) +
  theme_classic(base_size = 13) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 13),
    legend.position = "right"
  )

