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


## Step 3: Tree level growth summary

tree_growth <- do.call(rbind, lapply(split(dbh, dbh$tree_id), function(x) {
  
  x <- x[order(x$date), ]
  x$days_since_start <- as.numeric(x$date - min(x$date))
  
  fit_day <- lm(dbh_mm ~ days_since_start, data = x)
  fit_week <- lm(dbh_mm ~ week, data = x)
  
  data.frame(
    tree_id = unique(x$tree_id),
    site = unique(x$site),
    species = unique(x$species),
    replicate = unique(x$replicate),
    initial_dbh_mm = x$dbh_mm[1],
    final_dbh_mm = x$dbh_mm[nrow(x)],
    dbh_change_mm = x$dbh_mm[nrow(x)] - x$dbh_mm[1],
    dbh_percent_change = 100 * (x$dbh_mm[nrow(x)] - x$dbh_mm[1]) / x$dbh_mm[1],
    dbh_slope_mm_day = coef(fit_day)[2],
    dbh_slope_mm_week = coef(fit_week)[2]
  )
}))

##Question: Did trees grow more than zero?

t.test(tree_growth$dbh_change_mm, mu = 0)
t.test(tree_growth$dbh_slope_mm_week, mu = 0)
t.test(tree_growth$dbh_percent_change, mu = 0)

#Across all trees, stem diameter increased significantly during the 10-week period. 
#Mean net DBH increase was 2.45 mm, equivalent to about 6.1% relative growth, 
#and the mean growth slope was 0.234 mm per week.

#Stem growth was first evaluated across all trees by reducing weekly DBH measurements 
#to individual-tree growth metrics. For each tree, we calculated net DBH change, 
#percent DBH change, and the slope of DBH over measurement week. 
#One-sample t-tests were used to determine whether mean growth metrics differed 
#significantly from zero, indicating measurable stem diameter growth during the study period.


## Question: site × species analysis using the tree-level growth metrics.
slope_mod <- aov(dbh_slope_mm_week ~ site * species, data = tree_growth)
summary(slope_mod)
#site, species, site:species = not significant for growth slope
  shapiro.test(residuals(slope_mod))
  fligner.test(dbh_slope_mm_week ~ interaction(site, species),
  data = tree_growth)
  tree_growth$rank_slope <- rank(tree_growth$dbh_slope_mm_week)
  rank_slope_mod <- aov(rank_slope ~ site * species, data = tree_growth)
  summary(rank_slope_mod)

change_mod <- aov(dbh_change_mm ~ site * species, data = tree_growth)
summary(change_mod)
#site, species, site:species = not significant for growth slope
  shapiro.test(residuals(change_mod))
  fligner.test(dbh_change_mm ~ interaction(site, species),
             data = tree_growth)
  tree_growth$rank_change <- rank(tree_growth$dbh_change_mm)
  rank_change_mod <- aov(rank_change ~ site * species, data = tree_growth)
  summary(rank_change_mod)

percent_mod <- aov(dbh_percent_change ~ site * species, data = tree_growth)
summary(percent_mod)
#site, species, site:species = not significant for growth slope
  shapiro.test(residuals(percent_mod))
  fligner.test(dbh_percent_change ~ interaction(site, species),
               data = tree_growth)
  tree_growth$rank_percent <- rank(tree_growth$dbh_percent_change)
  rank_percent_mod <- aov(rank_percent ~ site * species, data = tree_growth)
  summary(rank_percent_mod)

  TukeyHSD(slope_mod, which = "species")
  TukeyHSD(change_mod, which = "species")
  TukeyHSD(percent_mod, which = "species")
  TukeyHSD(rank_slope_mod, which = "species")
  TukeyHSD(rank_change_mod, which = "species")
  TukeyHSD(rank_percent_mod, which = "species")

#Question: descriptive stats
growth_summary_stats <- do.call(rbind, lapply(split(tree_growth,
                                                    list(tree_growth$site,
                                                         tree_growth$species),
                                                    drop = TRUE), function(x) {
                                                      data.frame(
                                                        site = unique(x$site),
                                                        species = unique(x$species),
                                                        n = nrow(x),
                                                        
                                                        change_mean = mean(x$dbh_change_mm),
                                                        change_sd = sd(x$dbh_change_mm),
                                                        change_se = sd(x$dbh_change_mm) / sqrt(nrow(x)),
                                                        
                                                        slope_mean = mean(x$dbh_slope_mm_week),
                                                        slope_sd = sd(x$dbh_slope_mm_week),
                                                        slope_se = sd(x$dbh_slope_mm_week) / sqrt(nrow(x)),
                                                        
                                                        percent_mean = mean(x$dbh_percent_change),
                                                        percent_sd = sd(x$dbh_percent_change),
                                                        percent_se = sd(x$dbh_percent_change) / sqrt(nrow(x))
                                                      )
                                                    }))

row.names(growth_summary_stats) <- NULL

growth_summary_stats

#Question: did cumulative DBH change through time differently by site or species? repeated measures

dbh_growth <- subset(dbh, week > 1)

dbh_growth$week_f <- factor(dbh_growth$week)

rm_growth <- aov(
  dbh_change_from_initial ~ site * species * week_f +
    Error(tree_id / week_f),
  data = dbh_growth
)

summary(rm_growth)

# week_f	Did DBH change accumulate over time?	yes, strongly
# site:week_f	Did growth trajectory differ between sites?	no
# species:week_f	Did species differ in growth trajectory?	NO
# site:species:week_f	Did species respond differently through time depending on site? NO


