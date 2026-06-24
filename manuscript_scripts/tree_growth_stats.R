# source("scripts/functions.R")
# source("scripts/plot_objects.R")

##Step: read and prepare the data sets-----

dbh_weekly<- read.csv("raw_data/dbh_clean.csv")
  dbh_weekly$site <- as.factor(dbh_weekly$site)
  dbh_weekly$species <- as.factor(dbh_weekly$species)
  dbh_weekly$date <- as.Date(dbh_weekly$date, "%m/%d/%Y")
  dbh_weekly$replicate <- factor(dbh_weekly$replicate)
  dbh_weekly$tree_id  <- paste(dbh_weekly$site, dbh_weekly$species, dbh_weekly$replicate, sep = "-")
  
  dbh_weekly <- dbh_weekly[order(dbh_weekly$tree_id , dbh_weekly$date), ]  
    #make a week variable
  dbh_weekly$week <- ave(dbh_weekly$dbh_mm, dbh_weekly$tree_id , FUN = seq_along)
  # Create temporary row identifier to preserve the current weekly-data order
  dbh_weekly$row_id <- seq_len(nrow(dbh_weekly))

#pit sizes data
tree_metadata <- read.csv("calculated_data/tree_metadata_derived.csv")
  tree_metadata$site <- factor(tree_metadata$site)
  tree_metadata$species <- factor(tree_metadata$species)
  tree_metadata$replicate <- factor(tree_metadata$replicate)
  tree_metadata$pit_size_class <- factor(
    tree_metadata$pit_size_class,
    levels = c("small", "large")
  )

#merge tree growth with pit volume
  dbh_weekly_pit <- merge(
  dbh_weekly,
    tree_metadata[, c(
      "tree_id",
      "pit_size_class",
      "pit_area_m2",
      "pit_volume_m3"
    )],
    by = "tree_id",
    all.x = TRUE,
    sort = FALSE
  )

# Restore original chronological/tree order
  dbh_weekly_pit <- dbh_weekly_pit[
  order(dbh_weekly_pit$row_id),]

# Remove temporary row identifier
  dbh_weekly_pit$row_id <- NULL
row.names(dbh_weekly_pit) <- NULL

##Step: create the tree-level growth data set with pit info----

# Calculate each tree's initial DBH
dbh_weekly_pit$initial_dbh <- ave(
  dbh_weekly_pit$dbh_mm,
  dbh_weekly_pit$tree_id,
  FUN = function(x) x[1]
)

dbh_weekly_pit$dbh_change_from_initial <-
  dbh_weekly_pit$dbh_mm - dbh_weekly_pit$initial_dbh

# Create one-row-per-tree growth dataset
tree_growth_pit <- do.call(
  rbind,
  lapply(split(dbh_weekly_pit, dbh_weekly_pit$tree_id), function(x) {
    
    x <- x[order(x$date), ]
    x$days_since_start <- as.numeric(x$date - min(x$date))
    
    fit_day <- lm(dbh_mm ~ days_since_start, data = x)
    fit_week <- lm(dbh_mm ~ week, data = x)
    
    data.frame(
      tree_id = unique(x$tree_id),
      site = unique(x$site),
      species = unique(x$species),
      replicate = unique(x$replicate),
      
      pit_size_class = unique(x$pit_size_class),
      pit_area_m2 = unique(x$pit_area_m2),
      pit_volume_m3 = unique(x$pit_volume_m3),
      
      initial_dbh_mm = x$dbh_mm[1],
      final_dbh_mm = x$dbh_mm[nrow(x)],
      dbh_change_mm = x$dbh_mm[nrow(x)] - x$dbh_mm[1],
      dbh_percent_change = 
        100 * (x$dbh_mm[nrow(x)] - x$dbh_mm[1]) / x$dbh_mm[1],
      dbh_slope_mm_day = coef(fit_day)[2],
      dbh_slope_mm_week = coef(fit_week)[2]
    )
  })
)

row.names(tree_growth_pit) <- NULL


##Step: Downtown-only stem growth-analyze pit size influence-----

city_growth <- subset(tree_growth_pit, site == "c")

# Confirm that this is one row per downtown tree
dim(city_growth)
length(unique(city_growth$tree_id))

with(
  city_growth,
  table(species, pit_size_class)
)

# Confirm estimated available soil volumes associated with each class
unique(
  city_growth[, c("pit_size_class", "pit_volume_m3")]
)

# Descriptive statistics by pit-size class
pit_growth_summary <- do.call(
  rbind,
  lapply(split(city_growth, city_growth$pit_size_class), function(x) {
    data.frame(
      pit_size_class = unique(x$pit_size_class),
      pit_volume_m3 = unique(x$pit_volume_m3),
      n = nrow(x),
      
      dbh_change_mean = mean(x$dbh_change_mm),
      dbh_change_sd = sd(x$dbh_change_mm),
      dbh_change_se = sd(x$dbh_change_mm) / sqrt(nrow(x)),
      
      dbh_slope_mean = mean(x$dbh_slope_mm_week),
      dbh_slope_sd = sd(x$dbh_slope_mm_week),
      dbh_slope_se = sd(x$dbh_slope_mm_week) / sqrt(nrow(x)),
      
      dbh_percent_mean = mean(x$dbh_percent_change),
      dbh_percent_sd = sd(x$dbh_percent_change),
      dbh_percent_se = sd(x$dbh_percent_change) / sqrt(nrow(x))
    )
  })
)

row.names(pit_growth_summary) <- NULL

pit_growth_summary

# Downtown-only pit-size model: primary growth response

#growth slope
city_growth$species <- factor(city_growth$species)
city_growth$pit_size_class <- factor(
  city_growth$pit_size_class,
  levels = c("small", "large")
)

pit_slope_mod <- lm(
  dbh_slope_mm_week ~ species + pit_size_class,
  data = city_growth
)

summary(pit_slope_mod)

# Partial F-tests for each term, accounting for the other term
drop1(pit_slope_mod, test = "F")

# Net DBH change
pit_change_mod <- lm(
  dbh_change_mm ~ species + pit_size_class,
  data = city_growth
)

summary(pit_change_mod)
drop1(pit_change_mod, test = "F")


# Percent DBH change
pit_percent_mod <- lm(
  dbh_percent_change ~ species + pit_size_class,
  data = city_growth
)

summary(pit_percent_mod)
drop1(pit_percent_mod, test = "F")


#After accounting for species, pit-size class did not significantly influence any growth metric among downtown trees
# with trees in large pits growing slightly faster, but not significantly so.(+.035 mm/week, +0.407mm, +0.49%)
#species a stronger predictor (p = .0004)


#diagnostic checks for the models
plot(pit_slope_mod)
plot(pit_change_mod)
plot(pit_percent_mod)

shapiro.test(residuals(pit_slope_mod))
shapiro.test(residuals(pit_change_mod))
shapiro.test(residuals(pit_percent_mod))
#no clear normality issue

fligner.test(residuals(pit_slope_mod) ~ city_growth$pit_size_class)
fligner.test(residuals(pit_change_mod) ~ city_growth$pit_size_class)
fligner.test(residuals(pit_percent_mod) ~ city_growth$pit_size_class)
#There is no evidence that residual variance differs between small- and large-pit downtown trees

#we are ok to pool for tree growth


##Step: Primary pooled stem-growth analysis-----

#final growth models
#growth slope model
pool_slope_mod <- aov(
  dbh_slope_mm_week ~ site * species,
  data = tree_growth_pit
)

pool_change_mod <- aov(
  dbh_change_mm ~ site * species,
  data = tree_growth_pit
)

pool_percent_mod <- aov(
  dbh_percent_change ~ site * species,
  data = tree_growth_pit
)

summary(pool_slope_mod)
summary(pool_change_mod)
summary(pool_percent_mod)

#models based on weekly data
slope_mod <- aov(
  dbh_slope_mm_week ~ site * species,
  data = tree_growth_pit)

plot(slope_mod)

# Normality of residuals
shapiro.test(residuals(slope_mod))

# Homogeneity of residual variance among site x species groups
growth_group <- interaction(
  tree_growth_pit$site,
  tree_growth_pit$species,
  drop = TRUE
)

fligner.test(
  residuals(slope_mod) ~ growth_group
)

# Influence diagnostics
cook_threshold <- 4 / nrow(tree_growth_pit)
cook_threshold

cook_slope <- cooks.distance(slope_mod)

tree_growth_pit[
  cook_slope > cook_threshold,
  c(
    "tree_id",
    "site",
    "species",
    "dbh_slope_mm_week"
  )
]

#rank transformed anova because normality was violated (Shapiro)

tree_growth_pit$rank_slope <- rank(
  tree_growth_pit$dbh_slope_mm_week
)

rank_slope_mod <- aov(
  rank_slope ~ site * species,
  data = tree_growth_pit
)

summary(rank_slope_mod)

TukeyHSD(
  rank_slope_mod,
  which = "species"
)

#untransformed bioloigically summaries for text
species_slope_summary <- do.call(
  rbind,
  lapply(split(tree_growth_pit, tree_growth_pit$species), function(x) {
    data.frame(
      species = unique(x$species),
      n = nrow(x),
      slope_mean = mean(x$dbh_slope_mm_week),
      slope_sd = sd(x$dbh_slope_mm_week),
      slope_se = sd(x$dbh_slope_mm_week) / sqrt(nrow(x)),
      slope_median = median(x$dbh_slope_mm_week),
      slope_min = min(x$dbh_slope_mm_week),
      slope_max = max(x$dbh_slope_mm_week)
    )
  })
)

row.names(species_slope_summary) <- NULL

species_slope_summary

#growth model
change_mod <- aov(
  dbh_change_mm ~ site * species,
  data = tree_growth_pit
)

percent_mod <- aov(
  dbh_percent_change ~ site * species,
  data = tree_growth_pit
)

# Diagnostic plots: net DBH change
plot(change_mod)
plot(percent_mod)

shapiro.test(residuals(change_mod))
shapiro.test(residuals(percent_mod))

growth_group <- interaction(
  tree_growth_pit$site,
  tree_growth_pit$species,
  drop = TRUE
)

fligner.test(residuals(change_mod) ~ growth_group)
fligner.test(residuals(percent_mod) ~ growth_group)


# Influence diagnostics
cook_threshold <- 4 / nrow(tree_growth_pit)

cook_change <- cooks.distance(change_mod)
cook_percent <- cooks.distance(percent_mod)

tree_growth_pit[
  cook_change > cook_threshold,
  c("tree_id", "site", "species", "dbh_change_mm")
]

tree_growth_pit[
  cook_percent > cook_threshold,
  c("tree_id", "site", "species", "dbh_percent_change")
]


#rank transformed anova because normality was violated (Shapiro) for both models
tree_growth_pit$rank_change <- rank(
  tree_growth_pit$dbh_change_mm
)

tree_growth_pit$rank_percent <- rank(
  tree_growth_pit$dbh_percent_change
)

rank_change_mod <- aov(
  rank_change ~ site * species,
  data = tree_growth_pit
)

rank_percent_mod <- aov(
  rank_percent ~ site * species,
  data = tree_growth_pit
)

summary(rank_change_mod)
summary(rank_percent_mod)


TukeyHSD(
  rank_change_mod,
  which = "species"
)

TukeyHSD(
  rank_percent_mod,
  which = "species"
)

#untransformed bioloigically summaries for text
species_growth_summary <- do.call(
  rbind,
  lapply(split(tree_growth_pit, tree_growth_pit$species), function(x) {
    data.frame(
      species = unique(x$species),
      n = nrow(x),
      
      change_mean = mean(x$dbh_change_mm),
      change_sd = sd(x$dbh_change_mm),
      change_se = sd(x$dbh_change_mm) / sqrt(nrow(x)),
      change_median = median(x$dbh_change_mm),
      
      percent_mean = mean(x$dbh_percent_change),
      percent_sd = sd(x$dbh_percent_change),
      percent_se = sd(x$dbh_percent_change) / sqrt(nrow(x)),
      percent_median = median(x$dbh_percent_change)
    )
  })
)

row.names(species_growth_summary) <- NULL

species_growth_summary


# # week_f	Did DBH change accumulate over time?	yes, strongly
# # site:week_f	Did growth trajectory differ between sites?	no
# # species:week_f	Did species differ in growth trajectory?	Yes (after data transformation)
# # site:species:week_f	Did species respond differently through time depending on site? NO

##Step: Repeated-measures analysis of cumulative DBH trajectory

dbh_trajectory <- subset(dbh_weekly_pit, week > 1)

dbh_trajectory$week_f <- factor(dbh_trajectory$week)

rm_growth <- aov(
  dbh_change_from_initial ~ site * species * week_f +
    Error(tree_id / week_f),
  data = dbh_trajectory
)

summary(rm_growth)
#yes, trees grew weekly but not site or species differences. 
#Week	F₈,₁₉₂ = 17.89 p	< 0.001	cumulative DBH growth increased through time