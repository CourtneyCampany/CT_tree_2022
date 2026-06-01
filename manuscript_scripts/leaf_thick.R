
library(tidyverse)

##Step: Read in and wrangle LMA data-----

leaf_raw <- read_csv("raw_data/leafthick.csv")

leaf_audit <- leaf_raw %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    site = factor(site),
    species = factor(species),
    replicate = factor(replicate),
    week = as.integer(week),
    week_f = factor(week),
    tree_id = factor(paste(site, species, replicate, sep = "_")),
    
    # Primary response for manuscript
    lma_g_m2 = drymass_g / area_cm2 * 10000,
    
    # Reciprocal traits retained for interpretation only
    sla_cm2_g = area_cm2 / drymass_g,
    sla_m2_kg = 1000 / lma_g_m2
  )

glimpse(leaf_audit)

#check for repeated measure completeness
leaf_tree_completeness <- leaf_audit %>%
  group_by(site, species, replicate, tree_id) %>%
  summarise(
    n_records = n(),
    n_lma = sum(!is.na(lma_g_m2)),
    missing_lma_weeks = paste(week[is.na(lma_g_m2)], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(site, species, replicate)

leaf_tree_completeness

leaf_tree_completeness %>%
  count(n_lma)

leaf_tree_completeness %>%
  filter(n_lma < 10)

#Prepare candidate complete-tree dataset
complete_leaf_trees <- leaf_tree_completeness %>%
  filter(n_lma == 10) %>%
  pull(tree_id)

leaf_complete <- leaf_audit %>%
  filter(tree_id %in% complete_leaf_trees)

##Step: Check data distribution (using log transformation)-----

# Add log-transformed LMA to the complete-tree dataset
leaf_complete <- leaf_complete %>%
  mutate(
    log_lma_g_m2 = log(lma_g_m2)
  )

# Reproducible distribution summary
leaf_scale_check <- leaf_complete %>%
  summarise(
    n = sum(!is.na(lma_g_m2)),
    
    mean_lma = mean(lma_g_m2, na.rm = TRUE),
    median_lma = median(lma_g_m2, na.rm = TRUE),
    sd_lma = sd(lma_g_m2, na.rm = TRUE),
    min_lma = min(lma_g_m2, na.rm = TRUE),
    max_lma = max(lma_g_m2, na.rm = TRUE),
    
    mean_log_lma = mean(log_lma_g_m2, na.rm = TRUE),
    median_log_lma = median(log_lma_g_m2, na.rm = TRUE),
    sd_log_lma = sd(log_lma_g_m2, na.rm = TRUE),
    min_log_lma = min(log_lma_g_m2, na.rm = TRUE),
    max_log_lma = max(log_lma_g_m2, na.rm = TRUE)
  )

leaf_scale_check
# 
# qqnorm(leaf_complete$log_lma_g_m2)
# qqline(leaf_complete$log_lma_g_m2)
# hist(
#   leaf_complete$lma_g_m2,
#   main = "Raw LMA",
#   xlab = expression("LMA (g " * m^{-2} * ")")
# )
# 
# hist(
#   leaf_complete$log_lma_g_m2,
#   main = "Log-transformed LMA",
#   xlab = expression("log LMA")
# )

#data are right skewed, histograms, q-q plots improve with log

## Step: Primary repeated measures modelling-----

rm_lma <- aov(
  log_lma_g_m2 ~ site * species * week_f +
    Error(tree_id / week_f),
  data = leaf_complete
)

summary(rm_lma)
#species, week, site x week, species x week

#follow up models
