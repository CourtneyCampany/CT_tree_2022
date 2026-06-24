
library(tidyverse)
library(lubridate)
library(broom)

##Step: Read, wrangle, and check structure of water potential data----

water_raw <- read_csv("raw_data/water_potentials.csv", show_col_types = FALSE)

#standardize variables

water_audit <- water_raw %>%
  mutate(
    date = mdy(date),
    site = factor(site),
    species = factor(species),
    replicate = as.factor(replicate),
    week = as.integer(week),
    week_f = factor(week),
    tree_id = factor(paste(site, species, replicate, sep = "_")),
    
    # Pressure chamber values were recorded as positive balancing pressure in bar.
    # Convert to leaf water potential in MPa.
    # 1 bar = 0.1 MPa; leaf water potential is reported as negative.
    wp_mpa = -0.1 * wp_bar
  )

#data structure checks
#we know there are some missing points (struggling trees, failed measurements)
#continue with complete-tree aov approach, for now

#Create response-scale variables

water_audit <- water_audit %>%
  mutate(
    wp_mag_mpa = abs(wp_mpa),
    log_wp_mag_mpa = log(wp_mag_mpa)
  )

#create complete-tree dataset (all measurments needed for repeated measures)
water_tree_balance <- water_audit %>%
  group_by(site, species, replicate, tree_id) %>%
  summarise(
    n_rows = n(),
    n_weeks = n_distinct(week),
    n_wp_nonmissing = sum(!is.na(wp_mpa)),
    n_wp_missing = sum(is.na(wp_mpa)),
    complete_tree = n_weeks == 10 & n_wp_missing == 0,
    weeks_missing_wp = paste(week[is.na(wp_mpa)], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(site, species, replicate)

water_primary_complete <- water_audit %>%
  semi_join(
    water_tree_balance %>%
      filter(complete_tree) %>%
      select(tree_id),
    by = "tree_id"
  )

water_primary_complete <- water_primary_complete %>%
  mutate(
    wp_mag_mpa = abs(wp_mpa),
    log_wp_mag_mpa = log(wp_mag_mpa)
  )

# Compare full nonmissing dataset and complete-tree dataset

water_dist_check <- bind_rows(
  water_audit %>%
    filter(!is.na(wp_mpa)) %>%
    mutate(dataset = "All nonmissing observations"),
  
  water_primary_complete %>%
    filter(!is.na(wp_mpa)) %>%
    mutate(dataset = "Complete-tree primary dataset")
) %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    min_wp_mpa = min(wp_mpa),
    mean_wp_mpa = mean(wp_mpa),
    median_wp_mpa = median(wp_mpa),
    max_wp_mpa = max(wp_mpa),
    sd_wp_mpa = sd(wp_mpa),
    skew_wp_mpa = mean((wp_mpa - mean(wp_mpa))^3) / sd(wp_mpa)^3,
    min_wp_mag_mpa = min(wp_mag_mpa),
    mean_wp_mag_mpa = mean(wp_mag_mpa),
    median_wp_mag_mpa = median(wp_mag_mpa),
    max_wp_mag_mpa = max(wp_mag_mpa),
    skew_wp_mag_mpa = mean((wp_mag_mpa - mean(wp_mag_mpa))^3) / sd(wp_mag_mpa)^3,
    skew_log_wp_mag_mpa = mean((log_wp_mag_mpa - mean(log_wp_mag_mpa))^3) /
      sd(log_wp_mag_mpa)^3,
    .groups = "drop"
  )

water_dist_check

# ggplot(water_primary_complete, aes(x = wp_mpa)) +
#   geom_histogram(bins = 20, color = "black", fill = "grey80") +
#   labs(
#     x = "Midday leaf water potential (MPa)",
#     y = "Count"
#   ) +
#   theme_classic()
# 
# ggplot(water_primary_complete, aes(sample = wp_mpa)) +
#   stat_qq() +
#   stat_qq_line() +
#   labs(
#     x = "Theoretical quantiles",
#     y = "Observed midday leaf water potential (MPa)"
#   ) +
#   theme_classic()

#no transformation needed

##Step: Run repeated measure primary model-----

rm_water <- aov(
  wp_mpa ~ site * species * week_f + Error(tree_id / week_f),
  data = water_primary_complete
)

summary(rm_water)
#species, site x week, species x week

#stats table for manuscript, write 

water_stats_primary <- tidy(rm_water) %>%
  filter(term != "Residuals") %>%
  mutate(
    trait = "Midday leaf water potential",
    response = "wp_mpa",
    units = "MPa",
    analysis = "Primary site × species × week repeated-measures AOV",
    dataset = "Complete trees",
    model = "wp_mpa ~ site * species * week_f + Error(tree_id/week_f)",
    p_value = p.value,
    p_label = case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p_value, 4))
    )
  ) %>%
  select(
    trait, response, units, analysis, dataset, model,
    stratum, term, df, sumsq, meansq, statistic, p_value, p_label
  )

water_stats_primary

write_csv(water_stats_primary, "calculated_data/water_stats_primary.csv")

##means table

water_means_primary <- water_primary_complete %>%
  group_by(site, species, week) %>%
  summarise(
    n = sum(!is.na(wp_mpa)),
    mean = mean(wp_mpa, na.rm = TRUE),
    sd = sd(wp_mpa, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    trait = "Midday leaf water potential",
    response = "wp_mpa",
    units = "MPa",
    analysis = "Primary site × species × week",
    dataset = "Complete trees"
  ) %>%
  select(
    trait, response, units, analysis, dataset,
    site, species, week, n, mean, sd, se
  )

water_means_primary


#follow ups for interactions terms

#Site effect within each week
#Justified by significant site:week_f interaction
#Model adjusts for species within each week.

water_site_week_tests <- water_primary_complete %>%
  group_by(week_f) %>%
  group_modify(~ {
    mod <- lm(wp_mpa ~ site + species, data = .x)
    
    broom::tidy(mod) %>%
      filter(term == "sitep")
  }) %>%
  ungroup() %>%
  mutate(
    trait = "Midday leaf water potential",
    response = "wp_mpa",
    units = "MPa",
    analysis = "Site effect within week, adjusted for species",
    dataset = "Complete trees",
    week = as.integer(as.character(week_f)),
    contrast = "p - c",
    estimate_park_minus_downtown = estimate,
    p_raw = p.value,
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  ) %>%
  select(
    trait, response, units, analysis, dataset,
    week, contrast,
    estimate_park_minus_downtown,
    std.error, statistic, p_raw, p_holm, significant
  )

water_site_week_tests %>%
  print(n = Inf, width = Inf)
#no week persisted after Holm correction

# Species effect within each week
# Justified by significant species:week_f interaction
# Model adjusts for site within each week.

water_species_week_tests <- water_primary_complete %>%
  group_by(week_f) %>%
  group_modify(~ {
    mod <- aov(wp_mpa ~ site + species, data = .x)
    
    broom::tidy(mod) %>%
      filter(term == "species")
  }) %>%
  ungroup() %>%
  mutate(
    trait = "Midday leaf water potential",
    response = "wp_mpa",
    units = "MPa",
    analysis = "Species effect within week, adjusted for site",
    dataset = "Complete trees",
    week = as.integer(as.character(week_f)),
    p_raw = p.value,
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  ) %>%
  select(
    trait, response, units, analysis, dataset,
    week, term, df, sumsq, meansq, statistic,
    p_raw, p_holm, significant
  )

water_species_week_tests %>%
  print(n = Inf, width = Inf)
#many weeks persisted after Holm

#examine means of species x week to see the differences

water_species_week_means <- water_primary_complete %>%
  group_by(species, week) %>%
  summarise(
    n = sum(!is.na(wp_mpa)),
    mean_wp_mpa = mean(wp_mpa, na.rm = TRUE),
    sd_wp_mpa = sd(wp_mpa, na.rm = TRUE),
    se_wp_mpa = sd_wp_mpa / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(week, species)

water_species_week_means %>%
  print(n = Inf, width = Inf)

#weaker site x week means
water_site_week_means <- water_primary_complete %>%
  group_by(site, week) %>%
  summarise(
    n = sum(!is.na(wp_mpa)),
    mean_wp_mpa = mean(wp_mpa, na.rm = TRUE),
    sd_wp_mpa = sd(wp_mpa, na.rm = TRUE),
    se_wp_mpa = sd_wp_mpa / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(week, site)

water_site_week_means %>%
  print(n = Inf, width = Inf)

##Step: Does pitsize affect water potentials?-----

tree_metadata_raw <- read_csv("calculated_data/tree_metadata_derived.csv")

tree_metadata_clean <- tree_metadata_raw %>%
  mutate(
    site = factor(site, levels = c("c", "p")),
    species = factor(species, levels = c("d", "h", "m")),
    replicate = factor(replicate),
    
    # Metadata tree_id uses hyphens; keep it for checking only
    tree_id_metadata = tree_id,
    
    # Create tree_id style that matches the LMA dataset
    tree_id = factor(paste(site, species, replicate, sep = "_")),
    
    # Main pit-size variable for model
    pit_size = factor(pit_size_class, levels = c("small", "large")),
    
    pit_length_cm = as.numeric(pit_length_cm),
    pit_width_cm = as.numeric(pit_width_cm),
    pit_depth_cm = as.numeric(pit_depth_cm),
    pit_area_m2 = as.numeric(pit_area_m2),
    pit_volume_m3 = as.numeric(pit_volume_m3)
  )


water_pit_audit <- water_audit %>%
  left_join(
    tree_metadata_clean %>%
      select(
        site, species, replicate, tree_id,
        tree_id_metadata,
        pit_size,
        pit_length_cm,
        pit_width_cm,
        pit_depth_cm,
        pit_area_m2,
        pit_volume_m3
      ),
    by = c("site", "species", "replicate", "tree_id")
  ) %>%
  filter(site == "c")

#complete tree check
water_pit_tree_balance <- water_pit_audit %>%
  group_by(species, replicate, tree_id, pit_size) %>%
  summarise(
    n_rows = n(),
    n_weeks = n_distinct(week),
    n_wp_nonmissing = sum(!is.na(wp_mpa)),
    n_wp_missing = sum(is.na(wp_mpa)),
    complete_tree = n_weeks == 10 & n_wp_missing == 0,
    weeks_missing_wp = paste(week[is.na(wp_mpa)], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(species, replicate)

#complete tree data set for repeated measures

water_pit_complete <- water_pit_audit %>%
  semi_join(
    water_pit_tree_balance %>%
      filter(complete_tree) %>%
      select(tree_id),
    by = "tree_id"
  )

water_pit_complete %>%
  distinct(species, replicate, tree_id, pit_size) %>%
  count(species, pit_size, name = "n_complete_trees")

#several incomplete trees aross weeks. Be cautious with model (if interactions)

rm_water_pit <- aov(
  wp_mpa ~ pit_size * species * week_f + Error(tree_id / week_f),
  data = water_pit_complete
)

summary(rm_water_pit) #no pit size effects (STOP)

##Step: Plot water potential data-----

# #labels
# site_labels <- c(
#   c = "Downtown",
#   p = "Park"
# )
# 
# species_labels <- c(
#   d = "Dogwood",
#   h = "Hawthorn",
#   m = "Maple"
# )
# 
# water_species_week_plot <- water_primary_complete %>%
#   group_by(species, week) %>%
#   summarise(
#     n = sum(!is.na(wp_mpa)),
#     mean_wp_mpa = mean(wp_mpa, na.rm = TRUE),
#     sd_wp_mpa = sd(wp_mpa, na.rm = TRUE),
#     se_wp_mpa = sd_wp_mpa / sqrt(n),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     species_label = factor(species_labels[as.character(species)],
#                            levels = species_labels)
#   )
# 
# water_site_week_plot <- water_primary_complete %>%
#   group_by(site, week) %>%
#   summarise(
#     n = sum(!is.na(wp_mpa)),
#     mean_wp_mpa = mean(wp_mpa, na.rm = TRUE),
#     sd_wp_mpa = sd(wp_mpa, na.rm = TRUE),
#     se_wp_mpa = sd_wp_mpa / sqrt(n),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     site_label = factor(site_labels[as.character(site)],
#                         levels = site_labels)
#   )
# 
# #Species × week pattern
# 
# fig_water_species <- ggplot(
#   water_species_week_plot,
#   aes(
#     x = week,
#     y = mean_wp_mpa,
#     group = species_label,
#     linetype = species_label,
#     shape = species_label
#   )
# ) +
#   geom_line(linewidth = 0.7) +
#   geom_point(size = 2.2) +
#   geom_errorbar(
#     aes(
#       ymin = mean_wp_mpa - se_wp_mpa,
#       ymax = mean_wp_mpa + se_wp_mpa
#     ),
#     width = 0.15,
#     linewidth = 0.4
#   ) +
#   scale_x_continuous(breaks = 1:10) +
#   labs(
#     x = "Week",
#     y = expression("Midday leaf water potential (MPa)"),
#     linetype = "Species",
#     shape = "Species"
#   ) +
#   theme_classic() +
#   theme(
#     legend.position = "right"
#   )
# 
# fig_water_species

#caption = “Mean midday leaf water potential across the 10-week study period for each species, 
#pooled across sites. Points represent weekly means ± SE. 
#More negative values indicate greater midday water stress.

ggsave(
  filename = "figures/midday_water_potential_species_week.png",
  plot = fig_water_species,
  width = 7,
  height = 7,
  dpi = 600
)

# Site × week pattern (since didn't survive holm, not going to use)
fig_water_site <- ggplot(
  water_site_week_plot,
  aes(
    x = week,
    y = mean_wp_mpa,
    group = site_label,
    linetype = site_label,
    shape = site_label
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.2) +
  geom_errorbar(
    aes(
      ymin = mean_wp_mpa - se_wp_mpa,
      ymax = mean_wp_mpa + se_wp_mpa
    ),
    width = 0.15,
    linewidth = 0.4
  ) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Week",
    y = expression("Midday leaf water potential (MPa)"),
    linetype = "Site",
    shape = "Site"
  ) +
  theme_classic() +
  theme(
    legend.position = "right"
  )

fig_water_site