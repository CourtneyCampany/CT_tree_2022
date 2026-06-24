library(tidyverse)
library(lubridate)

##Step: Read, wrangle, and check structure of chlorophyll from DMF extracts----

chl_raw <- read_csv("raw_data/chlorophyll_extract.csv", show_col_types = FALSE)

chl_clean <- chl_raw %>%
  filter(if_any(everything(), ~ !is.na(.x))) %>%
  mutate(
    site = as.character(site),
    species = as.character(species),
    date = mdy(date),
    replicate = as.integer(replicate),
    week = as.integer(week),
    week_f = factor(week),
    tree_id = paste(site, species, replicate, sep = "_")
  )

#calculate total chlorophyll from absorbance

extract_vol_L <- 0.003       # 3 mL DMF

chl_extract <- chl_clean %>%
  mutate(
    # Convert punch diameter from mm to cm, then calculate area of one punch
    punch_diameter_cm = punch_diameter_mm / 10,
    punch_radius_cm = punch_diameter_cm / 2,
    punch_area_cm2 = pi * punch_radius_cm^2,
    total_punch_area_cm2 = punch_num * punch_area_cm2,
    
    # Inskeep & Bloom DMF equations; units = mg chlorophyll L-1 extract
    chla_mg_L_calc = 12.70 * `abs_664.5` - 2.79 * abs_647,
    chlb_mg_L_calc = 20.70 * abs_647 - 4.62 * `abs_664.5`,
    total_chl_mg_L_calc = 17.90 * abs_647 + 8.08 * `abs_664.5`,
    
    # Convert concentration in extract to total chlorophyll amount in sample
    chla_ug_sample = chla_mg_L_calc * extract_vol_L * 1000,
    chlb_ug_sample = chlb_mg_L_calc * extract_vol_L * 1000,
    total_chl_ug_sample = total_chl_mg_L_calc * extract_vol_L * 1000,
    
    # Area-normalized chlorophyll
    chla_ug_cm2 = chla_ug_sample / total_punch_area_cm2,
    chlb_ug_cm2 = chlb_ug_sample / total_punch_area_cm2,
    total_chl_ug_cm2 = total_chl_ug_sample / total_punch_area_cm2,
    
    # Fresh-mass-normalized chlorophyll
    chla_mg_g_fw = (chla_mg_L_calc * extract_vol_L) / (total_mass_mg / 1000),
    chlb_mg_g_fw = (chlb_mg_L_calc * extract_vol_L) / (total_mass_mg / 1000),
    total_chl_mg_g_fw = (total_chl_mg_L_calc * extract_vol_L) / (total_mass_mg / 1000),
    
    # Chlorophyll a:b ratio
    chl_ab_ratio = chla_mg_L_calc / chlb_mg_L_calc
  )

#Check whether the file's final columns match the corrected equations

chl_extract %>%
  summarise(
    max_abs_diff_file_chla_vs_calc =
      max(abs(chla - chla_mg_L_calc), na.rm = TRUE),
    
    max_abs_diff_file_chlb_vs_correct_chlb =
      max(abs(chlb - chlb_mg_L_calc), na.rm = TRUE),
    
    max_abs_diff_file_chlb_vs_apparent_file_formula =
      max(abs(chlb - (20.70 * abs_647 + 8.08 * `abs_664.5`)), na.rm = TRUE)
  )

# Summary of corrected variables
chl_extract %>%
  summarise(
    n_complete = sum(!is.na(total_chl_ug_cm2)),
    mean_total_chl_ug_cm2 = mean(total_chl_ug_cm2, na.rm = TRUE),
    sd_total_chl_ug_cm2 = sd(total_chl_ug_cm2, na.rm = TRUE),
    min_total_chl_ug_cm2 = min(total_chl_ug_cm2, na.rm = TRUE),
    max_total_chl_ug_cm2 = max(total_chl_ug_cm2, na.rm = TRUE),
    mean_chl_ab_ratio = mean(chl_ab_ratio, na.rm = TRUE),
    sd_chl_ab_ratio = sd(chl_ab_ratio, na.rm = TRUE),
    min_chl_ab_ratio = min(chl_ab_ratio, na.rm = TRUE),
    max_chl_ab_ratio = max(chl_ab_ratio, na.rm = TRUE)
  )

chl_extract_complete <- chl_extract %>%
  filter(!is.na(total_chl_ug_cm2))

# Distribution checks before modeling
skewness_base <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  sum((x - m)^3) / ((n - 1) * s^3)
}

chl_distribution_check <- chl_extract %>%
  mutate(
    log_total_chl_ug_cm2 = log(total_chl_ug_cm2),
    sqrt_total_chl_ug_cm2 = sqrt(total_chl_ug_cm2),
    log_chla_ug_cm2 = log(chla_ug_cm2),
    log_chlb_ug_cm2 = log(chlb_ug_cm2),
    log_chl_ab_ratio = log(chl_ab_ratio)
  ) %>%
  summarise(
    n_total = sum(!is.na(total_chl_ug_cm2)),
    
    mean_total_chl = mean(total_chl_ug_cm2, na.rm = TRUE),
    median_total_chl = median(total_chl_ug_cm2, na.rm = TRUE),
    sd_total_chl = sd(total_chl_ug_cm2, na.rm = TRUE),
    min_total_chl = min(total_chl_ug_cm2, na.rm = TRUE),
    max_total_chl = max(total_chl_ug_cm2, na.rm = TRUE),
    
    skew_total_chl = skewness_base(total_chl_ug_cm2),
    skew_log_total_chl = skewness_base(log_total_chl_ug_cm2),
    skew_sqrt_total_chl = skewness_base(sqrt_total_chl_ug_cm2),
    
    skew_chla = skewness_base(chla_ug_cm2),
    skew_chlb = skewness_base(chlb_ug_cm2),
    skew_chl_ab_ratio = skewness_base(chl_ab_ratio),
    skew_log_chl_ab_ratio = skewness_base(log_chl_ab_ratio)
  )

chl_distribution_check

ggplot(chl_extract, aes(x = total_chl_ug_cm2)) +
  geom_histogram(bins = 30) +
  labs(
    x = expression("Total chlorophyll ("*mu*"g cm"^-2*")"),
    y = "Count"
  ) +
  theme_classic()

ggplot(chl_extract, aes(x = log(total_chl_ug_cm2))) +
  geom_histogram(bins = 30) +
  labs(
    x = expression("log total chlorophyll ("*mu*"g cm"^-2*")"),
    y = "Count"
  ) +
  theme_classic()
#log transformation best helps correct right skewness

#add logged variables
chl_extract_complete <- chl_extract %>%
  filter(!is.na(total_chl_ug_cm2)) %>%
  mutate(
    log_total_chl_ug_cm2 = log(total_chl_ug_cm2),
    sqrt_total_chl_ug_cm2 = sqrt(total_chl_ug_cm2),
    log_chla_ug_cm2 = log(chla_ug_cm2),
    log_chlb_ug_cm2 = log(chlb_ug_cm2),
    log_chl_ab_ratio = log(chl_ab_ratio)
  )


##Step: Run repeated measures model on chlorophyll variables-----

#first create completeness dataset
chl_tree_balance <- chl_extract_complete %>%
  group_by(tree_id, site, species, replicate) %>%
  summarise(
    n_weeks_observed = n_distinct(week),
    weeks_observed = paste(sort(unique(week)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(n_weeks_observed, site, species, replicate)

complete_chl_trees <- chl_tree_balance %>%
  filter(n_weeks_observed == 10) %>%
  pull(tree_id)

chl_extract_aov <- chl_extract_complete %>%
  filter(tree_id %in% complete_chl_trees) %>%
  mutate(
    site = factor(site),
    species = factor(species),
    week_f = factor(week),
    tree_id = factor(tree_id)
  )

#run model
rm_chl_total <- aov(
  log_total_chl_ug_cm2 ~ site * species * week_f +
    Error(tree_id / week_f),
  data = chl_extract_aov
)

summary(rm_chl_total)
#species, site_species, species x week


#target followups for interactions using Holm

#species x week
species_week_tests <- chl_extract_aov %>%
  group_by(week) %>%
  group_modify(~ {
    
    pairwise <- pairwise.t.test(
      x = .x$log_total_chl_ug_cm2,
      g = .x$species,
      p.adjust.method = "holm",
      pool.sd = FALSE
    )
    
    as.data.frame(as.table(pairwise$p.value)) %>%
      filter(!is.na(Freq)) %>%
      rename(
        species_1 = Var1,
        species_2 = Var2,
        p_holm = Freq
      )
  }) %>%
  ungroup() %>%
  arrange(week, p_holm)

species_week_tests %>%
  print(n = Inf)

#species x week means
chl_species_week_means <- chl_extract_aov %>%
  group_by(species, week) %>%
  summarise(
    n = n(),
    mean_total_chl_ug_cm2 = mean(total_chl_ug_cm2),
    sd_total_chl_ug_cm2 = sd(total_chl_ug_cm2),
    se_total_chl_ug_cm2 = sd_total_chl_ug_cm2 / sqrt(n),
    mean_log_total_chl = mean(log_total_chl_ug_cm2),
    backtransformed_mean_total_chl_ug_cm2 = exp(mean_log_total_chl),
    .groups = "drop"
  )

chl_species_week_means %>%
  print(n = Inf)

# site x species followup
chl_tree_means <- chl_extract_aov %>%
  group_by(tree_id, site, species, replicate) %>%
  summarise(
    mean_log_total_chl = mean(log_total_chl_ug_cm2),
    mean_total_chl_ug_cm2 = mean(total_chl_ug_cm2),
    backtransformed_tree_mean_total_chl_ug_cm2 =
      exp(mean_log_total_chl),
    .groups = "drop"
  )

site_species_tests <- chl_tree_means %>%
  group_by(species) %>%
  summarise(
    n_city = sum(site == "c"),
    n_park = sum(site == "p"),
    mean_city = mean(backtransformed_tree_mean_total_chl_ug_cm2[site == "c"]),
    mean_park = mean(backtransformed_tree_mean_total_chl_ug_cm2[site == "p"]),
    estimate_city_minus_park =
      mean(mean_log_total_chl[site == "c"]) -
      mean(mean_log_total_chl[site == "p"]),
    p_raw = t.test(mean_log_total_chl ~ site)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  )

site_species_tests %>%
  print(n = Inf)

#species means

chl_means_species <- chl_extract_aov %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mean_total_chl_ug_cm2 = mean(total_chl_ug_cm2),
    sd_total_chl_ug_cm2 = sd(total_chl_ug_cm2),
    se_total_chl_ug_cm2 = sd_total_chl_ug_cm2 / sqrt(n),
    backtransformed_mean_total_chl_ug_cm2 =
      exp(mean(log_total_chl_ug_cm2)),
    .groups = "drop"
  )

chl_means_species %>%
  print(n = Inf)

##Step: Did chlorophyll change by pit size------

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

# Join planting pit metadata to chlorophyll dataset
chl_extract <- chl_extract %>%
  mutate(
    site = factor(site, levels = c("c", "p")),
    species = factor(species, levels = c("d", "h", "m")),
    replicate = factor(replicate),
    tree_id = factor(paste(site, species, replicate, sep = "_"))
  ) %>%
  left_join(
    tree_metadata_clean %>%
      select(
        site, species, replicate, tree_id,
        pit_size,
        pit_length_cm, pit_width_cm, pit_depth_cm,
        pit_area_m2, pit_volume_m3
      ),
    by = c("site", "species", "replicate", "tree_id")
  )

# Downtown-only complete repeated-measures dataset
chl_downtown_complete <- chl_extract %>%
  filter(
    site == "c",
    !is.na(total_chl_ug_cm2),
    !is.na(pit_size)
  ) %>%
  mutate(
    log_total_chl_ug_cm2 = log(total_chl_ug_cm2),
    pit_size = factor(pit_size, levels = c("small", "large")),
    species = factor(species, levels = c("d", "h", "m")),
    week_f = factor(week),
    tree_id = factor(tree_id)
  )

chl_downtown_tree_balance <- chl_downtown_complete %>%
  group_by(tree_id, species, replicate, pit_size) %>%
  summarise(
    n_weeks_observed = n_distinct(week),
    weeks_observed = paste(sort(unique(week)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(n_weeks_observed, species, replicate)

complete_downtown_chl_trees <- chl_downtown_tree_balance %>%
  filter(n_weeks_observed == 10) %>%
  pull(tree_id)

chl_downtown_aov <- chl_downtown_complete %>%
  filter(tree_id %in% complete_downtown_chl_trees) %>%
  droplevels()

#run model
rm_chl_downtown <- aov(
  log_total_chl_ug_cm2 ~ pit_size * species * week_f +
    Error(tree_id / week_f),
  data = chl_downtown_aov
)

summary(rm_chl_downtown)

#followup pit x week

pit_week_tests <- chl_downtown_aov %>%
  group_by(week) %>%
  summarise(
    n_small = sum(pit_size == "small"),
    n_large = sum(pit_size == "large"),
    mean_small = mean(total_chl_ug_cm2[pit_size == "small"]),
    mean_large = mean(total_chl_ug_cm2[pit_size == "large"]),
    se_small = sd(total_chl_ug_cm2[pit_size == "small"]) / sqrt(n_small),
    se_large = sd(total_chl_ug_cm2[pit_size == "large"]) / sqrt(n_large),
    estimate_large_minus_small =
      mean(log_total_chl_ug_cm2[pit_size == "large"]) -
      mean(log_total_chl_ug_cm2[pit_size == "small"]),
    p_raw = t.test(log_total_chl_ug_cm2 ~ pit_size)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    p_adj = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_adj < 0.05, "Yes", "No")
  )

pit_week_tests %>%
  print(n = Inf, width = Inf)
##although interaction with pit existed, it did not survive Holm

##Step: Plotting total chlorophyll-----

# Labels for plotting
species_labels <- c(
  "d" = "Dogwood",
  "h" = "Hawthorn",
  "m" = "Maple"
)

site_labels <- c(
  "c" = "Downtown",
  "p" = "Park"
)

pit_labels <- c(
  "small" = "Small pit",
  "large" = "Large pit"
)

#figure data
chl_plot_species_week <- chl_extract_aov %>%
  group_by(species, week) %>%
  summarise(
    n = n(),
    mean_total_chl_ug_cm2 = mean(total_chl_ug_cm2),
    sd_total_chl_ug_cm2 = sd(total_chl_ug_cm2),
    se_total_chl_ug_cm2 = sd_total_chl_ug_cm2 / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    species_label = factor(
      species_labels[as.character(species)],
      levels = species_labels
    )
  )

#plot
fig_chl_species_week <- ggplot(
  chl_plot_species_week,
  aes(
    x = week,
    y = mean_total_chl_ug_cm2,
    group = species_label,
    shape = species_label,
    linetype = species_label
  )
) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      ymin = mean_total_chl_ug_cm2 - se_total_chl_ug_cm2,
      ymax = mean_total_chl_ug_cm2 + se_total_chl_ug_cm2
    ),
    width = 0.15,
    linewidth = 0.4
  ) +
  scale_x_continuous(
    breaks = sort(unique(chl_plot_species_week$week))
  ) +
  labs(
    x = "Week",
    y = expression("Total chlorophyll ("*mu*"g cm"^-2*")"),
    shape = "Species",
    linetype = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

fig_chl_species_week

ggsave(
  filename = "figures/fig_chl_species_week.png",
  plot = fig_chl_species_week,
  width = 6.5,
  height = 4.5,
  units = "in",
  dpi = 600
)
