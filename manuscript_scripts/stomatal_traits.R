library(tidyverse)

##Step: read, wrangle and examine structure of stomatal density-----

stomata_raw <- read.csv("raw_data/stomata_density.csv")

stomata_density_raw <- stomata_raw %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    week_f = factor(week),
    tree_id = paste(site, species, replicate, sep = "_"),
    fov_area_mm2 = pi * (fov_diam_mm / 2)^2,
    stomatal_density_mm2 = stomata_count / fov_area_mm2
  )

#means tree per week

stomata_density_leaf <- stomata_density_raw %>%
  group_by(date, week, week_f, site, species, replicate, tree_id) %>%
  summarise(
    n_count_rows = n(),
    n_nonmissing_counts = sum(!is.na(stomata_count)),
    mean_stomata_count = mean(stomata_count, na.rm = TRUE),
    sd_stomata_count = sd(stomata_count, na.rm = TRUE),
    mean_stomatal_density_mm2 = mean(stomatal_density_mm2, na.rm = TRUE),
    sd_stomatal_density_mm2 = sd(stomatal_density_mm2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    leaf_complete = n_count_rows == 3 & n_nonmissing_counts == 3
  )

#complete tree dataset

expected_weeks <- 2:10

stomata_tree_audit <- stomata_density_leaf %>%
  group_by(site, species, replicate, tree_id) %>%
  summarise(
    n_complete_weeks = sum(leaf_complete),
    missing_weeks = paste(
      setdiff(expected_weeks, sort(unique(week[leaf_complete]))),
      collapse = ","
    ),
    complete_weeks_2_10 = all(expected_weeks %in% week[leaf_complete]),
    .groups = "drop"
  )


stomata_complete_ids <- stomata_tree_audit %>%
  filter(complete_weeks_2_10) %>%
  pull(tree_id)

stomata_density_primary <- stomata_density_leaf %>%
  filter(
    tree_id %in% stomata_complete_ids,
    week %in% expected_weeks,
    leaf_complete
  ) %>%
  mutate(
    log_stomatal_density_mm2 = log(mean_stomatal_density_mm2)
  )

#check data distribution
density_scale_summary <- stomata_density_primary %>%
  summarise(
    n = n(),
    min_density = min(mean_stomatal_density_mm2),
    mean_density = mean(mean_stomatal_density_mm2),
    median_density = median(mean_stomatal_density_mm2),
    max_density = max(mean_stomatal_density_mm2),
    sd_density = sd(mean_stomatal_density_mm2),
    cv_density = sd_density / mean_density,
    skew_raw = mean(
      ((mean_stomatal_density_mm2 - mean_density) / sd_density)^3
    ),
    min_log_density = min(log_stomatal_density_mm2),
    mean_log_density = mean(log_stomatal_density_mm2),
    median_log_density = median(log_stomatal_density_mm2),
    max_log_density = max(log_stomatal_density_mm2),
    sd_log_density = sd(log_stomatal_density_mm2),
    skew_log = mean(
      ((log_stomatal_density_mm2 - mean_log_density) / sd_log_density)^3
    )
  )

density_scale_summary

density_by_group <- stomata_density_primary %>%
  group_by(site, species) %>%
  summarise(
    n = n(),
    mean_density = mean(mean_stomatal_density_mm2),
    sd_density = sd(mean_stomatal_density_mm2),
    cv_density = sd_density / mean_density,
    min_density = min(mean_stomatal_density_mm2),
    median_density = median(mean_stomatal_density_mm2),
    max_density = max(mean_stomatal_density_mm2),
    .groups = "drop"
  )

density_by_group

ggplot(stomata_density_primary,
       aes(x = mean_stomatal_density_mm2)) +
  geom_histogram(bins = 20) +
  labs(
    x = expression("Stomatal density (stomata " * mm^{-2} * ")"),
    y = "Frequency"
  ) +
  theme_classic()

ggplot(stomata_density_primary,
       aes(x = log_stomatal_density_mm2)) +
  geom_histogram(bins = 20) +
  labs(
    x = "log stomatal density",
    y = "Frequency"
  ) +
  theme_classic()
#log not better

ggplot(stomata_density_primary,
       aes(sample = mean_stomatal_density_mm2)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    x = "Theoretical quantiles",
    y = expression("Stomatal density (stomata " * mm^{-2} * ")")
  ) +
  theme_classic()

ggplot(stomata_density_primary,
       aes(sample = log_stomatal_density_mm2)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    x = "Theoretical quantiles",
    y = "log stomatal density"
  ) +
  theme_classic()
#log not better

##Step: run the repeated measures model-----

# Response: mean_stomatal_density_mm2
# Scale decision: raw scale

stomata_density_primary <- stomata_density_primary %>%
  mutate(
    site = factor(site),
    species = factor(species),
    week_f = factor(week_f),
    tree_id = factor(tree_id)
  )

rm_density <- aov(
  mean_stomatal_density_mm2 ~ site * species * week_f +
    Error(tree_id / week_f),
  data = stomata_density_primary
)

summary(rm_density)
#species, week, species x week

#stats table to merge with other traits in manuscript
density_summary <- summary(rm_density)

density_stats_between <- as.data.frame(
  density_summary[["Error: tree_id"]][[1]]
) %>%
  tibble::rownames_to_column("model_term") %>%
  mutate(
    error_stratum = "tree_id",
    effect_type = "Between-tree"
  )

density_stats_within <- as.data.frame(
  density_summary[["Error: tree_id:week_f"]][[1]]
) %>%
  tibble::rownames_to_column("model_term") %>%
  mutate(
    error_stratum = "tree_id:week_f",
    effect_type = "Repeated-measures"
  )

density_stats_primary <- bind_rows(
  density_stats_between,
  density_stats_within
) %>%
  rename(
    df = Df,
    sum_sq = `Sum Sq`,
    mean_sq = `Mean Sq`,
    f_value = `F value`,
    p_value = `Pr(>F)`
  ) %>%
  mutate(
    trait = "Stomatal density",
    response = "mean_stomatal_density_mm2",
    units = "stomata mm^-2",
    analysis = "Primary site x species x week repeated-measures AOV",
    scale = "raw",
    p_value_text = case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.001 ~ "<0.001",
      TRUE ~ as.character(signif(p_value, 3))
    ),
    significant = case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.05 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  select(
    trait, response, units, analysis, scale,
    effect_type, error_stratum, model_term,
    df, sum_sq, mean_sq, f_value,
    p_value, p_value_text, significant
  )

density_stats_primary <- density_stats_primary %>%
  tibble::as_tibble()

density_stats_primary %>%
  print(n = Inf)

# Full table, including residual rows

density_stats_terms <- density_stats_primary %>%
  filter(model_term != "Residuals")

write.csv(
  density_stats_terms,
  "stomatal_density_primary_stats.csv",
  row.names = FALSE
)

#model diagnostics

density_lm_check <- lm(
  mean_stomatal_density_mm2 ~ site * species * week_f,
  data = stomata_density_primary
)

par(mfrow = c(1, 2))

plot(
  fitted(density_lm_check),
  resid(density_lm_check),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs fitted"
)
abline(h = 0, lty = 2)

qqnorm(resid(density_lm_check))
qqline(resid(density_lm_check))

#followups with species x site interaction

density_species_week_contrasts <- stomata_density_primary %>%
  group_by(week, week_f) %>%
  group_modify(~ {
    
    species_pairs <- combn(sort(unique(.x$species)), 2, simplify = FALSE)
    
    bind_rows(lapply(species_pairs, function(pair) {
      
      dat_pair <- .x %>%
        filter(species %in% pair)
      
      test_out <- t.test(
        mean_stomatal_density_mm2 ~ species,
        data = dat_pair
      )
      
      tibble(
        contrast = paste(pair[2], "minus", pair[1]),
        estimate = unname(diff(test_out$estimate)),
        mean_1 = unname(test_out$estimate[1]),
        mean_2 = unname(test_out$estimate[2]),
        statistic = unname(test_out$statistic),
        df = unname(test_out$parameter),
        p_raw = test_out$p.value
      )
    }))
  }) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = ifelse(p_holm < 0.05, "Yes", "No")
  )

density_species_week_contrasts %>%
  arrange(week, p_holm) %>%
  print(n = Inf, width = Inf)

#maple always higher, other two species similar across weeks after Holm


#means table
density_species_week_means <- stomata_density_primary %>%
  group_by(species, week, week_f) %>%
  summarise(
    n = n(),
    mean_density = mean(mean_stomatal_density_mm2),
    sd_density = sd(mean_stomatal_density_mm2),
    se_density = sd_density / sqrt(n),
    .groups = "drop"
  )

# density_species_week_means %>%
#   arrange(week, species) %>%
#   print(n = Inf, width = Inf)


##Step: Does pit size affect stomatal density in downtown trees?

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

#joing to SD
stomata_density_pit <- stomata_density_primary %>%
mutate(
  site = factor(site, levels = c("c", "p")),
  species = factor(species, levels = c("d", "h", "m")),
  replicate = factor(replicate),
  tree_id = factor(tree_id)
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
  ) %>%
  filter(site == "c") %>%
  mutate(
    pit_size = factor(pit_size, levels = c("small", "large")),
    week_f = factor(week_f),
    tree_id = factor(tree_id)
  )

#model
rm_density_pit <- aov(
  mean_stomatal_density_mm2 ~ pit_size * species * week_f +
    Error(tree_id / week_f),
  data = stomata_density_pit
)

summary(rm_density_pit)

#NO pit size has no affect on SD


##Step: Plot stomatal density -------

species_labels <- c(
  d = "Dogwood",
  h = "Hawthorn",
  m = "Maple"
)


density_species_week_plot_data <- stomata_density_primary %>%
  mutate(
    species_label = factor(
      species_labels[as.character(species)],
      levels = species_labels
    )
  ) %>%
  group_by(species, species_label, week) %>%
  summarise(
    n = n(),
    mean_density = mean(mean_stomatal_density_mm2),
    sd_density = sd(mean_stomatal_density_mm2),
    se_density = sd_density / sqrt(n),
    .groups = "drop"
  )

fig_stomatal_density <- ggplot(
  density_species_week_plot_data,
  aes(
    x = week,
    y = mean_density,
    group = species_label,
    shape = species_label,
    linetype = species_label
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.4) +
  geom_errorbar(
    aes(
      ymin = mean_density - se_density,
      ymax = mean_density + se_density
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_x_continuous(
    breaks = 2:10,
    limits = c(2, 10)
  ) +
  labs(
    x = "Manuscript week",
    y = expression("Stomatal density (stomata " * mm^{-2} * ")"),
    shape = NULL,
    linetype = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11)
  )

fig_stomatal_density


ggsave(
  filename = "figures/fig_stomatal_density_species_week.pdf",
  plot = fig_stomatal_density,
  width = 5.5,
  height = 4.0,
  units = "in"
)
