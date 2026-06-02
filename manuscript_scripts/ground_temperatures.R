library(tidyverse)
library(lubridate)

#Step: read, wrangle, and examine structure of ground temperature-----

ground_raw <- read_csv("raw_data/ground_temperature.csv", show_col_types = FALSE)

ground_audit <- ground_raw %>%
  mutate(
    date = mdy(date),
    site = factor(site),
    species = factor(species),
    replicate = as.integer(replicate),
    temp_replicate = as.integer(temp_replicate),
    week = as.integer(week),
    week_f = factor(week),
    tree_id = paste(site, species, replicate, sep = "_"),
    temperature_C = (temperature_F - 32) * 5 / 9
  )

#means by week
ground_primary <- ground_audit %>%
  group_by(site, species, replicate, tree_id, week, week_f) %>%
  summarize(
    date = first(date),
    n_dates = n_distinct(date),
    n_temp_replicates = n(),
    n_temperature_values = sum(!is.na(temperature_C)),
    n_temperature_missing = sum(is.na(temperature_C)),
    ground_temp_C = mean(temperature_C, na.rm = TRUE),
    ground_temp_sd_C = sd(temperature_C, na.rm = TRUE),
    ground_temp_min_C = min(temperature_C, na.rm = TRUE),
    ground_temp_max_C = max(temperature_C, na.rm = TRUE),
    .groups = "drop"
  )

ground_primary

ground_response_summary <- ground_primary %>%
  summarize(
    n_tree_week_observations = n(),
    n_missing = sum(is.na(ground_temp_C)),
    min_ground_temp_C = min(ground_temp_C, na.rm = TRUE),
    mean_ground_temp_C = mean(ground_temp_C, na.rm = TRUE),
    median_ground_temp_C = median(ground_temp_C, na.rm = TRUE),
    max_ground_temp_C = max(ground_temp_C, na.rm = TRUE),
    sd_ground_temp_C = sd(ground_temp_C, na.rm = TRUE)
  )

ground_response_summary %>%
  print(n = Inf, width = Inf)


#check distributions

ground_distribution_summary <- ground_primary %>%
  summarize(
    n = n(),
    n_missing = sum(is.na(ground_temp_C)),
    min = min(ground_temp_C, na.rm = TRUE),
    q1 = quantile(ground_temp_C, 0.25, na.rm = TRUE),
    median = median(ground_temp_C, na.rm = TRUE),
    mean = mean(ground_temp_C, na.rm = TRUE),
    q3 = quantile(ground_temp_C, 0.75, na.rm = TRUE),
    max = max(ground_temp_C, na.rm = TRUE),
    sd = sd(ground_temp_C, na.rm = TRUE)
  )

ground_distribution_summary

ggplot(ground_primary, aes(x = ground_temp_C)) +
  geom_histogram(bins = 20, color = "black", fill = "grey80") +
  labs(
    x = "Midmorning ground temperature (°C)",
    y = "Tree-week observations"
  ) +
  theme_classic()

ggplot(ground_primary, aes(sample = ground_temp_C)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    x = "Theoretical quantiles",
    y = "Observed ground temperature (°C)"
  ) +
  theme_classic()

#no transformation needed

##Step: Run the repeated measurements model-----

rm_ground <- aov(
  ground_temp_C ~ site * species * week_f +
    Error(tree_id / week_f),
  data = ground_primary
)

summary(rm_ground) #week and site by week


# Extract repeated-measures statistics table
ground_rm_summary <- summary(rm_ground)

ground_rm_stats <- purrr::imap_dfr(
  ground_rm_summary,
  ~ {
    tab <- as.data.frame(.x[[1]])
    tab$term <- rownames(tab)
    
    tab %>%
      as_tibble() %>%
      rename(
        df = Df,
        sum_sq = `Sum Sq`,
        mean_sq = `Mean Sq`,
        f_value = `F value`,
        p_value = `Pr(>F)`
      ) %>%
      mutate(
        trait = "Midmorning ground temperature",
        response = "ground_temp_C",
        units = "°C",
        analysis = "Primary site × species × week repeated-measures ANOVA",
        error_stratum = .y,
        term = stringr::str_squish(term),
        p_value = as.numeric(p_value),
        significance = case_when(
          is.na(p_value) ~ "",
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          p_value < 0.1 ~ ".",
          TRUE ~ ""
        ),
        .before = 1
      )
  }
) %>%
  select(
    trait, response, units, analysis,
    error_stratum, term,
    df, sum_sq, mean_sq, f_value, p_value, significance
  )

ground_rm_stats

ground_rm_stats_terms <- ground_rm_stats %>%
  filter(term != "Residuals")

ground_rm_stats_terms

write_csv(
  ground_rm_stats_terms,
  "calculated_data/ground_temperature_primary_rm_stats.csv"
)

#model followups for interactions

#means for the interactions
# Observed site means by week
ground_site_week_means <- ground_primary %>%
  group_by(week, week_f, site) %>%
  summarize(
    n = n(),
    mean_ground_temp_C = mean(ground_temp_C, na.rm = TRUE),
    sd_ground_temp_C = sd(ground_temp_C, na.rm = TRUE),
    se_ground_temp_C = sd_ground_temp_C / sqrt(n),
    .groups = "drop"
  )

ground_site_week_means %>%
  print(n = Inf, width = Inf)

#site x week means

ground_site_week_diff <- ground_site_week_means %>%
  select(week, site, mean_ground_temp_C) %>%
  pivot_wider(
    names_from = site,
    values_from = mean_ground_temp_C
  ) %>%
  mutate(
    estimate_c_minus_p_C = c - p
  ) %>%
  arrange(week)

ground_site_week_diff %>%
  print(n = Inf, width = Inf)


# Site effect is tested within each week, with species retained in the model.

ground_site_week_tests <- ground_primary %>%
  group_by(week) %>%
  group_modify(~ {
    week_model <- aov(
      ground_temp_C ~ site + species,
      data = .x
    )
    
    week_tab <- as.data.frame(summary(week_model)[[1]]) %>%
      rownames_to_column("term") %>%
      as_tibble() %>%
      rename(
        df = Df,
        sum_sq = `Sum Sq`,
        mean_sq = `Mean Sq`,
        f_value = `F value`,
        p_raw = `Pr(>F)`
      ) %>%
      mutate(term = stringr::str_squish(term))
    
    week_tab %>%
      filter(term == "site")
  }) %>%
  ungroup() %>%
  left_join(
    ground_site_week_diff %>%
      select(week, estimate_c_minus_p_C),
    by = "week"
  ) %>%
  mutate(
    trait = "Midmorning ground temperature",
    response = "ground_temp_C",
    units = "°C",
    analysis = "Site effect within week",
    contrast_type = "Downtown minus park",
    p_holm = p.adjust(p_raw, method = "holm"),
    significant_holm = case_when(
      p_holm < 0.001 ~ "***",
      p_holm < 0.01 ~ "**",
      p_holm < 0.05 ~ "*",
      p_holm < 0.1 ~ ".",
      TRUE ~ ""
    ),
    .before = 1
  ) %>%
  select(
    trait, response, units, analysis, contrast_type,
    week, term,
    estimate_c_minus_p_C,
    df, sum_sq, mean_sq, f_value,
    p_raw, p_holm, significant_holm
  )

ground_site_week_tests %>%
  print(n = Inf, width = Inf)


##Step: Create figure------
site_labels <- c(
  c = "Downtown",
  p = "Park"
)

ground_fig_data <- ground_primary %>%
  group_by(site, week) %>%
  summarize(
    n = n(),
    mean_ground_temp_C = mean(ground_temp_C, na.rm = TRUE),
    sd_ground_temp_C = sd(ground_temp_C, na.rm = TRUE),
    se_ground_temp_C = sd_ground_temp_C / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    site_label = factor(site_labels[as.character(site)],
                        levels = c("Downtown", "Park"))
  )

fig_ground_site_week <- ggplot(
  ground_fig_data,
  aes(
    x = week,
    y = mean_ground_temp_C,
    group = site_label,
    linetype = site_label,
    shape = site_label
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      ymin = mean_ground_temp_C - se_ground_temp_C,
      ymax = mean_ground_temp_C + se_ground_temp_C
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  scale_x_continuous(
    breaks = sort(unique(ground_fig_data$week))
  ) +
  labs(
    x = "Week",
    y = "Midmorning ground temperature (°C)",
    linetype = "Planting site",
    shape = "Planting site"
  ) +
  theme_classic(base_size = 12)

fig_ground_site_week

ggsave(
  filename = "figures/fig_ground_temperature_site_week.png",
  plot = fig_ground_site_week,
  width = 6,
  height = 4,
  dpi = 600
)


