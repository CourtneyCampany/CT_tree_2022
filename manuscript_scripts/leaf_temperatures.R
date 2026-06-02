library(tidyverse)

##Step: Read, wrangle, and check data structure-----

leaf_temp_raw <- read_csv("raw_data/leaf_temperature.csv")

leaf_temp_raw %>%
  summarise(
    n_rows = n(),
    n_dates = n_distinct(date),
    min_week = min(week, na.rm = TRUE),
    max_week = max(week, na.rm = TRUE),
    n_weeks = n_distinct(week),
    n_sites = n_distinct(site),
    n_species = n_distinct(species),
    n_tree_reps = n_distinct(replicate),
    n_temp_reps = n_distinct(temp_replicate)
  )

#clean variables, convert to Celsius
leaf_temp_clean <- leaf_temp_raw %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    site = factor(site),
    species = factor(species),
    replicate = as.factor(replicate),
    temp_replicate = as.factor(temp_replicate),
    tree_id = interaction(site, species, replicate, sep = "_", drop = TRUE),
    week_f = factor(week),
    top_C = (top_F - 32) * 5 / 9,
    bottom_C = (bottom_F - 32) * 5 / 9,
    leaf_temp_mean_C = rowMeans(across(c(top_C, bottom_C)), na.rm = TRUE),
    top_bottom_diff_C = top_C - bottom_C
  )


leaf_temp_tree_week <- leaf_temp_clean %>%
  group_by(tree_id, site, species, replicate, week, week_f) %>%
  summarise(
    date_min = min(date),
    date_max = max(date),
    n_temp_reps = n_distinct(temp_replicate),
    top_temp_C = mean(top_C, na.rm = TRUE),
    bottom_temp_C = mean(bottom_C, na.rm = TRUE),
    leaf_temp_C = mean(leaf_temp_mean_C, na.rm = TRUE),
    top_bottom_diff_C = mean(top_bottom_diff_C, na.rm = TRUE),
    top_sd_C = sd(top_C, na.rm = TRUE),
    bottom_sd_C = sd(bottom_C, na.rm = TRUE),
    .groups = "drop"
  )


leaf_temp_complete <- leaf_temp_tree_week %>%
  group_by(tree_id) %>%
  filter(n_distinct(week) == 9) %>%
  ungroup()


# ggplot(leaf_temp_complete, aes(x = top_temp_C)) +
#   geom_histogram(bins = 20) +
#   labs(
#     x = "Midmorning top leaf temperature (°C)",
#     y = "Count"
#   ) +
#   theme_classic()
# 
# ggplot(leaf_temp_complete, aes(sample = top_temp_C)) +
#   stat_qq() +
#   stat_qq_line() +
#   labs(
#     x = "Theoretical quantiles",
#     y = "Observed top leaf temperature (°C)"
#   ) +
#   theme_classic()
#no transformation needed


##Step: Run repeated measures-----

rm_top_temp <- aov(
  top_temp_C ~ site * species * week_f + Error(tree_id / week_f),
  data = leaf_temp_complete
)

summary(rm_top_temp)
#week, site x week

#extract results for table

extract_rm_stats <- function(model, trait, response, units, transformation, analysis_name) {
  
  model_summary <- summary(model)
  
  bind_rows(
    imap(model_summary, function(stratum, stratum_name) {
      as.data.frame(stratum[[1]]) %>%
        rownames_to_column("term") %>%
        mutate(error_stratum = stratum_name)
    })
  ) %>%
    rename(
      df = Df,
      sum_sq = `Sum Sq`,
      mean_sq = `Mean Sq`,
      f_value = `F value`,
      p_value = `Pr(>F)`
    ) %>%
    filter(term != "Residuals") %>%
    mutate(
      trait = trait,
      response = response,
      units = units,
      transformation = transformation,
      analysis_name = analysis_name,
      .before = term
    )
}

top_temp_stats_primary <- extract_rm_stats(
  model = rm_top_temp,
  trait = "Midmorning top leaf temperature",
  response = "top_temp_C",
  units = "degrees C",
  transformation = "none",
  analysis_name = "Primary site × species × week repeated-measures model"
)

top_temp_stats_primary

##means table
top_temp_means_primary <- leaf_temp_complete %>%
  group_by(site, species, week, week_f) %>%
  summarise(
    n_trees = n_distinct(tree_id),
    mean_top_temp_C = mean(top_temp_C, na.rm = TRUE),
    sd_top_temp_C = sd(top_temp_C, na.rm = TRUE),
    se_top_temp_C = sd_top_temp_C / sqrt(n_trees),
    .groups = "drop"
  ) %>%
  mutate(
    trait = "Midmorning top leaf temperature",
    response = "top_temp_C",
    units = "degrees C",
    analysis_name = "Primary site × species × week repeated-measures model",
    .before = site
  )

top_temp_means_primary

##follow up with site x week
#during which weeks did the two planting environments differ in top leaf temperature?

leaf_temp_site_week_contrasts <- leaf_temp_complete %>%
  mutate(
    site = relevel(site, ref = "c"),
    species = factor(species)
  ) %>%
  group_by(week) %>%
  group_modify(~ {
    
    week_model <- lm(top_temp_C ~ site + species, data = .x)
    site_test <- summary(week_model)$coefficients["sitep", ]
    
    tibble(
      mean_top_temp_c = mean(.x$top_temp_C[.x$site == "c"], na.rm = TRUE),
      mean_top_temp_p = mean(.x$top_temp_C[.x$site == "p"], na.rm = TRUE),
      estimate_p_minus_c = unname(site_test["Estimate"]),
      std_error = unname(site_test["Std. Error"]),
      statistic = unname(site_test["t value"]),
      p_raw = unname(site_test["Pr(>|t|)"])
    )
  }) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  )

leaf_temp_site_week_contrasts


##Step: Plot temp leaf temperature------

top_temp_site_week_means <- leaf_temp_complete %>%
  group_by(site, week, week_f) %>%
  summarise(
    n_trees = n_distinct(tree_id),
    mean_top_temp_C = mean(top_temp_C, na.rm = TRUE),
    sd_top_temp_C = sd(top_temp_C, na.rm = TRUE),
    se_top_temp_C = sd_top_temp_C / sqrt(n_trees),
    .groups = "drop"
  )

top_temp_site_week_means

fig_top_temp_site_week <- ggplot(
  top_temp_site_week_means,
  aes(
    x = week,
    y = mean_top_temp_C,
    group = site,
    linetype = site,
    shape = site
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      ymin = mean_top_temp_C - se_top_temp_C,
      ymax = mean_top_temp_C + se_top_temp_C
    ),
    width = 0.15,
    linewidth = 0.4
  ) +
  scale_x_continuous(
    breaks = sort(unique(top_temp_site_week_means$week))
  ) +
  labs(
    x = "Week",
    y = "Midmorning top leaf temperature (°C)",
    linetype = "Site",
    shape = "Site"
  ) +
  theme_classic(base_size = 12)

fig_top_temp_site_week

