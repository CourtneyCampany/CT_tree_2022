
library(tidyverse)

##Step: Read in and wrangle LMA data-----

leaf_raw <- read.csv("raw_data/leafthick.csv")

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
# Make sure factor reference levels are consistent
leaf_complete <- leaf_complete %>%
  mutate(
    site = factor(site, levels = c("c", "p")),
    species = factor(species, levels = c("d", "h", "m")),
    week_f = factor(week)
  )


# Site differences within each week
# Justified by significant site:week_f interaction.
#
# Model within each week:
# log_lma_g_m2 ~ site + species
#
# Estimate is p - c, adjusted for species.
# Positive estimate = Park > Downtown.
# Negative estimate = Park < Downtown.

lma_site_week_tests <- leaf_complete %>%
  group_by(week) %>%
  group_modify(~{
    
    m <- lm(log_lma_g_m2 ~ site + species, data = .x)
    coefs <- summary(m)$coefficients
    
    tibble(
      estimate_p_minus_c = coefs["sitep", "Estimate"],
      std_error = coefs["sitep", "Std. Error"],
      statistic = coefs["sitep", "t value"],
      p_raw = coefs["sitep", "Pr(>|t|)"]
    )
    
  }) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  )

lma_site_week_tests
##Only week 8 remained significant after Holm correction.(p=0.0145)
#Park trees had lower log(LMA) than Downtown trees in week 8.


#Species differences within each week
# Justified by significant species:week_f interaction.
#
# Model within each week:
# log_lma_g_m2 ~ site + species
#
# This tests whether species differ within each week,
# adjusted for site.

lma_species_week_tests <- leaf_complete %>%
  group_by(week) %>%
  group_modify(~{
    
    m <- lm(log_lma_g_m2 ~ site + species, data = .x)
    a <- anova(m)
    
    tibble(
      df_species = a["species", "Df"],
      df_residual = a["Residuals", "Df"],
      F_value = a["species", "F value"],
      p_raw = a["species", "Pr(>F)"]
    )
    
  }) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  )

lma_species_week_tests
#species differences are very strong


# Species pairwise contrasts within each week
# Justified because species differs within every week.
#
# Model within each week:
# log_lma_g_m2 ~ site + species
#
# Species levels are d, h, m.
# Contrasts:
# h - d
# m - d
# m - h

lma_species_week_pairwise <- leaf_complete %>%
  group_by(week) %>%
  group_modify(~{
    
    m <- lm(log_lma_g_m2 ~ site + species, data = .x)
    
    b <- coef(m)
    v <- vcov(m)
    df_resid <- df.residual(m)
    
    contrast_list <- list(
      "h - d" = c(speciesh = 1),
      "m - d" = c(speciesm = 1),
      "m - h" = c(speciesm = 1, speciesh = -1)
    )
    
    bind_rows(
      lapply(names(contrast_list), function(contrast_name) {
        
        L <- rep(0, length(b))
        names(L) <- names(b)
        L[names(contrast_list[[contrast_name]])] <- contrast_list[[contrast_name]]
        
        estimate <- sum(L * b)
        std_error <- sqrt(as.numeric(t(L) %*% v %*% L))
        statistic <- estimate / std_error
        p_raw <- 2 * pt(abs(statistic), df = df_resid, lower.tail = FALSE)
        
        tibble(
          contrast = contrast_name,
          estimate = estimate,
          std_error = std_error,
          df = df_resid,
          statistic = statistic,
          p_raw = p_raw
        )
      })
    )
    
  }) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  )

lma_species_week_pairwise

#Descriptive weekly means for interpretation (not really needed)
lma_site_week_means <- leaf_complete %>%
  group_by(site, week) %>%
  summarise(
    n = n(),
    mean_lma_g_m2 = mean(lma_g_m2, na.rm = TRUE),
    se_lma_g_m2 = sd(lma_g_m2, na.rm = TRUE) / sqrt(n),
    mean_log_lma = mean(log_lma_g_m2, na.rm = TRUE),
    se_log_lma = sd(log_lma_g_m2, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )


lma_species_week_means <- leaf_complete %>%
  group_by(species, week) %>%
  summarise(
    n = n(),
    mean_lma_g_m2 = mean(lma_g_m2, na.rm = TRUE),
    se_lma_g_m2 = sd(lma_g_m2, na.rm = TRUE) / sqrt(n),
    mean_log_lma = mean(log_lma_g_m2, na.rm = TRUE),
    se_log_lma = sd(log_lma_g_m2, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

# Species weekly means for interpreting pairwise contrasts

lma_species_week_means <- leaf_complete %>%
  group_by(species, week) %>%
  summarise(
    n = n(),
    mean_lma_g_m2 = mean(lma_g_m2, na.rm = TRUE),
    se_lma_g_m2 = sd(lma_g_m2, na.rm = TRUE) / sqrt(n),
    mean_log_lma = mean(log_lma_g_m2, na.rm = TRUE),
    se_log_lma = sd(log_lma_g_m2, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

print(lma_site_week_means, n = Inf, width = Inf)
print(lma_species_week_means, n = Inf, width = Inf)

## Step: Create statistics table for manuscript-----

extract_rm_aov_stats <- function(model,
                                 trait,
                                 trait_label,
                                 units,
                                 model_response,
                                 transformation,
                                 analysis_name,
                                 data) {
  
  model_summary <- summary(model)
  
  stats_table <- purrr::imap_dfr(model_summary, function(stratum, stratum_name) {
    
    aov_table <- as.data.frame(stratum[[1]], check.names = FALSE)
    aov_table$term <- trimws(rownames(aov_table))
    
    df_den <- aov_table %>%
      filter(term == "Residuals") %>%
      pull(Df)
    
    if (length(df_den) == 0) {
      df_den <- NA_real_
    }
    
    aov_table %>%
      filter(term != "Residuals") %>%
      transmute(
        error_stratum = str_remove(stratum_name, "^Error: "),
        term = term,
        df_num = Df,
        df_den = df_den[1],
        sum_sq = `Sum Sq`,
        mean_sq = `Mean Sq`,
        F_value = `F value`,
        p_value = `Pr(>F)`
      )
  }) %>%
    mutate(
      trait = trait,
      trait_label = trait_label,
      units = units,
      model_response = model_response,
      transformation = transformation,
      analysis_name = analysis_name,
      n_obs = nrow(data),
      n_trees = n_distinct(data$tree_id),
      p_significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        p_value < 0.1 ~ ".",
        TRUE ~ ""
      )
    ) %>%
    select(
      trait, trait_label, units,
      model_response, transformation, analysis_name,
      n_obs, n_trees,
      error_stratum, term,
      df_num, df_den,
      sum_sq, mean_sq, F_value, p_value, p_significance
    )
  
  return(stats_table)
}

lma_stats_primary <- extract_rm_aov_stats(
  model = rm_lma,
  trait = "leaf_mass_per_area",
  trait_label = "Leaf mass per area",
  units = "g m^-2",
  model_response = "log_lma_g_m2",
  transformation = "natural log",
  analysis_name = "Primary site x species x week repeated-measures ANOVA",
  data = leaf_complete
)

lma_stats_primary

write_csv(lma_stats_primary, "calculated_data/lma_stats_primary.csv")

##Step: Is LMA affected by pit size in downtown trees?-----

#prepare tree metadata for merge

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

#merge pit data with LMA

lma_pit <- leaf_complete %>%
  left_join(
    tree_metadata_clean %>%
      select(
        site, species, replicate, tree_id,
        pit_size, pit_length_cm, pit_width_cm, pit_depth_cm,
        pit_area_m2, pit_volume_m3
      ),
    by = c("site", "species", "replicate", "tree_id")
  ) %>%
  filter(site == "c") %>%
  mutate(
    site = factor(site, levels = c("c", "p")),
    species = factor(species, levels = c("d", "h", "m")),
    replicate = factor(replicate),
    tree_id = factor(tree_id),
    week_f = factor(week),
    pit_size = factor(pit_size, levels = c("small", "large"))
  )

#audit merge
lma_pit_check <- lma_pit %>%
  summarise(
    n_rows = n(),
    n_trees = n_distinct(tree_id),
    n_species = n_distinct(species),
    n_weeks = n_distinct(week),
    missing_lma = sum(is.na(lma_g_m2)),
    missing_log_lma = sum(is.na(log_lma_g_m2)),
    missing_pit_size = sum(is.na(pit_size)),
    missing_pit_area_m2 = sum(is.na(pit_area_m2)),
    missing_pit_volume_m3 = sum(is.na(pit_volume_m3)),
    min_pit_area_m2 = min(pit_area_m2, na.rm = TRUE),
    max_pit_area_m2 = max(pit_area_m2, na.rm = TRUE),
    min_pit_volume_m3 = min(pit_volume_m3, na.rm = TRUE),
    max_pit_volume_m3 = max(pit_volume_m3, na.rm = TRUE)
  )

lma_pit_tree_check <- lma_pit %>%
  group_by(species, replicate, tree_id) %>%
  summarise(
    n_records = n(),
    n_weeks = n_distinct(week),
    pit_size = first(pit_size),
    pit_area_m2 = first(pit_area_m2),
    pit_volume_m3 = first(pit_volume_m3),
    missing_lma = sum(is.na(lma_g_m2)),
    .groups = "drop"
  ) %>%
  arrange(species, replicate)

lma_pit_balance_check <- lma_pit %>%
  count(species, pit_size, week, name = "n_records") %>%
  arrange(species, pit_size, week)

lma_pit_check
lma_pit_tree_check
lma_pit_balance_check

#run model

rm_lma_pit <- aov(
  log_lma_g_m2 ~ pit_size * species * week_f +
    Error(tree_id / week_f),
  data = lma_pit
)

summary(rm_lma_pit)
#pit size x week (p = 0.0347)

#mergeable model stats table for manuscript
lma_stats_pit <- extract_rm_aov_stats(
  model = rm_lma_pit,
  trait = "leaf_mass_per_area",
  trait_label = "Leaf mass per area",
  units = "g m^-2",
  model_response = "log_lma_g_m2",
  transformation = "natural log",
  analysis_name = "Downtown-only pit_size x species x week repeated-measures ANOVA",
  data = lma_pit
)

lma_stats_pit

write_csv(lma_stats_pit, "calculated_data/lma_stats_pit.csv")

#interaction term model follow up

lma_pit <- lma_pit %>%
  mutate(
    pit_size = factor(pit_size, levels = c("small", "large")),
    species = factor(species, levels = c("d", "h", "m")),
    week_f = factor(week)
  )

# Pit-size differences within each week
# Model within each week:
# log_lma_g_m2 ~ pit_size + species
#
# Estimate is large - small, adjusted for species.
# Positive estimate = large pits > small pits.
# Negative estimate = large pits < small pits.

lma_pit_week_tests <- lma_pit %>%
  group_by(week) %>%
  group_modify(~{
    
    m <- lm(log_lma_g_m2 ~ pit_size + species, data = .x)
    coefs <- summary(m)$coefficients
    
    tibble(
      estimate_large_minus_small = coefs["pit_sizelarge", "Estimate"],
      std_error = coefs["pit_sizelarge", "Std. Error"],
      statistic = coefs["pit_sizelarge", "t value"],
      p_raw = coefs["pit_sizelarge", "Pr(>|t|)"]
    )
    
  }) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  )

print(lma_pit_week_tests, n = Inf, width = Inf)
#only significant for week 8

##visualize the means

#Downtown pit-size weekly means for interpretation (sig weeks only)

sig_pit_weeks <- lma_pit_week_tests %>%
  filter(significant == "Yes") %>%
  pull(week)

lma_pit_sig_week_means <- lma_pit %>%
  filter(week %in% sig_pit_weeks) %>%
  group_by(pit_size, week) %>%
  summarise(
    n = n(),
    mean_lma_g_m2 = mean(lma_g_m2, na.rm = TRUE),
    se_lma_g_m2 = sd(lma_g_m2, na.rm = TRUE) / sqrt(n),
    mean_log_lma = mean(log_lma_g_m2, na.rm = TRUE),
    se_log_lma = sd(log_lma_g_m2, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

print(lma_pit_sig_week_means, n = Inf, width = Inf)
#not biologically important (size x week, only week 8)

##Step: Plot the LMA data-----

lma_plot_data <- leaf_complete %>%
  group_by(site, species, week) %>%
  summarise(
    n = n(),
    mean_lma_g_m2 = mean(lma_g_m2, na.rm = TRUE),
    se_lma_g_m2 = sd(lma_g_m2, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

lma_site_species_plot <- ggplot(
  lma_plot_data,
  aes(
    x = week,
    y = mean_lma_g_m2,
    group = site,
    shape = site,
    linetype = site
  )
) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      ymin = mean_lma_g_m2 - se_lma_g_m2,
      ymax = mean_lma_g_m2 + se_lma_g_m2
    ),
    width = 0.15
  ) +
  facet_wrap(~ species) +
  scale_x_continuous(
    breaks = 1:10
  ) +
  labs(
    x = "Week",
    y = expression("Leaf mass per area (g " * m^{-2} * ")"),
    shape = "Site",
    linetype = "Site"
  ) +
  theme_classic()

lma_site_species_plot

#save plot
ggsave(
  filename = "figures/lma_site_species_week_plot.png",
  plot = lma_site_species_plot,
  width = 7,
  height = 4.5,
  dpi = 300
)




lma_pit_plot_data <- lma_pit %>%
  group_by(pit_size, week) %>%
  summarise(
    n = n(),
    mean_lma_g_m2 = mean(lma_g_m2, na.rm = TRUE),
    se_lma_g_m2 = sd(lma_g_m2, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

lma_pit_week_plot <- ggplot(
  lma_pit_plot_data,
  aes(
    x = week,
    y = mean_lma_g_m2,
    group = pit_size,
    shape = pit_size,
    linetype = pit_size
  )
) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      ymin = mean_lma_g_m2 - se_lma_g_m2,
      ymax = mean_lma_g_m2 + se_lma_g_m2
    ),
    width = 0.15
  ) +
  scale_x_continuous(
    breaks = 1:10
  ) +
  labs(
    x = "Week",
    y = expression("Leaf mass per area (g " * m^{-2} * ")"),
    shape = "Pit size",
    linetype = "Pit size"
  ) +
  theme_classic()

lma_pit_week_plot

##LMA increased across weeks, likely has typical maturation process across the early growing season (cites).
##If LMA increased as a stress response to hotter/drier mid-summer conditions, it did so at both sites. 


