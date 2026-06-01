library(tidyverse)

##SteP: Read, wrangle, and summarize isotope data-----
isotope_data <- read.csv("raw_data/isotope_data.csv")

isotope_data <- isotope_data %>%
  mutate(
    site = factor(site),
    species = factor(species),
    replicate = factor(replicate),
    week = as.numeric(week),
    week_f = factor(week),
    tree_id = interaction(site, species, replicate, drop = TRUE),
    CN_ratio = carbon_perc / nitro_perc,
    log_CN_ratio = log(CN_ratio)
  )

#check skewness
skewness_simple <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / sd(x)^3
}

trait_summary <- isotope_data %>%
  summarize(
    n_nitro = sum(!is.na(nitro_perc)),
    mean_nitro = mean(nitro_perc, na.rm = TRUE),
    median_nitro = median(nitro_perc, na.rm = TRUE),
    sd_nitro = sd(nitro_perc, na.rm = TRUE),
    min_nitro = min(nitro_perc, na.rm = TRUE),
    max_nitro = max(nitro_perc, na.rm = TRUE),
    skew_nitro = skewness_simple(nitro_perc),
    
    n_carbon = sum(!is.na(carbon_perc)),
    mean_carbon = mean(carbon_perc, na.rm = TRUE),
    median_carbon = median(carbon_perc, na.rm = TRUE),
    sd_carbon = sd(carbon_perc, na.rm = TRUE),
    min_carbon = min(carbon_perc, na.rm = TRUE),
    max_carbon = max(carbon_perc, na.rm = TRUE),
    skew_carbon = skewness_simple(carbon_perc),
    
    n_c13 = sum(!is.na(c13)),
    mean_c13 = mean(c13, na.rm = TRUE),
    median_c13 = median(c13, na.rm = TRUE),
    sd_c13 = sd(c13, na.rm = TRUE),
    min_c13 = min(c13, na.rm = TRUE),
    max_c13 = max(c13, na.rm = TRUE),
    skew_c13 = skewness_simple(c13),
    
    n_CN = sum(!is.na(CN_ratio)),
    mean_CN = mean(CN_ratio, na.rm = TRUE),
    median_CN = median(CN_ratio, na.rm = TRUE),
    sd_CN = sd(CN_ratio, na.rm = TRUE),
    min_CN = min(CN_ratio, na.rm = TRUE),
    max_CN = max(CN_ratio, na.rm = TRUE),
    skew_CN = skewness_simple(CN_ratio),
    
    mean_log_CN = mean(log_CN_ratio, na.rm = TRUE),
    median_log_CN = median(log_CN_ratio, na.rm = TRUE),
    sd_log_CN = sd(log_CN_ratio, na.rm = TRUE),
    skew_log_CN = skewness_simple(log_CN_ratio)
  )

trait_summary


trait_long <- isotope_data %>%
  select(site, species, replicate, week, tree_id,
         nitro_perc, carbon_perc, c13, CN_ratio, log_CN_ratio) %>%
  pivot_longer(
    cols = c(nitro_perc, carbon_perc, c13, CN_ratio, log_CN_ratio),
    names_to = "trait",
    values_to = "value"
  )

# Histograms
ggplot(trait_long, aes(x = value)) +
  geom_histogram(bins = 25) +
  facet_wrap(~ trait, scales = "free") +
  theme_classic()

# Q-Q plots
ggplot(trait_long, aes(sample = value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ trait, scales = "free") +
  theme_classic()

# Boxplots by site and species
ggplot(trait_long, aes(x = species, y = value)) +
  geom_boxplot() +
  facet_grid(trait ~ site, scales = "free_y") +
  theme_classic()

##Step: Primary repeated measures models-----
aov_N <- aov(
  nitro_perc ~ site * species * week_f + Error(tree_id/week_f),
  data = isotope_data
)


aov_c13 <- aov(
  c13 ~ site * species * week_f + Error(tree_id/week_f),
  data = isotope_data
)

aov_log_CN <- aov(
  log_CN_ratio ~ site * species * week_f + Error(tree_id/week_f),
  data = isotope_data
)

summary(aov_N)
summary(aov_c13)
summary(aov_log_CN)

#model diagnostics
diag_N <- lm(
  nitro_perc ~ site * species * week_f + tree_id,
  data = isotope_data
)

diag_log_CN <- lm(
  log_CN_ratio ~ site * species * week_f + tree_id,
  data = isotope_data
)

diag_c13 <- lm(
  c13 ~ site * species * week_f + tree_id,
  data = isotope_data
)

#Extract residuals, fitted values, and influence measures
extract_diag <- function(model, trait_name, data) {
  row_ids <- as.integer(names(residuals(model)))
  
  data[row_ids, ] %>%
    mutate(
      trait = trait_name,
      fitted = fitted(model),
      residual = residuals(model),
      std_residual = rstandard(model),
      cooks_distance = cooks.distance(model)
    )
}

diag_all <- bind_rows(
  extract_diag(diag_N, "Foliar %N", isotope_data),
  extract_diag(diag_log_CN, "log(C/N)", isotope_data),
  extract_diag(diag_c13, "c13", isotope_data)
)

#residuals
ggplot(diag_all, aes(x = fitted, y = std_residual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted") +
  facet_wrap(~ trait, scales = "free") +
  theme_classic()

diag_top5_residuals %>%
  select(
    trait, site, species, replicate, week, tree_id,
    nitro_perc, carbon_perc, c13, CN_ratio,
    fitted, residual, std_residual, cooks_distance
  ) %>%
  print(n = Inf, width = Inf)

diag_extreme_residuals <- diag_all %>%
  filter(abs(std_residual) > 3) %>%
  select(
    trait, site, species, replicate, week, tree_id,
    nitro_perc, carbon_perc, c13, CN_ratio,
    fitted, residual, std_residual, cooks_distance
  ) %>%
  arrange(trait, desc(abs(std_residual)))

diag_extreme_residuals %>%
  print(n = Inf, width = Inf)

#q-q plots
ggplot(diag_all, aes(sample = std_residual)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ trait, scales = "free") +
  theme_classic()
#shapiro
diag_all %>%
  group_by(trait) %>%
  summarize(
    n = n(),
    shapiro_p = shapiro.test(std_residual)$p.value,
    min_std_resid = min(std_residual),
    max_std_resid = max(std_residual),
    .groups = "drop"
  )
#some non-normaility but for N based on a singular point and with
#13C the transformation is not easy


#should we also log transform N%?
isotope_data <- isotope_data %>%
  mutate(
    log_nitro_perc = log(nitro_perc)
  )

aov_log_N <- aov(
  log_nitro_perc ~ site * species * week_f + Error(tree_id/week_f),
  data = isotope_data
)

summary(aov_log_N)

diag_log_N <- lm(
  log_nitro_perc ~ site * species * week_f + tree_id,
  data = isotope_data
)

diag_log_N_table <- extract_diag(
  diag_log_N,
  "log(Foliar %N)",
  isotope_data
)

diag_log_N_table %>%
  summarize(
    n = n(),
    shapiro_p = shapiro.test(std_residual)$p.value,
    min_std_resid = min(std_residual),
    max_std_resid = max(std_residual),
    max_abs_std_resid = max(abs(std_residual)),
    max_cooks_distance = max(cooks_distance)
  )

fligner.test(log_nitro_perc ~ interaction(site, species), data = isotope_data)
fligner.test(log_nitro_perc ~ week_f, data = isotope_data)

## aov struggled with the missing trees that did not have complete repeated measures
## remove those trees (n=4) and only run models on trees that are complete'
complete_tree_ids <- isotope_data %>%
  group_by(tree_id) %>%
  summarize(n_weeks = n_distinct(week), .groups = "drop") %>%
  filter(n_weeks == 10) %>%
  pull(tree_id)

isotope_complete <- isotope_data %>%
  filter(tree_id %in% complete_tree_ids)

n_distinct(isotope_data$tree_id)
n_distinct(isotope_complete$tree_id)

nrow(isotope_data)
nrow(isotope_complete)

aov_log_N_complete <- aov(
  log_nitro_perc ~ site * species * week_f + Error(tree_id/week_f),
  data = isotope_complete
)

# aov_N_complete <- aov(
#   nitro_perc ~ site * species * week_f + Error(tree_id/week_f),
#   data = isotope_complete
# )

aov_log_CN_complete <- aov(
  log_CN_ratio ~ site * species * week_f + Error(tree_id/week_f),
  data = isotope_complete
)

aov_c13_complete <- aov(
  c13 ~ site * species * week_f + Error(tree_id/week_f),
  data = isotope_complete
)

summary(aov_log_N_complete)
# summary(aov_N_complete)
summary(aov_log_CN_complete)
summary(aov_c13_complete)

#evaluate whether to log N%
diag_log_N <- lm(
  log_nitro_perc ~ site * species * week_f + tree_id,
  data = isotope_data
)

diag_log_N_table <- extract_diag(
  diag_log_N,
  "log(Foliar %N)",
  isotope_data
)

diag_log_N_table %>%
  summarize(
    n = n(),
    shapiro_p = shapiro.test(std_residual)$p.value,
    min_std_resid = min(std_residual),
    max_std_resid = max(std_residual),
    max_abs_std_resid = max(abs(std_residual)),
    max_cooks_distance = max(cooks_distance)
  )

fligner.test(log_nitro_perc ~ interaction(site, species), data = isotope_data)
fligner.test(log_nitro_perc ~ week_f, data = isotope_data)

# Diagnostic models for complete-tree AOVs only

diag_log_N_complete <- lm(
  log_nitro_perc ~ site * species * week_f + tree_id,
  data = isotope_complete
)

diag_N_complete <- lm(
  nitro_perc ~ site * species * week_f + tree_id,
  data = isotope_complete
)

diag_log_CN_complete <- lm(
  log_CN_ratio ~ site * species * week_f + tree_id,
  data = isotope_complete
)

diag_c13_complete <- lm(
  c13 ~ site * species * week_f + tree_id,
  data = isotope_complete
)

extract_diag <- function(model, trait_name, data) {
  row_ids <- as.integer(names(residuals(model)))
  
  data[row_ids, ] %>%
    mutate(
      trait = trait_name,
      fitted = fitted(model),
      residual = residuals(model),
      std_residual = rstandard(model),
      cooks_distance = cooks.distance(model)
    )
}

diag_complete_all <- bind_rows(
  extract_diag(diag_log_N_complete, "log(Foliar %N)", isotope_complete),
  extract_diag(diag_N_complete, "Foliar %N", isotope_complete),
  extract_diag(diag_log_CN_complete, "log(C/N)", isotope_complete),
  extract_diag(diag_c13_complete, "c13", isotope_complete)
)

diag_complete_all %>%
  group_by(trait) %>%
  summarize(
    n = n(),
    shapiro_p = shapiro.test(std_residual)$p.value,
    min_std_resid = min(std_residual),
    max_std_resid = max(std_residual),
    max_abs_std_resid = max(abs(std_residual)),
    max_cooks_distance = max(cooks_distance),
    .groups = "drop"
  )

fligner.test(log_nitro_perc ~ interaction(site, species), data = isotope_complete)
fligner.test(nitro_perc ~ interaction(site, species), data = isotope_complete)
fligner.test(log_CN_ratio ~ interaction(site, species), data = isotope_complete)
fligner.test(c13 ~ interaction(site, species), data = isotope_complete)
fligner.test(log_nitro_perc ~ week_f, data = isotope_complete)
fligner.test(nitro_perc ~ week_f, data = isotope_complete)
fligner.test(log_CN_ratio ~ week_f, data = isotope_complete)
fligner.test(c13 ~ week_f, data = isotope_complete)

#going to use complete repeated measures trees data, log CN, log N, c13
#c13 = species, species x week, site x week
#log CN = site, species, week, 3 way
#log N = site, species, site & species x week

#investigate site x week
# Function to run site/species AOVs within each week
week_followup_aov <- function(data, response_var) {
  
  weeks <- sort(unique(data$week))
  
  results <- lapply(weeks, function(w) {
    
    dat_w <- data[data$week == w, ]
    
    form <- as.formula(
      paste(response_var, "~ site * species")
    )
    
    mod <- aov(form, data = dat_w)
    tab <- summary(mod)[[1]]
    
    data.frame(
      week = w,
      effect = rownames(tab),
      df = tab$Df,
      F_value = tab$`F value`,
      p_value = tab$`Pr(>F)`,
      row.names = NULL
    )
  })
  
  do.call(rbind, results)
  
  log_N_week_followup <- week_followup_aov(
    isotope_complete,
    "log_nitro_perc"
  )
  
  log_CN_week_followup <- week_followup_aov(
    isotope_complete,
    "log_CN_ratio"
  )
  
  c13_week_followup <- week_followup_aov(
    isotope_complete,
    "c13"
  )
  
  log_N_week_followup
  log_CN_week_followup
  c13_week_followup
}

#holm
log_N_week_followup <- log_N_week_followup %>%
  filter(effect != "Residuals") %>%
  group_by(effect) %>%
  mutate(p_holm = p.adjust(p_value, method = "holm")) %>%
  ungroup()

log_CN_week_followup <- log_CN_week_followup %>%
  filter(effect != "Residuals") %>%
  group_by(effect) %>%
  mutate(p_holm = p.adjust(p_value, method = "holm")) %>%
  ungroup()

c13_week_followup <- c13_week_followup %>%
  filter(effect != "Residuals") %>%
  group_by(effect) %>%
  mutate(p_holm = p.adjust(p_value, method = "holm")) %>%
  ungroup()

log_N_week_followup
log_CN_week_followup
c13_week_followup

log_N_week_followup <- log_N_week_followup %>%
  mutate(effect = stringr::str_trim(effect))

log_CN_week_followup <- log_CN_week_followup %>%
  mutate(effect = stringr::str_trim(effect))

c13_week_followup <- c13_week_followup %>%
  mutate(effect = stringr::str_trim(effect))

log_N_week_followup %>%
  filter(effect %in% c("site", "species", "site:species")) %>%
  arrange(effect, week) %>%
  print(n = Inf)

log_CN_week_followup %>%
  filter(effect %in% c("site", "species", "site:species")) %>%
  arrange(effect, week) %>%
  print(n = Inf)

c13_week_followup %>%
  filter(effect %in% c("site", "species", "site:species")) %>%
  arrange(effect, week) %>%
  print(n = Inf)

#direction of site differences
site_week_means <- isotope_complete %>%
  group_by(site, week) %>%
  summarize(
    n = n(),
    mean_N = mean(nitro_perc, na.rm = TRUE),
    se_N = sd(nitro_perc, na.rm = TRUE) / sqrt(n),
    
    mean_CN = mean(CN_ratio, na.rm = TRUE),
    se_CN = sd(CN_ratio, na.rm = TRUE) / sqrt(n),
    
    mean_c13 = mean(c13, na.rm = TRUE),
    se_c13 = sd(c13, na.rm = TRUE) / sqrt(n),
    
    .groups = "drop"
  ) %>%
  arrange(week, site)

site_week_means

site_week_contrasts <- site_week_means %>%
  select(site, week, mean_N, mean_CN, mean_c13) %>%
  pivot_wider(
    names_from = site,
    values_from = c(mean_N, mean_CN, mean_c13)
  ) %>%
  mutate(
    N_p_minus_c = mean_N_p - mean_N_c,
    CN_p_minus_c = mean_CN_p - mean_CN_c,
    c13_p_minus_c = mean_c13_p - mean_c13_c
  )

site_week_contrasts

species_week_means <- isotope_complete %>%
  group_by(species, week) %>%
  summarize(
    n = n(),
    mean_N = mean(nitro_perc, na.rm = TRUE),
    se_N = sd(nitro_perc, na.rm = TRUE) / sqrt(n),
    
    mean_CN = mean(CN_ratio, na.rm = TRUE),
    se_CN = sd(CN_ratio, na.rm = TRUE) / sqrt(n),
    
    mean_c13 = mean(c13, na.rm = TRUE),
    se_c13 = sd(c13, na.rm = TRUE) / sqrt(n),
    
    .groups = "drop"
  ) %>%
  arrange(week, species)

species_week_means

species_week_ranks <- species_week_means %>%
  group_by(week) %>%
  mutate(
    rank_N = rank(-mean_N),
    rank_CN = rank(mean_CN),
    rank_c13_less_negative = rank(-mean_c13)
  ) %>%
  arrange(week, rank_N)

species_week_ranks


##Step: Make figures ------

#site × week pattern for foliar N and C/N across species
site_week_plot_data <- site_week_plot_data %>%
  mutate(
    trait_label = case_when(
      trait == "mean_N" ~ "Foliar N (%)",
      trait == "mean_CN" ~ "Foliar C/N",
      trait == "mean_c13" ~ "c13"
    )
  )

ggplot(site_week_plot_data, aes(x = week, y = mean, group = site, shape = site)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.15
  ) +
  facet_wrap(~ trait_label, scales = "free_y") +
  theme_classic() +
  labs(
    x = "Week",
    y = "Mean ± SE",
    shape = "Site"
  )

#species x week
species_week_plot_data <- species_week_means %>%
  pivot_longer(
    cols = c(mean_N, mean_CN, mean_c13),
    names_to = "trait",
    values_to = "mean"
  ) %>%
  mutate(
    se = case_when(
      trait == "mean_N" ~ se_N,
      trait == "mean_CN" ~ se_CN,
      trait == "mean_c13" ~ se_c13
    ),
    trait_label = case_when(
      trait == "mean_N" ~ "Foliar N (%)",
      trait == "mean_CN" ~ "Foliar C/N",
      trait == "mean_c13" ~ "c13"
    )
  )

ggplot(species_week_plot_data, aes(x = week, y = mean, group = species, shape = species)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.15
  ) +
  facet_wrap(~ trait_label, scales = "free_y") +
  theme_classic() +
  labs(
    x = "Week",
    y = "Mean ± SE",
    shape = "Species"
  )

ggsave(
  "figures/elemental_isotope_site_week_figure.png",
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/elemental_isotope_species_week_figure.png",
  width = 8,
  height = 5,
  dpi = 300
)

#single panel figure
# Combine site and species summaries into one long plotting table
# Make site-level summary
site_week_means <- isotope_complete %>%
  group_by(site, week) %>%
  summarize(
    n = n(),
    
    mean_N = mean(nitro_perc, na.rm = TRUE),
    se_N = sd(nitro_perc, na.rm = TRUE) / sqrt(n),
    
    mean_CN = mean(CN_ratio, na.rm = TRUE),
    se_CN = sd(CN_ratio, na.rm = TRUE) / sqrt(n),
    
    mean_c13 = mean(c13, na.rm = TRUE),
    se_c13 = sd(c13, na.rm = TRUE) / sqrt(n),
    
    .groups = "drop"
  ) %>%
  mutate(
    panel = "Site",
    group = as.character(site)
  )

# Make species-level summary
species_week_means <- isotope_complete %>%
  group_by(species, week) %>%
  summarize(
    n = n(),
    
    mean_N = mean(nitro_perc, na.rm = TRUE),
    se_N = sd(nitro_perc, na.rm = TRUE) / sqrt(n),
    
    mean_CN = mean(CN_ratio, na.rm = TRUE),
    se_CN = sd(CN_ratio, na.rm = TRUE) / sqrt(n),
    
    mean_c13 = mean(c13, na.rm = TRUE),
    se_c13 = sd(c13, na.rm = TRUE) / sqrt(n),
    
    .groups = "drop"
  ) %>%
  mutate(
    panel = "Species",
    group = as.character(species)
  )

elemental_combined_plot_data <- bind_rows(
  site_week_means,
  species_week_means
) %>%
  pivot_longer(
    cols = c(mean_N, mean_CN, mean_c13),
    names_to = "trait",
    values_to = "mean"
  ) %>%
  mutate(
    se = case_when(
      trait == "mean_N" ~ se_N,
      trait == "mean_CN" ~ se_CN,
      trait == "mean_c13" ~ se_c13
    ),
    trait_label = case_when(
      trait == "mean_N" ~ "Foliar N (%)",
      trait == "mean_CN" ~ "Foliar C/N",
      trait == "mean_c13" ~ "c13"
    ),
    panel = factor(panel, levels = c("Site", "Species")),
    trait_label = factor(
      trait_label,
      levels = c("Foliar N (%)", "Foliar C/N", "c13")
    )
  )

elemental_combined_plot_data <- elemental_combined_plot_data %>%
  mutate(
    group = case_when(
      panel == "Site" & group == "c" ~ "Downtown",
      panel == "Site" & group == "p" ~ "Park",
      TRUE ~ group
    )
  )

# Combined panel figure
elemental_combined_panel <- ggplot(
  elemental_combined_plot_data,
  aes(x = week, y = mean, group = group, shape = group, linetype = group)
) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.15
  ) +
  facet_grid(
    trait_label ~ panel,
    scales = "free_y"
  ) +
  theme_classic() +
  labs(
    x = "Week",
    y = "Mean ± SE",
    shape = "Group",
    linetype = "Group"
  )

elemental_combined_panel

ggsave(
  "figures/elemental_isotope_combined_panel.png",
  elemental_combined_panel,
  width = 9,
  height = 8,
  dpi = 300
)

##SteP: Extract model results for a manuscript table-----

# Helper function for significance codes
sig_code <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ ".",
    TRUE ~ ""
  )
}

# Helper function for manuscript-friendly p-values
format_p <- function(p) {
  case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

extract_aovlist_results <- function(aov_model, trait, model_scale) {
  
  model_summary <- summary(aov_model)
  
  out <- purrr::imap_dfr(model_summary, function(stratum, stratum_name) {
    
    tab <- as.data.frame(stratum[[1]])
    tab$effect <- rownames(tab)
    
    # Clean column names manually because aov summary columns contain spaces/symbols
    names(tab) <- gsub(" ", "_", names(tab))
    names(tab) <- gsub("Pr\\(>F\\)", "p_value", names(tab))
    names(tab) <- gsub("F_value", "F_value", names(tab))
    names(tab) <- gsub("Sum_Sq", "sum_sq", names(tab))
    names(tab) <- gsub("Mean_Sq", "mean_sq", names(tab))
    
    tab %>%
      filter(effect != "Residuals") %>%
      mutate(
        trait = trait,
        model_scale = model_scale,
        error_stratum = gsub("Error: ", "", stratum_name),
        p_formatted = format_p(p_value),
        significance = sig_code(p_value)
      ) %>%
      select(
        trait,
        model_scale,
        error_stratum,
        effect,
        Df,
        sum_sq,
        mean_sq,
        F_value,
        p_value,
        p_formatted,
        significance
      )
  })
  
  out
}

elemental_aov_table <- bind_rows(
  extract_aovlist_results(
    aov_log_N_complete,
    trait = "Foliar nitrogen",
    model_scale = "log(%N)"
  ),
  
  extract_aovlist_results(
    aov_log_CN_complete,
    trait = "Foliar C/N",
    model_scale = "log(C/N)"
  ),
  
  extract_aovlist_results(
    aov_c13_complete,
    trait = "Carbon isotope composition",
    model_scale = "c13"
  )
)

elemental_aov_table

elemental_aov_table_formatted <- elemental_aov_table %>%
  mutate(
    F_value = round(F_value, 3),
    sum_sq = round(sum_sq, 3),
    mean_sq = round(mean_sq, 3)
  ) %>%
  select(
    trait,
    model_scale,
    error_stratum,
    effect,
    Df,
    F_value,
    p_formatted,
    significance
  )

elemental_aov_table_formatted

write.csv(
  elemental_aov_table,
  "calculated_data/elemental_isotope_AOV_table_full.csv",
  row.names = FALSE
)

write.csv(
  elemental_aov_table_formatted,
  "calculated_data/elemental_isotope_AOV_table_formatted.csv",
  row.names = FALSE
)

##SteP: create potential data table----
# Make sure site/species are clean factors
elemental_trait_summary <- isotope_complete %>%
  group_by(site, species, week) %>%
  summarize(
    n = n(),
    
    mean_N = mean(nitro_perc, na.rm = TRUE),
    se_N = sd(nitro_perc, na.rm = TRUE) / sqrt(n),
    
    mean_CN = mean(CN_ratio, na.rm = TRUE),
    se_CN = sd(CN_ratio, na.rm = TRUE) / sqrt(n),
    
    mean_c13 = mean(c13, na.rm = TRUE),
    se_c13 = sd(c13, na.rm = TRUE) / sqrt(n),
    
    .groups = "drop"
  )

elemental_trait_summary

write.csv(
  elemental_trait_summary,
  "calculated_data/elemental_isotope_trait_summary_site_species_week.csv",
  row.names = FALSE
)

site_week_means <- isotope_complete %>%
  group_by(site, week) %>%
  summarize(
    n = n(),
    
    mean_N = mean(nitro_perc, na.rm = TRUE),
    se_N = sd(nitro_perc, na.rm = TRUE) / sqrt(n),
    
    mean_CN = mean(CN_ratio, na.rm = TRUE),
    se_CN = sd(CN_ratio, na.rm = TRUE) / sqrt(n),
    
    mean_c13 = mean(c13, na.rm = TRUE),
    se_c13 = sd(c13, na.rm = TRUE) / sqrt(n),
    
    .groups = "drop"
  )

site_week_means

write.csv(
  site_week_means,
  "calculated_data/elemental_isotope_site_week_means.csv",
  row.names = FALSE
)

species_week_means <- isotope_complete %>%
  group_by(species, week) %>%
  summarize(
    n = n(),
    
    mean_N = mean(nitro_perc, na.rm = TRUE),
    se_N = sd(nitro_perc, na.rm = TRUE) / sqrt(n),
    
    mean_CN = mean(CN_ratio, na.rm = TRUE),
    se_CN = sd(CN_ratio, na.rm = TRUE) / sqrt(n),
    
    mean_c13 = mean(c13, na.rm = TRUE),
    se_c13 = sd(c13, na.rm = TRUE) / sqrt(n),
    
    .groups = "drop"
  )

species_week_means

write.csv(
  species_week_means,
  "calculated_data/elemental_isotope_species_week_means.csv",
  row.names = FALSE
)



##Step: decisions table-----

elemental_analysis_decisions <- tibble(
  trait = c("Foliar nitrogen", "Foliar C/N", "Carbon isotope composition", "Foliar carbon"),
  variable = c("nitro_perc", "CN_ratio", "c13", "carbon_perc"),
  primary_model_object = c(
    "aov_log_N_complete",
    "aov_log_CN_complete",
    "aov_c13_complete",
    "not modeled as primary response"
  ),
  model_scale = c(
    "log(nitro_perc)",
    "log(CN_ratio)",
    "original c13 scale",
    "descriptive/QC only"
  ),
  figure_scale = c(
    "original %N",
    "original C/N",
    "original c13",
    "not emphasized"
  ),
  diagnostic_decision = c(
    "Log transformation improved residual normality and reduced heterogeneity",
    "Log transformation produced acceptable diagnostics",
    "Retained original isotope scale; diagnostics imperfect but Cook's distances low",
    "Used to calculate C/N ratio; not emphasized as standalone response"
  ),
  interpretation_focus = c(
    "site, species, week, site x week, species x week",
    "site, species, week, site x week, species x week",
    "species, week, site x week with weak corrected site follow-ups",
    "supporting variable only"
  )
)

elemental_analysis_decisions

write.csv(
  elemental_analysis_decisions,
  "manuscript_scripts/elemental_isotope_analysis_decisions.csv",
  row.names = FALSE
)

##Step: Did pit size affect foliar traits: NO-----

tree_metadata <- read_csv(
  "calculated_data/tree_metadata_derived.csv",
  show_col_types = FALSE
)

tree_metadata <- tree_metadata %>%
  mutate(
    site = factor(site),
    species = factor(species),
    replicate = factor(replicate),
    tree_id = interaction(site, species, replicate, drop = TRUE)
  )

tree_metadata$pit_size <- tree_metadata$pit_size_class


# Make sure join variables are the same type
isotope_data$site <- as.character(isotope_data$site)
isotope_data$species <- as.character(isotope_data$species)
isotope_data$replicate <- as.character(isotope_data$replicate)
isotope_data$tree_id <- as.character(isotope_data$tree_id)

tree_metadata$site <- as.character(tree_metadata$site)
tree_metadata$species <- as.character(tree_metadata$species)
tree_metadata$replicate <- as.character(tree_metadata$replicate)
tree_metadata$tree_id <- as.character(tree_metadata$tree_id)

# Optional safety step:
# Standardize tree_id separators in case one file uses c.d.3 and the other uses c-d-3
isotope_data$tree_id <- gsub("[.]", "-", isotope_data$tree_id)
tree_metadata$tree_id <- gsub("[.]", "-", tree_metadata$tree_id)

# Join
isotope_pit <- merge(
  isotope_data,
  tree_metadata[, c("site", "species", "replicate", "tree_id",
                    "pit_size", "pit_area_m2", "pit_volume_m3")],
  by = c("site", "species", "replicate", "tree_id"),
  all.x = TRUE
)

isotope_pit_c <- isotope_pit %>%
  filter(site == "c") %>%
  mutate(
    site = factor(site),
    species = factor(species),
    replicate = factor(replicate),
    tree_id = factor(tree_id),
    week_f = factor(week),
    pit_size = factor(pit_size, levels = c("small", "large"))
  )

# Basic Downtown audit
nrow(isotope_pit_c)

isotope_pit_c %>%
  count(species, pit_size)

isotope_pit_c %>%
  count(tree_id, week_f) %>%

# Identify complete Downtown trees
complete_trees_c <- isotope_pit_c %>%
  group_by(tree_id) %>%
  summarise(
    n_obs = n(),
    n_weeks = n_distinct(week_f),
    .groups = "drop"
  ) %>%
  filter(n_weeks == n_distinct(isotope_pit_c$week_f)) %>%
  pull(tree_id)

complete_trees_c

# Create complete-tree Downtown dataset
isotope_pit_c_complete <- isotope_pit_c %>%
  filter(tree_id %in% complete_trees_c) %>%
  droplevels()

# Final complete-tree audit
nrow(isotope_pit_c_complete)

isotope_pit_c_complete %>%
  count(species, pit_size)

isotope_pit_c_complete %>%
  count(tree_id, week_f) %>%
  print(n = Inf)

#run models
# Downtown-only pit-size repeated-measures AOVs

aov_pit_log_N <- aov(
  log_nitro_perc ~ pit_size * species * week_f +
    Error(tree_id / week_f),
  data = isotope_pit_c_complete
)

aov_pit_log_CN <- aov(
  log_CN_ratio ~ pit_size * species * week_f +
    Error(tree_id / week_f),
  data = isotope_pit_c_complete
)

aov_pit_c13 <- aov(
  c13 ~ pit_size * species * week_f +
    Error(tree_id / week_f),
  data = isotope_pit_c_complete
)

summary(aov_pit_log_N)
summary(aov_pit_log_CN)
summary(aov_pit_c13)
#no effect of pit size

## Step: Create a means table-----

element_primary_complete <- isotope_complete

# Downtown pit-size complete dataset from the pit-size workflow
element_pit_complete <- isotope_pit_c_complete

# Helper function for standardized trait means

make_trait_means <- function(data, trait_var, trait_label, trait_units,
                             analysis_name, group_vars) {
  
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_obs = n(),
      n_trees = n_distinct(tree_id),
      mean = mean(.data[[trait_var]], na.rm = TRUE),
      sd = sd(.data[[trait_var]], na.rm = TRUE),
      se = sd / sqrt(n_obs),
      ci95 = qt(0.975, df = n_obs - 1) * se,
      lower_ci95 = mean - ci95,
      upper_ci95 = mean + ci95,
      .groups = "drop"
    ) %>%
    mutate(
      analysis = analysis_name,
      trait = trait_var,
      trait_label = trait_label,
      trait_units = trait_units,
      .before = 1
    )
}

# Primary means: site × species × week
element_means_primary <- bind_rows(
  
  make_trait_means(
    data = element_primary_complete,
    trait_var = "nitro_perc",
    trait_label = "Foliar nitrogen",
    trait_units = "%",
    analysis_name = "Primary site × species × week",
    group_vars = c("site", "species", "week")
  ),
  
  make_trait_means(
    data = element_primary_complete,
    trait_var = "CN_ratio",
    trait_label = "Carbon to nitrogen ratio",
    trait_units = "unitless",
    analysis_name = "Primary site × species × week",
    group_vars = c("site", "species", "week")
  ),
  
  make_trait_means(
    data = element_primary_complete,
    trait_var = "c13",
    trait_label = "delta13C",
    trait_units = "per mil",
    analysis_name = "Primary site × species × week",
    group_vars = c("site", "species", "week")
  )
)
element_means_primary

# Downtown pit-size means: pit_size × species × week

element_means_pit <- bind_rows(
  
  make_trait_means(
    data = element_pit_complete,
    trait_var = "nitro_perc",
    trait_label = "Foliar nitrogen",
    trait_units = "%",
    analysis_name = "Downtown pit size × species × week",
    group_vars = c("site", "pit_size", "species", "week")
  ),
  
  make_trait_means(
    data = element_pit_complete,
    trait_var = "CN_ratio",
    trait_label = "Carbon to nitrogen ratio",
    trait_units = "unitless",
    analysis_name = "Downtown pit size × species × week",
    group_vars = c("site", "pit_size", "species", "week")
  ),
  
  make_trait_means(
    data = element_pit_complete,
    trait_var = "c13",
    trait_label = "delta13C",
    trait_units = "per mil",
    analysis_name = "Downtown pit size × species × week",
    group_vars = c("site", "pit_size", "species", "week")
  )
)

element_means_pit

# Combined elemental/isotope means table
element_means_all <- bind_rows(
  element_means_primary,
  element_means_pit
) %>%
  mutate(
    trait_group = "elemental_isotope",
    .before = 1
  )

element_means_all

element_means_display <- element_means_all %>%
  mutate(
    mean = round(mean, 3),
    sd = round(sd, 3),
    se = round(se, 3),
    ci95 = round(ci95, 3),
    lower_ci95 = round(lower_ci95, 3),
    upper_ci95 = round(upper_ci95, 3)
  )

element_means_display

write.csv(
  element_means_display,
  "calculated_data/element_isotope_trait_means_display.csv",
  row.names = FALSE
)
