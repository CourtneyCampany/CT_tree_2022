library(tidyverse)

##Step: Read, wrangle, and check structure of health survey-----

tree_health_raw <- read.csv("raw_data/final_tree_health.csv")

tree_health <- tree_health_raw %>%
  mutate(
    site = recode(
      site,
      "c" = "Downtown",
      "p" = "Park"
    ),
    species = recode(
      species,
      "d" = "dogwood",
      "h" = "hawthorn",
      "m" = "maple"
    ),
    site = factor(site, levels = c("Park", "Downtown")),
    species = factor(species, levels = c("dogwood", "maple", "hawthorn")),
    
    # Exploratory summary score only.
    # Lower values indicate healthier trees.
    visual_stress_sum =
      crown_discoloration + twig_dieback + crown_vigor,
    
    visual_stress_mean =
      visual_stress_sum / 3
  )

health_traits <- c(
  "crown_discoloration",
  "twig_dieback",
  "crown_vigor",
  "visual_stress_mean"
)

#means of health traits
health_species_summary <- tree_health %>%
  group_by(species) %>%
  summarise(
    n = n(),
    
    crown_discoloration_mean = mean(crown_discoloration),
    crown_discoloration_sd = sd(crown_discoloration),
    
    twig_dieback_mean = mean(twig_dieback),
    twig_dieback_sd = sd(twig_dieback),
    
    crown_vigor_mean = mean(crown_vigor),
    crown_vigor_sd = sd(crown_vigor),
    
    visual_stress_mean = mean(visual_stress_mean),
    visual_stress_sd = sd(visual_stress_mean),
    
    .groups = "drop"
  )

health_site_summary <- tree_health %>%
  group_by(site) %>%
  summarise(
    n = n(),
    
    crown_discoloration_mean = mean(crown_discoloration),
    twig_dieback_mean = mean(twig_dieback),
    crown_vigor_mean = mean(crown_vigor),
    visual_stress_mean = mean(visual_stress_mean),
    
    .groups = "drop"
  )

health_site_species_summary <- tree_health %>%
  group_by(site, species) %>%
  summarise(
    n = n(),
    
    crown_discoloration_mean = mean(crown_discoloration),
    twig_dieback_mean = mean(twig_dieback),
    crown_vigor_mean = mean(crown_vigor),
    visual_stress_mean = mean(visual_stress_mean),
    
    .groups = "drop"
  )

health_species_summary
health_site_summary
health_site_species_summary

##Step: Kruskal_Wallis test ------
# These are appropriate preliminary tests because the health
# ratings are ordinal and contain many tied values.

run_kruskal <- function(data, response, predictor) {
  f <- as.formula(paste(response, "~", predictor))
  test <- kruskal.test(f, data = data)
  
  tibble(
    response = response,
    predictor = predictor,
    statistic = unname(test$statistic),
    df = unname(test$parameter),
    p_value = test$p.value
  )
}

health_species_tests <- map_dfr(
  health_traits,
  ~ run_kruskal(tree_health, response = .x, predictor = "species")
) %>%
  mutate(
    p_value = signif(p_value, 4),
    significant = if_else(p_value < 0.05, "Yes", "No")
  )

health_site_tests <- map_dfr(
  health_traits,
  ~ run_kruskal(tree_health, response = .x, predictor = "site")
) %>%
  mutate(
    p_value = signif(p_value, 4),
    significant = if_else(p_value < 0.05, "Yes", "No")
  )

health_species_tests #twig dieback, marginal crown discolor
health_site_tests #nothing by site

##Step: Pairwise species comparisons------
# Pairwise Wilcoxon rank-sum tests with Holm correction.
# These are useful for traits with a species-level signal

run_pairwise_wilcox <- function(data, response, group_var = "species") {
  
  group_levels <- levels(droplevels(data[[group_var]]))
  comparisons <- combn(group_levels, 2, simplify = FALSE)
  
  map_dfr(comparisons, function(comp) {
    
    group1 <- comp[1]
    group2 <- comp[2]
    
    x <- data %>%
      filter(.data[[group_var]] == group1) %>%
      pull(.data[[response]])
    
    y <- data %>%
      filter(.data[[group_var]] == group2) %>%
      pull(.data[[response]])
    
    test <- wilcox.test(
      x,
      y,
      exact = FALSE,
      correct = TRUE
    )
    
    tibble(
      response = response,
      group1 = group1,
      group2 = group2,
      statistic = unname(test$statistic),
      p_raw = test$p.value
    )
  }) %>%
    mutate(
      p_holm = p.adjust(p_raw, method = "holm"),
      significant = if_else(p_holm < 0.05, "Yes", "No")
    )
}

health_species_pairwise <- map_dfr(
  health_traits,
  ~ run_pairwise_wilcox(tree_health, response = .x)
) %>%
  mutate(
    p_raw = signif(p_raw, 4),
    p_holm = signif(p_holm, 4)
  )

health_species_pairwise
#after Holm - hawthorn lower than dogwood/maple for twig dieback

##Step: Focused table of results-----

health_key_species_results <- health_species_tests %>%
  filter(response %in% c("twig_dieback", "visual_stress_mean"))

health_key_pairwise_results <- health_species_pairwise %>%
  filter(response %in% c("twig_dieback", "visual_stress_mean"))

health_key_species_results
health_key_pairwise_results

##Step: Figure -----

fig_final_health_species <- ggplot(
  tree_health,
  aes(x = species, y = visual_stress_mean)
) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_jitter(width = 0.08, height = 0, size = 2, alpha = 0.75) +
  labs(
    x = "Species",
    y = "Final visual stress score",
    title = "Final visual tree health by species"
  ) +
  theme_classic(base_size = 12)

fig_final_health_species



