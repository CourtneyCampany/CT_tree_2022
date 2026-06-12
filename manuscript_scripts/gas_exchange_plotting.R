library(tidyverse)
library(lubridate)
library(purrr)

##Step: read and tidy data for gas exchange-----

gx_raw <- read_csv("raw_data/gasexchange_master.csv",
  show_col_types = FALSE,
  name_repair = "unique"
)

gx <- gx_raw %>%
  transmute(
    site_code = site,
    species_code = species,
    replicate = as.integer(replicate),
    week = as.integer(week),
    obs = as.integer(obs),
    date_time = ymd_hms(date),
    
    site = factor(
      recode(site_code,
             c = "Downtown",
             p = "Park"),
      levels = c("Park", "Downtown")
    ),
    
    species = factor(
      recode(species_code,
             d = "Dogwood",
             h = "Hawthorn",
             m = "Maple"),
      levels = c("Dogwood", "Hawthorn", "Maple")
    ),
    
    A = as.numeric(A),            # µmol CO2 m-2 s-1
    gsw = as.numeric(gsw),        # mol H2O m-2 s-1
    E_mol = as.numeric(E),        # mol H2O m-2 s-1
    E = as.numeric(Emm),          # mmol H2O m-2 s-1
    
    Ca = as.numeric(Ca),
    Qin = as.numeric(Qin),
    VPDleaf = as.numeric(VPDleaf),
    Tleaf = as.numeric(TleafCnd),
    RHcham = as.numeric(RHcham),
    Flow = as.numeric(Flow)
  ) %>%
  mutate(
    week_f = factor(week),
    tree_id = interaction(site, species, replicate, sep = "_", drop = TRUE),
    iWUE = A / gsw
  )


##Step: Summary figures to examine data quality-----

gx_long <- gx %>%
  pivot_longer(
    cols = c(A, gsw, E),
    names_to = "response",
    values_to = "value"
  ) %>%
  mutate(
    response = factor(
      response,
      levels = c("A", "gsw", "E"),
      labels = c(
        expression(italic(A)~"("*mu*"mol CO"[2]~m^-2~s^-1*")"),
        expression(italic(g)[sw]~"(mol H"[2]*"O "~m^-2~s^-1*")"),
        expression(italic(E)~"(mmol H"[2]*"O "~m^-2~s^-1*")")
      )
    )
  )

gx_summary <- gx_long %>%
  group_by(site, species, week, response) %>%
  summarise(
    n = n(),
    mean = mean(value),
    se = sd(value) / sqrt(n),
    .groups = "drop"
  )

ggplot(
  gx_summary,
  aes(x = week, y = mean, color = species, group = species)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.12,
    linewidth = 0.45
  ) +
  facet_grid(response ~ site, scales = "free_y",
             labeller = label_parsed) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Measurement week",
    y = NULL,
    color = "Species"
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "plain")
  )


##Step: Data inspection and transformations-----

skewness <- function(x) {
  n <- length(x)
  s <- sd(x)
  n / ((n - 1) * (n - 2)) *
    sum(((x - mean(x)) / s)^3)
}

scale_check <- tibble(
  response = c(
    "A: raw",
    "gsw: raw",
    "gsw: square-root",
    "E: raw",
    "E: square-root"
  ),
  skewness = c(
    skewness(gx$A),
    skewness(gx$gsw),
    skewness(gx$gsw_sqrt),
    skewness(gx$E),
    skewness(gx$E_sqrt)
  )
)

scale_check
#gsw and E are often skewed, fixed by sqrt transformation, A is fine
gx <- gx %>%
  mutate(
    gsw_sqrt = sqrt(gsw),
    E_sqrt = sqrt(E),
    cell = interaction(site, species, week_f, drop = TRUE)
  )

distribution_data <- gx %>%
  transmute(
    `A: raw` = A,
    `gsw: raw` = gsw,
    `gsw: square-root` = gsw_sqrt,
    `E: raw` = E,
    `E: square-root` = E_sqrt
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "response",
    values_to = "value"
  )

ggplot(distribution_data, aes(x = value)) +
  geom_histogram(bins = 20, color = "white") +
  facet_wrap(~ response, scales = "free", ncol = 2) +
  labs(
    x = "Observed value",
    y = "Number of observations"
  ) +
  theme_classic(base_size = 11)


library(car)
#variances are equal
leveneTest(A ~ cell, data = gx)
leveneTest(gsw ~ cell, data = gx)
leveneTest(E ~ cell, data = gx)

leveneTest(gsw_sqrt ~ cell, data = gx)
leveneTest(E_sqrt ~ cell, data = gx)

##Step: Plotting and Repeated Measures for A-----

A_summary <- gx %>%
  group_by(site, species, week) %>%
  summarise(
    n = n(),
    mean_A = mean(A),
    se_A = sd(A) / sqrt(n),
    .groups = "drop"
  )

#repeated measure anova (strong site:species:week effect, p = 0.036194)
rm_A <- aov(
  A ~ site * species * week_f +
    Error(tree_id / week_f),
  data = gx
)

summary(rm_A)

# which species drive the interaction?
# Separate species-specific datasets
gx_dogwood <- gx %>%
  filter(species == "Dogwood") %>%
  droplevels()

gx_hawthorn <- gx %>%
  filter(species == "Hawthorn") %>%
  droplevels()

gx_maple <- gx %>%
  filter(species == "Maple") %>%
  droplevels()

# Species-specific repeated-measures models
rm_A_dogwood <- aov(
  A ~ site * week_f +
    Error(tree_id / week_f),
  data = gx_dogwood
)

rm_A_hawthorn <- aov(
  A ~ site * week_f +
    Error(tree_id / week_f),
  data = gx_hawthorn
)

rm_A_maple <- aov(
  A ~ site * week_f +
    Error(tree_id / week_f),
  data = gx_maple
)

summary(rm_A_dogwood) #week
summary(rm_A_hawthorn) #site:week
summary(rm_A_maple) #week

#larger interaction above driven by hawthorn

#Plot photosynthesis with site trajectories compared within species
A_summary_species <- gx %>%
  group_by(species, site, week) %>%
  summarise(
    n = n(),
    mean_A = mean(A),
    se_A = sd(A) / sqrt(n),
    .groups = "drop"
  )


#The Hawthorn interaction does not appear to indicate consistently lower photosynthesis downtown. 
#Instead, site differences change direction through time.

#Hawthorn follow up: How does the site interaction play out?
hawthorn_A_contrasts <- gx_hawthorn %>%
  group_by(week) %>%
  nest() %>%
  mutate(
    Downtown_mean = map_dbl(
      data,
      ~ mean(.x$A[.x$site == "Downtown"])
    ),
    Park_mean = map_dbl(
      data,
      ~ mean(.x$A[.x$site == "Park"])
    ),
    difference = Downtown_mean - Park_mean,
    p_raw = map_dbl(
      data,
      ~ t.test(
        A ~ site,
        data = .x,
        var.equal = TRUE
      )$p.value
    )
  ) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  ) %>%
  select(
    week,
    Downtown_mean,
    Park_mean,
    difference,
    p_raw,
    p_holm,
    significant
  )

hawthorn_A_contrasts
#after holm corrections (used for multiple comparisons) only week 2 differed

##Step: Final figure for photosynthesis

A_summary_species <- gx %>%
  group_by(species, site, week) %>%
  summarise(
    n = n(),
    mean_A = mean(A),
    se_A = sd(A) / sqrt(n),
    .groups = "drop"
  )

ggplot(
  A_summary_species,
  aes(x = week, y = mean_A, color = site, group = site)
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.2) +
  geom_errorbar(
    aes(
      ymin = mean_A - se_A,
      ymax = mean_A + se_A
    ),
    width = 0.12,
    linewidth = 0.45
  ) +
  facet_wrap(~ species, nrow = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Measurement week",
    y = expression(italic(A)~"("*mu*"mol CO"[2]~m^-2~s^-1*")"),
    color = "Site"
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "plain")
  )


##Figure if I want to point out week 2 for hawthorn
# A_asterisk <- A_summary_species %>%
#   filter(species == "Hawthorn", week == 2) %>%
#   summarise(
#     species = first(species),
#     week = first(week),
#     y = max(mean_A + se_A) + 1.5
#   )
# 
# ggplot(
#   A_summary_species,
#   aes(x = week, y = mean_A, color = site, group = site)
# ) +
#   geom_line(linewidth = 0.85) +
#   geom_point(size = 2.2) +
#   geom_errorbar(
#     aes(
#       ymin = mean_A - se_A,
#       ymax = mean_A + se_A
#     ),
#     width = 0.12,
#     linewidth = 0.45
#   ) +
#   geom_text(
#     data = A_asterisk,
#     aes(x = week, y = y, label = "*"),
#     inherit.aes = FALSE,
#     size = 5
#   ) +
#   facet_wrap(~ species, nrow = 1) +
#   scale_x_continuous(breaks = 1:10) +
#   labs(
#     x = "Measurement week",
#     y = expression(italic(A)~"("*mu*"mol CO"[2]~m^-2~s^-1*")"),
#     color = "Site"
#   ) +
#   theme_classic(base_size = 11) +
#   theme(
#     legend.position = "bottom",
#     strip.background = element_blank(),
#     strip.text = element_text(face = "plain")
#   )

##Step: Plotting and Repeated Measures for gsw-----

#raw data
rm_gsw <- aov(
  gsw ~ site * species * week_f +
    Error(tree_id / week_f),
  data = gx
)

summary(rm_gsw)

#sqrt transformed
rm_gsw_sqrt <- aov(
  gsw_sqrt ~ site * species * week_f +
    Error(tree_id / week_f),
  data = gx
)
summary(rm_gsw_sqrt) #site:species:week (p = .010)

#inspect species specific interaction models
gx_dogwood <- gx %>%
  filter(species == "Dogwood") %>%
  droplevels()

gx_hawthorn <- gx %>%
  filter(species == "Hawthorn") %>%
  droplevels()

gx_maple <- gx %>%
  filter(species == "Maple") %>%
  droplevels()

rm_gsw_dogwood <- aov(
  gsw_sqrt ~ site * week_f +
    Error(tree_id / week_f),
  data = gx_dogwood
)

rm_gsw_hawthorn <- aov(
  gsw_sqrt ~ site * week_f +
    Error(tree_id / week_f),
  data = gx_hawthorn
)

rm_gsw_maple <- aov(
  gsw_sqrt ~ site * week_f +
    Error(tree_id / week_f),
  data = gx_maple
)

summary(rm_gsw_dogwood) #week
summary(rm_gsw_hawthorn) #site x week
summary(rm_gsw_maple) #week

#holms correction
gsw_species_interaction_p <- c(
  Dogwood = 0.821011,
  Hawthorn = 0.00000983,
  Maple = 0.499982
)

p.adjust(gsw_species_interaction_p, method = "holm")
#again, hawthorn drives interaction
#inspect within hawthorn

hawthorn_gsw_contrasts <- gx_hawthorn %>%
  group_by(week) %>%
  nest() %>%
  mutate(
    Downtown_mean = map_dbl(
      data,
      ~ mean(.x$gsw[.x$site == "Downtown"])
    ),
    Park_mean = map_dbl(
      data,
      ~ mean(.x$gsw[.x$site == "Park"])
    ),
    difference = Downtown_mean - Park_mean,
    p_raw = map_dbl(
      data,
      ~ t.test(
        gsw_sqrt ~ site,
        data = .x,
        var.equal = TRUE
      )$p.value
    )
  ) %>%
  ungroup() %>%      # essential: adjust across all 10 weekly comparisons
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  ) %>%
  select(
    week,
    Downtown_mean,
    Park_mean,
    difference,
    p_raw,
    p_holm,
    significant
  )

hawthorn_gsw_contrasts
#same as before, on correction only week 2


gsw_summary <- gx %>%
  group_by(species, site, week) %>%
  summarise(
    n = n(),
    mean_gsw = mean(gsw),
    se_gsw = sd(gsw) / sqrt(n),
    .groups = "drop"
  )

ggplot(
  gsw_summary,
  aes(x = week, y = mean_gsw, color = site, group = site)
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.2) +
  geom_errorbar(
    aes(
      ymin = mean_gsw - se_gsw,
      ymax = mean_gsw + se_gsw
    ),
    width = 0.12,
    linewidth = 0.45
  ) +
  facet_wrap(~ species, nrow = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Measurement week",
    y = expression(italic(g)[sw]~"(mol H"[2]*"O "~m^-2~s^-1*")"),
    color = "Site"
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "plain")
  )
##Step: Plotting and Repeated Measures for E-----

rm_E <- aov(
  E ~ site * species * week_f +
    Error(tree_id / week_f),
  data = gx
)

rm_E_sqrt <- aov(
  E_sqrt ~ site * species * week_f +
    Error(tree_id / week_f),
  data = gx
)

summary(rm_E)
summary(rm_E_sqrt) #site x species x week p = 0.008

#examine interaction within species
rm_E_dogwood <- aov(
  E_sqrt ~ site * week_f +
    Error(tree_id / week_f),
  data = gx %>% filter(species == "Dogwood") %>% droplevels()
)

rm_E_hawthorn <- aov(
  E_sqrt ~ site * week_f +
    Error(tree_id / week_f),
  data = gx %>% filter(species == "Hawthorn") %>% droplevels()
)

rm_E_maple <- aov(
  E_sqrt ~ site * week_f +
    Error(tree_id / week_f),
  data = gx %>% filter(species == "Maple") %>% droplevels()
)

summary(rm_E_dogwood) #week
summary(rm_E_hawthorn) #site x week
summary(rm_E_maple) #week

#holm correction
E_species_interaction_p <- c(
  Dogwood = 0.7662825,
  Hawthorn = 0.000005675,
  Maple = 0.4390719
)

p.adjust(E_species_interaction_p, method = "holm")

#inspect within hawthorn
gx_hawthorn <- gx %>%
  filter(species == "Hawthorn") %>%
  droplevels()

hawthorn_E_contrasts <- gx_hawthorn %>%
  group_by(week) %>%
  nest() %>%
  mutate(
    Downtown_mean = map_dbl(
      data,
      ~ mean(.x$E[.x$site == "Downtown"])
    ),
    Park_mean = map_dbl(
      data,
      ~ mean(.x$E[.x$site == "Park"])
    ),
    difference = Downtown_mean - Park_mean,
    p_raw = map_dbl(
      data,
      ~ t.test(
        E_sqrt ~ site,
        data = .x,
        var.equal = TRUE
      )$p.value
    )
  ) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  ) %>%
  select(
    week,
    Downtown_mean,
    Park_mean,
    difference,
    p_raw,
    p_holm,
    significant
  )

hawthorn_E_contrasts #same conclusion (week 2 significant after correction)

#plot transpiration
E_summary <- gx %>%
  group_by(species, site, week) %>%
  summarise(
    n = n(),
    mean_E = mean(E),
    se_E = sd(E) / sqrt(n),
    .groups = "drop"
  )

ggplot(
  E_summary,
  aes(x = week, y = mean_E, color = site, group = site)
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.2) +
  geom_errorbar(
    aes(
      ymin = mean_E - se_E,
      ymax = mean_E + se_E
    ),
    width = 0.12,
    linewidth = 0.45
  ) +
  facet_wrap(~ species, nrow = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Measurement week",
    y = expression(italic(E)~"(mmol H"[2]*"O "~m^-2~s^-1*")"),
    color = "Site"
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "plain")
  )



##Step: Plotting and Repeated Measures for iWUE-----

gx <- gx %>%
  mutate(
    iWUE = A / gsw,       # primary derived response:
    # µmol CO2 mol-1 H2O
  )

#calculate data quality on new derived variable
skewness(gx$iWUE)

gx <- gx %>%
  mutate(
    cell = interaction(site, species, week_f, drop = TRUE)
  )

car::leveneTest(iWUE ~ cell, data = gx, center = median)

#repeated measures
rm_iWUE <- aov(
  iWUE ~ site * species * week_f +
    Error(tree_id / week_f),
  data = gx
)

summary(rm_iWUE) #site x species x week (p = .036)

#investigate species specific interaction models
gx_dogwood <- gx %>%
  filter(species == "Dogwood") %>%
  droplevels()

gx_hawthorn <- gx %>%
  filter(species == "Hawthorn") %>%
  droplevels()

gx_maple <- gx %>%
  filter(species == "Maple") %>%
  droplevels()

rm_iWUE_dogwood <- aov(
  iWUE ~ site * week_f +
    Error(tree_id / week_f),
  data = gx_dogwood
)

rm_iWUE_hawthorn <- aov(
  iWUE ~ site * week_f +
    Error(tree_id / week_f),
  data = gx_hawthorn
)

rm_iWUE_maple <- aov(
  iWUE ~ site * week_f +
    Error(tree_id / week_f),
  data = gx_maple
)

summary(rm_iWUE_dogwood) #site x week
summary(rm_iWUE_hawthorn) #site x week
summary(rm_iWUE_maple) #site x week

#holm correction
iWUE_species_interaction_p <- c(
  Dogwood = 0.007298994,
  Hawthorn = 0.000001785,
  Maple = 0.01861072
)

p.adjust(iWUE_species_interaction_p, method = "holm")

iWUE_week_contrasts <- gx %>%
  group_by(species, week) %>%
  nest() %>%
  mutate(
    Downtown_mean = map_dbl(
      data,
      ~ mean(.x$iWUE[.x$site == "Downtown"])
    ),
    Park_mean = map_dbl(
      data,
      ~ mean(.x$iWUE[.x$site == "Park"])
    ),
    difference = Downtown_mean - Park_mean,
    p_raw = map_dbl(
      data,
      ~ t.test(
        iWUE ~ site,
        data = .x,
        var.equal = TRUE
      )$p.value
    )
  ) %>%
  ungroup() %>%     # essential: correct across all 30 comparisons
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  ) %>%
  select(
    species,
    week,
    Downtown_mean,
    Park_mean,
    difference,
    p_raw,
    p_holm,
    significant
  ) %>%
  arrange(species, week)

iWUE_week_contrasts

#plot iWUE
iWUE_summary <- gx %>%
  group_by(species, site, week) %>%
  summarise(
    n = n(),
    mean_iWUE = mean(iWUE),
    se_iWUE = sd(iWUE) / sqrt(n),
    .groups = "drop"
  )

ggplot(
  iWUE_summary,
  aes(x = week, y = mean_iWUE, color = site, group = site)
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.2) +
  geom_errorbar(
    aes(
      ymin = mean_iWUE - se_iWUE,
      ymax = mean_iWUE + se_iWUE
    ),
    width = 0.12,
    linewidth = 0.45
  ) +
  facet_wrap(~ species, nrow = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Measurement week",
    y = expression(
      "Intrinsic WUE (" * mu * "mol CO"[2] ~ "mol"^-1 ~ "H"[2] * "O)"
    ),
    color = "Site"
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "plain")
  )


##possible stand-alone difference plot iWUE figure that better shows interaction
#values above zero indicating higher Downtown efficiency and values below zero indicating higher Park efficiency.

iWUE_difference <- gx %>%
  group_by(species, site, week) %>%
  summarise(
    mean_iWUE = mean(iWUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = site,
    values_from = mean_iWUE
  ) %>%
  mutate(
    difference = Downtown - Park
  )

ggplot(
  iWUE_difference,
  aes(x = week, y = difference, color = species, group = species)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Measurement week",
    y = expression(Delta*"iWUE (Downtown - Park; "*mu*"mol CO"[2]~"mol"^-1~"H"[2]*"O)"),
    color = "Species"
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom"
  )


##SteP: Create dataframe of model outputs

#confirm models are existing objects
summary(rm_A)
summary(rm_gsw_sqrt)
summary(rm_E_sqrt)
summary(rm_iWUE)


extract_rm_anova <- function(model, response_label) {
  
  model_summary <- summary(model)
  
  between <- as.data.frame(model_summary[["Error: tree_id"]][[1]]) %>%
    rownames_to_column("effect")
  
  within <- as.data.frame(model_summary[["Error: tree_id:week_f"]][[1]]) %>%
    rownames_to_column("effect")
  
  bind_rows(between, within) %>%
    filter(!str_detect(effect, "Residuals")) %>%
    transmute(
      response = response_label,
      effect = str_trim(effect),
      df = Df,
      F_value = `F value`,
      p_value = `Pr(>F)`
    )
}

gasexchange_anova_table <- bind_rows(
  extract_rm_anova(rm_A, "Photosynthesis (A)"),
  extract_rm_anova(rm_gsw_sqrt, "Stomatal conductance (sqrt gsw)"),
  extract_rm_anova(rm_E_sqrt, "Transpiration (sqrt E)"),
  extract_rm_anova(rm_iWUE, "Intrinsic WUE (iWUE)")
) %>%
  mutate(
    effect = dplyr::case_when(
      effect == "site" ~ "Site",
      effect == "species" ~ "Species",
      effect == "week_f" ~ "Week",
      effect == "site:species" ~ "Site × Species",
      effect == "site:week_f" ~ "Site × Week",
      effect == "species:week_f" ~ "Species × Week",
      effect == "site:species:week_f" ~ "Site × Species × Week",
      TRUE ~ effect
    ),
    p_report = dplyr::case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    ),
    F_report = sprintf("%.2f", F_value)
  ) %>%
  select(response, effect, df, F_report, p_report)

gasexchange_anova_table




##Step: Possible integrated figure-----

gasexchange_long <- gx %>%
  select(site, species, week, A, gsw, E, iWUE) %>%
  pivot_longer(
    cols = c(A, gsw, E, iWUE),
    names_to = "response",
    values_to = "value"
  ) %>%
  mutate(
    response = factor(
      response,
      levels = c("A", "gsw", "E", "iWUE"),
      labels = c(
        "Photosynthesis, A",
        "Stomatal conductance, gsw",
        "Transpiration, E",
        "Intrinsic water-use efficiency, iWUE"
      )
    )
  )

gasexchange_summary <- gasexchange_long %>%
  group_by(response, species, site, week) %>%
  summarise(
    n = n(),
    mean = mean(value),
    se = sd(value) / sqrt(n),
    .groups = "drop"
  )

ggplot(
  gasexchange_summary,
  aes(x = week, y = mean, color = site, group = site)
) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.8) +
  geom_errorbar(
    aes(
      ymin = mean - se,
      ymax = mean + se
    ),
    width = 0.12,
    linewidth = 0.35
  ) +
  facet_grid(response ~ species, scales = "free_y") +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Measurement week",
    y = NULL,
    color = "Site"
  ) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "plain"),
    panel.spacing = unit(0.8, "lines")
  )


##Step: Write key outputs------

write_csv(gasexchange_anova_table,
          "calculated_data/gasexchange_primary_rm_anova_summary.csv"
)

write_csv(gasexchange_summary,
          "calculated_data/gasexchange_weekly_summary_means_SE.csv"
)

write_csv(iWUE_difference,
          "calculated_data/iWUE_Downtown_minus_Park_by_species_week.csv"
)