library(tidyverse)
library(mgcv)
library(patchwork)

## Preferred manuscript palette ----

species_cols_okabe <- c(
  "Dogwood"  = "#228833",
  "Hawthorn" = "#D55E00",
  "Maple"    = "#0072B2"
)

## Step: read gas exchange and isotope/nitrogen data ----

gx_raw <- read_csv(
  "raw_data/gasexchange_master.csv",
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
    
    A = as.numeric(A),
    gsw = as.numeric(gsw),
    E = as.numeric(Emm),
    Ci = as.numeric(Ci),
    Ca = as.numeric(Ca)
  ) %>%
  mutate(
    tree_id = interaction(site, species, replicate, sep = "_", drop = TRUE),
    CiCa = Ci / Ca,
    iWUE = if_else(gsw > 0, A / gsw, NA_real_)
  )

iso_raw <- read_csv(
  "raw_data/isotope_data.csv",
  show_col_types = FALSE,
  name_repair = "unique"
)

iso <- iso_raw %>%
  transmute(
    site_code = site,
    species_code = species,
    replicate = as.integer(replicate),
    week = as.integer(week),
    sample_ID = sample_ID,
    
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
    
    leaf_N = as.numeric(nitro_perc),
    leaf_C = as.numeric(carbon_perc),
    C_N = leaf_C / leaf_N,
    d15N = as.numeric(n15),
    d13C = as.numeric(c13)
  ) %>%
  mutate(
    tree_id = interaction(site, species, replicate, sep = "_", drop = TRUE)
  )

##Step: Merge  2 datasets-----
gx_N <- gx %>%
  inner_join(
    iso %>%
      select(site, species, replicate, week, tree_id, leaf_N, leaf_C, C_N, d15N, d13C),
    by = c("site", "species", "replicate", "week", "tree_id")
  ) %>%
  filter(
    !is.na(A),
    !is.na(leaf_N),
    leaf_N > 0
  )

gx_N %>%
  summarise(
    n_obs = n(),
    n_trees = n_distinct(tree_id),
    min_week = min(week, na.rm = TRUE),
    max_week = max(week, na.rm = TRUE),
    leaf_N_min = min(leaf_N, na.rm = TRUE),
    leaf_N_median = median(leaf_N, na.rm = TRUE),
    leaf_N_max = max(leaf_N, na.rm = TRUE),
    A_min = min(A, na.rm = TRUE),
    A_median = median(A, na.rm = TRUE),
    A_max = max(A, na.rm = TRUE)
  )

#diagnostic plot

ggplot(gx_N, aes(x = leaf_N, y = A, color = species)) +
  geom_point(alpha = 0.55, size = 2) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  facet_wrap(~ site) +
  scale_color_manual(values = species_cols_okabe) +
  labs(
    x = "Leaf nitrogen (%)",
    y = expression(A~"(µmol CO"[2]*" m"^-2*" s"^-1*")"),
    color = "Species"
  ) +
  theme_classic()

#does not show a lot of relationships

##Step: convert nitrogen to area basis----------

lma_raw <- read_csv(
  "raw_data/leafthick.csv",
  show_col_types = FALSE,
  name_repair = "unique"
)

lma <- lma_raw %>%
  transmute(
    site_code = site,
    species_code = species,
    replicate = as.integer(replicate),
    week = as.integer(week),
    
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
    
    drymass_g = as.numeric(drymass_g),
    area_cm2 = as.numeric(area_cm2),
    area_m2 = area_cm2 / 10000,
    LMA = drymass_g / area_m2
  ) %>%
  mutate(
    tree_id = interaction(site, species, replicate, sep = "_", drop = TRUE)
  )

#merge datasets
gx_N_area <- gx_N %>%
  inner_join(
    lma %>%
      select(site, species, replicate, week, tree_id, LMA),
    by = c("site", "species", "replicate", "week", "tree_id")
  ) %>%
  mutate(
    Narea = (leaf_N / 100) * LMA
  ) %>%
  filter(
    !is.na(A),
    !is.na(Narea),
    Narea > 0
  )

##Step: Does area-based N explain A better than the model without N?------

m_A_Narea_noN <- gam(
  A ~ species +
    site +
    s(tree_id, bs = "re"),
  data = gx_N_area,
  method = "REML"
)

m_A_Narea_linear <- gam(
  A ~ Narea +
    species +
    site +
    s(tree_id, bs = "re"),
  data = gx_N_area,
  method = "REML"
)

summary(m_A_Narea_linear)

AIC(
  m_A_Narea_noN,
  m_A_Narea_linear
)

#without nitrogen
m_A_Narea_species_int <- gam(
  A ~ Narea * species +
    site +
    s(tree_id, bs = "re"),
  data = gx_N_area,
  method = "REML"
)

summary(m_A_Narea_species_int)

AIC(
  m_A_Narea_noN,
  m_A_Narea_linear,
  m_A_Narea_species_int
)

#moving forward with area based

#model checks
plot(m_A_Narea_linear)

gx_N_area$resid_A_Narea <- residuals(m_A_Narea_linear)

ggplot(gx_N_area, aes(x = Narea, y = resid_A_Narea, color = species)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = species_cols_okabe) +
  labs(
    x = expression(N[area]~"(g N m"^-2*")"),
    y = "Model residuals",
    color = "Species"
  ) +
  theme_classic()

##Step: Manuscript figure for the A- N model-------

#predictions for CIs
pred_A_Narea <- gx_N_area %>%
  group_by(site, species) %>%
  summarise(
    Narea_min = min(Narea, na.rm = TRUE),
    Narea_max = max(Narea, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Narea = list(seq(Narea_min, Narea_max, length.out = 100))
  ) %>%
  unnest(Narea) %>%
  ungroup() %>%
  mutate(
    tree_id = gx_N_area$tree_id[1]
  )

pred_vals_Narea <- predict(
  m_A_Narea_linear,
  newdata = pred_A_Narea,
  se.fit = TRUE,
  exclude = "s(tree_id)"
)

pred_A_Narea <- pred_A_Narea %>%
  mutate(
    fit = pred_vals_Narea$fit,
    se = pred_vals_Narea$se.fit,
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

p_A_Narea <- ggplot() +
  geom_point(
    data = gx_N_area,
    aes(x = Narea, y = A, color = species),
    alpha = 0.55,
    size = 2
  ) +
  geom_ribbon(
    data = pred_A_Narea,
    aes(x = Narea, ymin = lower, ymax = upper, fill = species),
    alpha = 0.12,
    color = NA
  ) +
  geom_line(
    data = pred_A_Narea,
    aes(x = Narea, y = fit, color = species),
    linewidth = 1
  ) +
  facet_wrap(~ site) +
  scale_color_manual(values = species_cols_okabe) +
  scale_fill_manual(values = species_cols_okabe) +
  labs(
    x = expression(N[area]~"(g N m"^-2*")"),
    y = expression(A~"(µmol CO"[2]*" m"^-2*" s"^-1*")"),
    color = "Species",
    fill = "Species"
  ) +
  theme_classic()

p_A_Narea

ggsave(
  filename = "figures/A_Narea_relationship.png",
  plot = p_A_Narea,
  width = 7,
  height = 4.5,
  units = "in",
  dpi = 600
)

## Step: two panel ecophys bivariate figure-----

#read in gas exchange to remake that figure
gx_raw <- read_csv(
  "raw_data/gasexchange_master.csv",
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
    
    A = as.numeric(A),
    gsw = as.numeric(gsw),
    E = as.numeric(Emm),
    Ci = as.numeric(Ci),
    Ca = as.numeric(Ca)
  ) %>%
  mutate(
    tree_id = interaction(site, species, replicate, sep = "_", drop = TRUE),
    CiCa = Ci / Ca,
    iWUE = if_else(gsw > 0, A / gsw, NA_real_)
  )

gx_rel <- gx %>%
  filter(
    !is.na(A),
    !is.na(gsw),
    !is.na(Ci),
    !is.na(Ca),
    gsw > 0,
    Ci > 0,
    Ca > 0
  )

#model (validated in Ags script)
m_A_gsw_gam_species <- gam(
  A ~ species +
    site +
    s(gsw, by = species, k = 4) +
    s(tree_id, bs = "re"),
  data = gx_rel,
  method = "REML"
)

#predict for CIs
pred_A_gsw_species <- gx_rel %>%
  group_by(site, species) %>%
  summarise(
    gsw_min = min(gsw, na.rm = TRUE),
    gsw_max = max(gsw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    gsw = list(seq(gsw_min, gsw_max, length.out = 100))
  ) %>%
  unnest(gsw) %>%
  ungroup() %>%
  mutate(
    tree_id = gx_rel$tree_id[1]
  )

pred_vals_gsw <- predict(
  m_A_gsw_gam_species,
  newdata = pred_A_gsw_species,
  se.fit = TRUE,
  exclude = "s(tree_id)"
)

pred_A_gsw_species <- pred_A_gsw_species %>%
  mutate(
    fit = pred_vals_gsw$fit,
    se = pred_vals_gsw$se.fit,
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )



#y axis range set
A_ylim <- c(
  0,
  ceiling(max(c(gx_rel$A, gx_N_area$A), na.rm = TRUE))
)


p_A_gsw <- ggplot() +
  geom_point(
    data = gx_rel,
    aes(x = gsw, y = A, color = species),
    alpha = 0.55,
    size = 2
  ) +
  geom_ribbon(
    data = pred_A_gsw_species,
    aes(x = gsw, ymin = lower, ymax = upper, fill = species),
    alpha = 0.12,
    color = NA
  ) +
  geom_line(
    data = pred_A_gsw_species,
    aes(x = gsw, y = fit, color = species),
    linewidth = 1
  ) +
  facet_wrap(~ site) +
  scale_color_manual(values = species_cols_okabe) +
  scale_fill_manual(values = species_cols_okabe) +
  labs(
    x = expression(g[s]~"(mol H"[2]*"O m"^-2*" s"^-1*")"),
    y = expression(A~"(µmol CO"[2]*" m"^-2*" s"^-1*")"),
    color = "Species",
    fill = "Species"
  ) +
  theme_classic()

p_A_Narea <- ggplot() +
  geom_point(
    data = gx_N_area,
    aes(x = Narea, y = A, color = species),
    alpha = 0.55,
    size = 2
  ) +
  geom_ribbon(
    data = pred_A_Narea,
    aes(x = Narea, ymin = lower, ymax = upper, fill = species),
    alpha = 0.12,
    color = NA
  ) +
  geom_line(
    data = pred_A_Narea,
    aes(x = Narea, y = fit, color = species),
    linewidth = 1
  ) +
  facet_wrap(~ site) +
  scale_color_manual(values = species_cols_okabe) +
  scale_fill_manual(values = species_cols_okabe) +
  labs(
    x = expression(N[area]~"(g N m"^-2*")"),
    y = expression(A~"(µmol CO"[2]*" m"^-2*" s"^-1*")"),
    color = "Species",
    fill = "Species"
  ) +
  theme_classic()

#combined and save
p_A_gsw_panel <- p_A_gsw +
  coord_cartesian(ylim = A_ylim) +
  guides(fill = "none") +
  theme(
    legend.position = "bottom"
  )

p_A_Narea_panel <- p_A_Narea +
  coord_cartesian(ylim = A_ylim) +
  guides(fill = "none") +
  theme(
    legend.position = "bottom"
  )

p_A_trait_combo <- p_A_gsw_panel / p_A_Narea_panel +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom"
  )

windows()
p_A_trait_combo

ggsave(
  filename = "figures/A_trait_relationships_combo.png",
  plot = p_A_trait_combo,
  width = 7.5,
  height = 8.5,
  units = "in",
  dpi = 600
)
