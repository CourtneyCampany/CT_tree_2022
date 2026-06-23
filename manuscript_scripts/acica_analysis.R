library(tidyverse)
library(mgcv)

#plotting palette
species_cols_okabe <- c(
  "Dogwood"  = "#228833",
  "Hawthorn" = "#D55E00",
  "Maple"    = "#0072B2"
)

## Step: read and tidy gas exchange data ----

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
    week_f = factor(week),
    tree_id = interaction(site, species, replicate, sep = "_", drop = TRUE),
    CiCa = Ci / Ca,
    iWUE = if_else(gsw > 0, A / gsw, NA_real_)
  )

#cicca dataset
gx_cica <- gx %>%
  filter(
    !is.na(A),
    !is.na(gsw),
    !is.na(Ci),
    !is.na(Ca),
    gsw > 0,
    Ci > 0,
    Ca > 0
  ) %>%
  filter(
    CiCa > 0,
    CiCa < 1.2
  )

#Step: Make pooled plot

ggplot(gx_cica, aes(x = CiCa, y = A, color = species)) +
  geom_point(alpha = 0.55, size = 2) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 4),
    se = TRUE
  ) +
  facet_wrap(~ site) +
  scale_color_manual(values = species_cols_okabe) +
  labs(
    x = expression(C[i]/C[a]),
    y = expression(A~"(µmol CO"[2]*" m"^-2*" s"^-1*")"),
    color = "Species"
  ) +
  theme_classic()

##Step: compare linear vs nonlinear pooled A - Ci/Ca relationship ----

m_A_CiCa_linear_global <- gam(
  A ~ CiCa +
    species +
    site +
    s(tree_id, bs = "re"),
  data = gx_cica,
  method = "REML"
)

m_A_CiCa_gam_global <- gam(
  A ~ s(CiCa, k = 4) +
    species +
    site +
    s(tree_id, bs = "re"),
  data = gx_cica,
  method = "REML"
)

summary(m_A_CiCa_linear_global)
summary(m_A_CiCa_gam_global)

AIC(
  m_A_CiCa_linear_global,
  m_A_CiCa_gam_global
)

gam.check(m_A_CiCa_gam_global)

summary(m_A_CiCa_gam_global)

##is nonlinear pattern consistent across species

m_A_CiCa_gam_species <- gam(
  A ~ species +
    site +
    s(CiCa, by = species, k = 4) +
    s(tree_id, bs = "re"),
  data = gx_cica,
  method = "REML"
)

summary(m_A_CiCa_gam_species)

gam.check(m_A_CiCa_gam_species)

AIC(
  m_A_CiCa_gam_global,
  m_A_CiCa_gam_species
)


#plot

pred_A_CiCa_species <- gx_cica %>%
  group_by(site, species) %>%
  summarise(
    CiCa_min = min(CiCa, na.rm = TRUE),
    CiCa_max = max(CiCa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    CiCa = list(seq(CiCa_min, CiCa_max, length.out = 100))
  ) %>%
  unnest(CiCa) %>%
  ungroup() %>%
  mutate(
    tree_id = gx_cica$tree_id[1]
  )

pred_vals_CiCa <- predict(
  m_A_CiCa_gam_species,
  newdata = pred_A_CiCa_species,
  se.fit = TRUE,
  exclude = "s(tree_id)"
)

pred_A_CiCa_species <- pred_A_CiCa_species %>%
  mutate(
    fit = pred_vals_CiCa$fit,
    se = pred_vals_CiCa$se.fit,
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )


p_A_CiCa <- ggplot() +
  geom_point(
    data = gx_cica,
    aes(x = CiCa, y = A, color = species),
    alpha = 0.55,
    size = 2
  ) +
  geom_ribbon(
    data = pred_A_CiCa_species,
    aes(x = CiCa, ymin = lower, ymax = upper, fill = species),
    alpha = 0.12,
    color = NA
  ) +
  geom_line(
    data = pred_A_CiCa_species,
    aes(x = CiCa, y = fit, color = species),
    linewidth = 1
  ) +
  facet_wrap(~ site) +
  scale_color_manual(values = species_cols_okabe) +
  scale_fill_manual(values = species_cols_okabe) +
  labs(
    x = expression(C[i]/C[a]),
    y = expression(A~"(µmol CO"[2]*" m"^-2*" s"^-1*")"),
    color = "Species",
    fill = "Species"
  ) +
  theme_classic()

p_A_CiCa

#save for supplmental or reviewer comment
