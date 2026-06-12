library(tidyverse)
library(mgcv)

##Step: read and tidy data for gas exchange-----

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
    
    A = as.numeric(A),            # µmol CO2 m-2 s-1
    gsw = as.numeric(gsw),        # mol H2O m-2 s-1
    E_mol = as.numeric(E),        # mol H2O m-2 s-1
    E = as.numeric(Emm),          # mmol H2O m-2 s-1
    Ci = as.numeric(Ci),
    Ca = as.numeric(Ca)
  ) %>%
  mutate(
    week_f = factor(week),
    tree_id = interaction(site, species, replicate, sep = "_", drop = TRUE),
    CiCa = Ci / Ca,
    iWUE = if_else(gsw > 0, A / gsw, NA_real_)
  )

## Step: Are there any questionable observations?-----

gx_flags <- gx %>%
  filter(
    is.na(A) | is.na(gsw) | is.na(Ci) | is.na(Ca) |
      gsw <= 0 | Ca <= 0 | Ci <= 0 |
      CiCa <= 0 | CiCa > 1.2
  ) %>%
  select(tree_id, site, species, replicate, week, obs, A, gsw, Ci, Ca, CiCa, iWUE)

gx_flags ##good to go

## Step: create gas-exchange relationship dataset ----

gx_rel <- gx %>%
  filter(
    !is.na(A),
    !is.na(gsw),
    !is.na(Ci),
    !is.na(Ca),
    gsw > 0,
    Ci > 0,
    Ca > 0
  ) %>%
  mutate(
    CiCa = Ci / Ca,
    site_species = interaction(site, species, sep = "_", drop = TRUE)
  )

## Step: Test models for A-gs, linear vs gam for all poole data-------

## first summarize gsw and A range by site × species ----
#linear” behavior may reflect a narrower range of low gsw values.
#   gx_range_summary <- gx_rel %>%
#   group_by(site, species) %>%
#   summarise(
#     n_obs = n(),
#     n_trees = n_distinct(tree_id),
#     n_weeks = n_distinct(week),
#     gsw_min = min(gsw, na.rm = TRUE),
#     gsw_median = median(gsw, na.rm = TRUE),
#     gsw_max = max(gsw, na.rm = TRUE),
#     gsw_range = gsw_max - gsw_min,
#     A_min = min(A, na.rm = TRUE),
#     A_median = median(A, na.rm = TRUE),
#     A_max = max(A, na.rm = TRUE),
#     A_range = A_max - A_min,
#     .groups = "drop"
#   ) %>%
#   arrange(species, site)
# 
# gx_range_summary

#visual pooled (all treatments) A - gsw plot with one overall GAM smoother ----

ggplot(gx_rel, aes(x = gsw, y = A)) +
  geom_point(aes(color = species, shape = site), alpha = 0.65, size = 2) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 4), #k=4 allows curve, not to much wiggle
    se = TRUE
  ) +
  labs(
    x = expression(g[s]~"(mol H"[2]*"O m"^-2*" s"^-1*")"),
    y = expression(A~"(µmol CO"[2]*" m"^-2*" s"^-1*")")
  ) +
  theme_classic()

#All pooled model gam
m_A_gsw_gam_global <- gam(
  A ~ s(gsw, k = 4) +
    species +
    site +
    s(tree_id, bs = "re"),
  data = gx_rel,
  method = "REML"
)

summary(m_A_gsw_gam_global)
gam.check(m_A_gsw_gam_global)

#All pooled model linear
m_A_gsw_linear_global <- gam(
  A ~ gsw +
    species +
    site +
    s(tree_id, bs = "re"),
  data = gx_rel,
  method = "REML"
)

summary(m_A_gsw_linear_global)

#compare models with AIC
AIC(m_A_gsw_linear_global, m_A_gsw_gam_global)
##gam model has a lower AIC
#smooth edf for s(gsw) is 2.8 = curve model appropriate instead of linear

#next question, was the GAM model to constrained? 
gam.check(m_A_gsw_gam_global) # p value is not low, k index = 1, keep k =4 smoother

#The pooled A–gsw relationship is clearly nonlinear, and the GAM basis is adequate.
#A straight line is not sufficient for the pooled data.


##Step: is non linear relationship in A-gs, shared across species?
## range in gs varies, so curve could be because of one species

m_A_gsw_gam_species <- gam(
  A ~ species +
    site +
    s(gsw, by = species, k = 4) +
    s(tree_id, bs = "re"),
  data = gx_rel,
  method = "REML"
)

summary(m_A_gsw_gam_species)
gam.check(m_A_gsw_gam_species)
#edfs between 1.7 and 2.7 show relationships are somewhat- highly curved by species
#downtown effect is only marginal, after accounting for species (slighly slower A)

#should i report the pooled model or the species specific model?
AIC(m_A_gsw_gam_global, m_A_gsw_gam_species)
#they are very similar, species a little lower'
##Report the species specific model


#should we add site differences in the smooth factor?
m_A_gsw_gam_site_species <- gam(
  A ~ site_species +
    s(gsw, by = site_species, k = 4) +
    s(tree_id, bs = "re"),
  data = gx_rel,
  method = "REML"
)

summary(m_A_gsw_gam_site_species)
gam.check(m_A_gsw_gam_site_species)

AIC(
  m_A_gsw_gam_global,
  m_A_gsw_gam_species,
  m_A_gsw_gam_site_species
)

#The site × species GAM is the lowest-AIC model

#is the more complicated model because of unequal ranges in gsw?
gx_rel %>%
  group_by(site, species) %>%
  summarise(
    n_obs = n(),
    n_trees = n_distinct(tree_id),
    gsw_min = min(gsw, na.rm = TRUE),
    gsw_q25 = quantile(gsw, 0.25, na.rm = TRUE),
    gsw_median = median(gsw, na.rm = TRUE),
    gsw_q75 = quantile(gsw, 0.75, na.rm = TRUE),
    gsw_max = max(gsw, na.rm = TRUE),
    gsw_range = gsw_max - gsw_min,
    A_min = min(A, na.rm = TRUE),
    A_median = median(A, na.rm = TRUE),
    A_max = max(A, na.rm = TRUE),
    A_range = A_max - A_min,
    .groups = "drop"
  ) %>%
  arrange(species, site)

#Park hawthorn had strong curvature and a broad gsw range.
#Downtown hawthorn had an even broader gsw range, but lower curvature.
#So the hawthorn site difference is probably not just a range artifact. 
#It may reflect a real difference in how A responded to stomatal conductance between Park and Downtown

plot(
  m_A_gsw_gam_site_species,
  pages = 1,
  shade = TRUE,
  residuals = TRUE,
  pch = 16,
  cex = 0.7
)
#it is pretty clear that the site x species gam is too flexible, group specific 
#maple curve range limited (high end cannot be interpreted), same for dogowdd downtown

gx_rel %>%
  filter(site == "Park", species == "Hawthorn") %>%
  arrange(desc(gsw)) %>%
  select(tree_id, replicate, week, obs, A, gsw, Ci, Ca, CiCa, E, iWUE) %>%
  slice_head(n = 15)

#one really high hawthorwn points alters shape

gx_rel_sens <- gx_rel %>%
  filter(!(tree_id == "Park_Hawthorn_4" & week == 7 & obs == 11))

m_A_gsw_gam_site_species_sens <- gam(
  A ~ site_species +
    s(gsw, by = site_species, k = 4) +
    s(tree_id, bs = "re"),
  data = gx_rel_sens,
  method = "REML"
)

summary(m_A_gsw_gam_site_species_sens)
#does not alter the shape enough to make a change (edf values)

#summary, keep species only gam


##Step: Make plot that visualizes species preferred GAM model-----

##make sure predictions for curves are restricted to species datasets
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

pred_vals <- predict(
  m_A_gsw_gam_species,
  newdata = pred_A_gsw_species,
  se.fit = TRUE,
  exclude = "s(tree_id)"
)

pred_A_gsw_species <- pred_A_gsw_species %>%
  mutate(
    fit = pred_vals$fit,
    se = pred_vals$se.fit,
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )


#choose color scheme

library(RColorBrewer)

species_cols_okabe <- c(
  "Dogwood"  = "#228833",  # green
  "Hawthorn" = "#D55E00",  # vermillion
  "Maple"    = "#0072B2"   # blue
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

p_A_gsw
  
  ggsave(
    filename = "figures/A_gsw_species_GAM.png",
    plot = p_A_gsw,
    width = 7,
    height = 4.5,
    units = "in",
    dpi = 600
  )

#non color backup
# 
# ggplot() +
#   geom_point(
#     data = gx_rel,
#     aes(x = gsw, y = A, shape = species),
#     color = "black",
#     alpha = 0.60,
#     size = 2
#   ) +
#   geom_ribbon(
#     data = pred_A_gsw_species,
#     aes(x = gsw, ymin = lower, ymax = upper, fill = species),
#     alpha = 0.18,
#     color = NA
#   ) +
#   geom_line(
#     data = pred_A_gsw_species,
#     aes(x = gsw, y = fit, linetype = species),
#     color = "black",
#     linewidth = 1
#   ) +
#   facet_wrap(~ site) +
#   scale_shape_manual(
#     values = c(
#       "Dogwood" = 16,
#       "Hawthorn" = 17,
#       "Maple" = 15
#     )
#   ) +
#   scale_linetype_manual(
#     values = c(
#       "Dogwood" = "solid",
#       "Hawthorn" = "longdash",
#       "Maple" = "dotdash"
#     )
#   ) +
#   scale_fill_grey(
#     start = 0.35,
#     end = 0.80
#   ) +
#   labs(
#     x = expression(g[s]~"(mol H"[2]*"O m"^-2*" s"^-1*")"),
#     y = expression(A~"(µmol CO"[2]*" m"^-2*" s"^-1*")"),
#     shape = "Species",
#     linetype = "Species",
#     fill = "Species"
#   ) +
#   theme_classic()

