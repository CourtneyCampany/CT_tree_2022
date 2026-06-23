library(tidyverse)
library(patchwork)

##Step: Read and wrangle the 3 datasets for Figure 2------

dbh_raw <- read.csv("raw_data/dbh_clean.csv")

gas_raw <- read.csv("raw_data/gasexchange_master_clean.csv")

wp_raw <- read.csv("raw_data/water_potentials.csv")
  
# ---- Standardize DBH data
  dbh_dat <- dbh_raw %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      site = recode(site,
                    "c" = "Downtown",
                    "p" = "Park"),
      species = recode(species,
                       "d" = "Dogwood",
                       "h" = "Hawthorn",
                       "m" = "Maple"),
      tree_id = paste(site, species, replicate, sep = "_")
    ) %>%
    group_by(date) %>%
    mutate(week = dense_rank(date)) %>%
    ungroup() %>%
    mutate(
      week_f = factor(week),
      species = factor(species, levels = c("Dogwood", "Hawthorn", "Maple")),
      site = factor(site, levels = c("Downtown", "Park"))
    )
  
  
# ---- Standardize gas-exchange data

gas_dat <- gas_raw %>%
    mutate(
      site = recode(site,
                    "c" = "Downtown",
                    "p" = "Park"),
      species = recode(species,
                       "d" = "Dogwood",
                       "h" = "Hawthorn",
                       "m" = "Maple"),
      tree_id = paste(site, species, replicate, sep = "_"),
      week_f = factor(week),
      species = factor(species, levels = c("Dogwood", "Hawthorn", "Maple")),
      site = factor(site, levels = c("Downtown", "Park"))
    )
  
# ---- Standardize water-potential data
# wp_bar is treated as pressure-bomb balance pressure in bar.
# Convert to water potential in MPa as negative values:
# 1 bar = 0.1 MPa
  
wp_dat <- wp_raw %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      site = recode(site,
                    "c" = "Downtown",
                    "p" = "Park"),
      species = recode(species,
                       "d" = "Dogwood",
                       "h" = "Hawthorn",
                       "m" = "Maple"),
      tree_id = paste(site, species, replicate, sep = "_"),
      psi_mpa = -wp_bar * 0.1,
      week_f = factor(week),
      species = factor(species, levels = c("Dogwood", "Hawthorn", "Maple")),
      site = factor(site, levels = c("Downtown", "Park"))
    )
  
##Step: Create plotting summaries-----

#Helper function for means and SE
  
mean_se <- function(x) {
    tibble(
      n = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      se = sd(x, na.rm = TRUE) / sqrt(n)
    )
  }  
  
#DBH growth slope per tree
# Slope is calculated as mm per day, then converted to mm per week
# for easier interpretation.

dbh_slopes_tree <- dbh_dat %>%
  group_by(tree_id, site, species) %>%
  mutate(
    day = as.numeric(date - min(date))
  ) %>%
  summarise(
    dbh_slope_mm_day = coef(lm(dbh_mm ~ day))[2],
    dbh_slope_mm_week = dbh_slope_mm_day * 7,
    dbh_start_mm = first(dbh_mm[order(date)]),
    dbh_end_mm = last(dbh_mm[order(date)]),
    dbh_total_growth_mm = dbh_end_mm - dbh_start_mm,
    .groups = "drop"
  )

dbh_slope_summary <- dbh_slopes_tree %>%
  group_by(site, species) %>%
  summarise(
    n = n(),
    mean = mean(dbh_slope_mm_week, na.rm = TRUE),
    se = sd(dbh_slope_mm_week, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

#Weekly stomatal conductance summary

gsw_summary <- gas_dat %>%
  group_by(site, species, week) %>%
  summarise(
    n = sum(!is.na(gsw)),
    mean = mean(gsw, na.rm = TRUE),
    se = sd(gsw, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    week = as.numeric(week)
  )

#Weekly midday leaf water potential summary

psi_summary <- wp_dat %>%
  group_by(site, species, week) %>%
  summarise(
    n = sum(!is.na(psi_mpa)),
    mean = mean(psi_mpa, na.rm = TRUE),
    se = sd(psi_mpa, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    week = as.numeric(week)
  )

##Step: Draft 3-panel figure------

# ---- Species colors used throughout manuscript

species_cols <- c(
  "Dogwood"  = "#228833",
  "Hawthorn" = "#D55E00",
  "Maple"    = "#0072B2"
)

# ---- Species-pooled weekly summaries

gsw_species_summary <- gas_dat %>%
  group_by(species, week) %>%
  summarise(
    n = sum(!is.na(gsw)),
    mean = mean(gsw, na.rm = TRUE),
    se = sd(gsw, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(week = as.numeric(week))

psi_species_summary <- wp_dat %>%
  group_by(species, week) %>%
  summarise(
    n = sum(!is.na(psi_mpa)),
    mean = mean(psi_mpa, na.rm = TRUE),
    se = sd(psi_mpa, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(week = as.numeric(week))

# ---- Helper for mean ± SE in raw-tree DBH panel

mean_se_gg <- function(x) {
  n <- sum(!is.na(x))
  m <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(n)
  data.frame(y = m, ymin = m - se, ymax = m + se)
}

#Dynamic y-axis range for Panel A
#Includes both raw tree slopes and mean ± SE values

growth_y_min <- min(
  c(
    dbh_slopes_tree$dbh_slope_mm_week,
    dbh_slope_summary$mean - dbh_slope_summary$se
  ),
  na.rm = TRUE
)

growth_y_max <- max(
  c(
    dbh_slopes_tree$dbh_slope_mm_week,
    dbh_slope_summary$mean + dbh_slope_summary$se
  ),
  na.rm = TRUE
)

growth_y_range <- growth_y_max - growth_y_min

growth_ylim <- c(
  growth_y_min - growth_y_range * 0.05,
  growth_y_max + growth_y_range * 0.15
)

# ---- Shared theme

fig_theme2 <- theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.tag = element_text(face = "bold", size = 13)
  )

#Panel A: DBH growth slope, species emphasized

p_growth2_hleg <- ggplot(
  dbh_slopes_tree,
  aes(x = species, y = dbh_slope_mm_week, color = species)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed") +
  geom_jitter(
    aes(shape = site),
    width = 0.08,
    height = 0,
    size = 2.2,
    alpha = 0.65
  ) +
  stat_summary(
    fun.data = mean_se_gg,
    geom = "errorbar",
    width = 0.18,
    linewidth = 0.75
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4
  ) +
  scale_color_manual(values = species_cols) +
  coord_cartesian(ylim = growth_ylim) +
  labs(
    x = NULL,
    y = expression("Stem diameter growth slope (mm week"^-1*")"),
    shape = NULL
  ) +
  guides(
    color = "none",
    shape = guide_legend(
      direction = "horizontal",
      nrow = 1,
      override.aes = list(color = "black", size = 2.8, alpha = 1)
    )
  ) +
  fig_theme2 +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size=12),
    legend.position = c(0.04, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_blank(),
    legend.margin = margin(0, 1, 0, 1),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.x = unit(0.12, "cm"),
    plot.margin = margin(t = 2, r = 4, b = 0, l = 4)
  )

#Panel B: gsw, species-pooled time series

p_gsw2_hleg <- ggplot(
  gsw_species_summary,
  aes(x = week, y = mean, color = species, group = species)
) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.12,
    linewidth = 0.35,
    alpha = 0.8
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  scale_color_manual(values = species_cols) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = NULL,
    y = expression(g[sw]~"(mol H"[2]*"O m"^-2*" s"^-1*")"),
    color = NULL
  ) +
  guides(
    color = guide_legend(
      direction = "horizontal",
      nrow = 1,
      override.aes = list(linewidth = 1, size = 2.8)
    )
  ) +
  fig_theme2 +
  theme(
    axis.text.x = element_blank(),
    legend.position = c(0.04, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_blank(),
    legend.margin = margin(0, 1, 0, 1),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.x = unit(0.12, "cm"),
    plot.margin = margin(t = 2, r = 4, b = 0, l = 4)
  )

#Panel C: Psi_md, species-pooled time series

p_psi2_tight <- ggplot(
  psi_species_summary,
  aes(x = week, y = mean, color = species, group = species)
) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.12,
    linewidth = 0.35,
    alpha = 0.8
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  scale_color_manual(values = species_cols) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Sampling week",
    y = expression(Psi[md]~"(MPa)")
  ) +
  guides(color = "none") +
  fig_theme2 +
  theme(
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = -8)),
    plot.margin = margin(t = 0, r = 4, b = 0, l = 4)
  )

# Combine: compact growth panel + wide stacked physiology panels

right_stack <- p_gsw2_hleg / p_psi2_tight +
  plot_layout(heights = c(1, 1))

fig2_legend_test3 <- p_growth2_hleg + right_stack +
  plot_layout(widths = c(0.8, 2.2)) +
  plot_annotation(tag_levels = "A")

# windows()
# fig2_legend_test3


ggsave(
  filename = "figures/figure2_growth_gsw_psi.png",
  plot = fig2_legend_test3,
  width = 9.5,
  height = 5.2,
  units = "in",
  dpi = 600
)

# ---- Save PDF ----

ggsave(
  filename = "figures/figure2_growth_gsw_psi.pdf",
  plot = fig2_legend_test3,
  width = 9.5,
  height = 5.2,
  units = "in",
  device = cairo_pdf
)
