
library(tidyverse)
library(patchwork)

#Species colors used throughout manuscript

species_cols <- c(
  "Dogwood"  = "#228833",
  "Hawthorn" = "#D55E00",
  "Maple"    = "#0072B2"
)

##Step: Read in and format datasets for figure 3-----

stomata_raw <- read_csv("raw_data/stomata_density.csv", show_col_types = FALSE)

lma_raw <- read_csv("raw_data/leafthick.csv", show_col_types = FALSE)

chem_raw <- read_csv("raw_data/isotope_data.csv", show_col_types = FALSE)

#Standardize stomatal density data
  # Field-of-view area is calculated from fov_diam_mm
  # Density is stomata per mm^2

stomata_dat <- stomata_raw %>%
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
    fov_area_mm2 = pi * (fov_diam_mm / 2)^2,
    stomatal_density = stomata_count / fov_area_mm2,
    week_f = factor(week),
    species = factor(species, levels = c("Dogwood", "Hawthorn", "Maple")),
    site = factor(site, levels = c("Downtown", "Park"))
  )


#Standardize LMA data
  # LMA = dry mass / leaf area.
  # drymass_g / area_cm2 gives g cm^-2.
  # Multiply by 10,000 to convert to g m^-2.
  
lma_dat <- lma_raw %>%
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
    lma_g_m2 = (drymass_g / area_cm2) * 10000,
    week_f = factor(week),
    species = factor(species, levels = c("Dogwood", "Hawthorn", "Maple")),
    site = factor(site, levels = c("Downtown", "Park"))
  )

#Standardize foliar chemistry data
  # nitro_perc is foliar nitrogen concentration (%N).

chem_dat <- chem_raw %>%
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
    foliar_N_perc = nitro_perc,
    CN = carbon_perc / nitro_perc,
    week_f = factor(week),
    species = factor(species, levels = c("Dogwood", "Hawthorn", "Maple")),
    site = factor(site, levels = c("Downtown", "Park"))
  )


##Step: Create plotting summaries of datasets-----

summarise_mean_se <- function(data, response) {
  data %>%
    summarise(
      n = sum(!is.na({{ response }})),
      mean = mean({{ response }}, na.rm = TRUE),
      se = sd({{ response }}, na.rm = TRUE) / sqrt(n),
      .groups = "drop"
    )
}

#Stomatal density
  # First average field-of-view replicates to tree × week.

stomata_tree_week <- stomata_dat %>%
  group_by(tree_id, site, species, week) %>%
  summarise(
    stomatal_density = mean(stomatal_density, na.rm = TRUE),
    n_fields = sum(!is.na(stomatal_density)),
    .groups = "drop"
  )

stomata_species_week <- stomata_tree_week %>%
  group_by(species, week) %>%
  summarise_mean_se(stomatal_density) %>%
  mutate(
    week = as.numeric(week),
    trait = "Stomatal density"
  )

#LMA

lma_species_week <- lma_dat %>%
  group_by(species, week) %>%
  summarise_mean_se(lma_g_m2) %>%
  mutate(
    week = as.numeric(week),
    trait = "LMA"
  )

#Foliar N

foliarN_species_week <- chem_dat %>%
  group_by(species, week) %>%
  summarise_mean_se(foliar_N_perc) %>%
  mutate(
    week = as.numeric(week),
    trait = "Foliar N"
  )

##Step: Create 3 panel plot------

#Shared theme

fig3_theme <- theme_classic(base_size = 12) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.tag = element_text(face = "bold", size = 13)
  )

#Panel A: LMA with species legend

p_lma <- ggplot(
  lma_species_week,
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
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  labs(
    x = NULL,
    y = expression("LMA (g m"^-2*")"),
    color = NULL
  ) +
  guides(
    color = guide_legend(
      direction = "horizontal",
      nrow = 1,
      override.aes = list(linewidth = 1, size = 2.8)
    )
  ) +
  fig3_theme +
  theme(
    axis.text.x = element_blank(),
    legend.position = c(0.03, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_blank(),
    legend.margin = margin(0, 1, 0, 1),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.x = unit(0.12, "cm"),
    plot.margin = margin(t = 2, r = 4, b = 0, l = 4)
  )

#Panel B: Foliar N

p_foliarN <- ggplot(
  foliarN_species_week,
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
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  labs(
    x = NULL,
    y = "Foliar N (%)"
  ) +
  guides(color = "none") +
  fig3_theme +
  theme(
    axis.text.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 0, r = 4, b = 0, l = 4)
  )

#Panel C: Stomatal density
  # Stomatal density begins at week 2; week 1 is absent because
  # stomatal impressions were not available for that week.

p_stomata <- ggplot(
  stomata_species_week,
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
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  labs(
    x = "Sampling week",
    y = expression("Stomatal density (mm"^-2*")")
  ) +
  guides(color = "none") +
  fig3_theme +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 0, r = 4, b = 0, l = 4)
  )

#Combine reordered figure

#Panel A: LMA with finer y-axis breaks

p_lma <- p_lma +
  scale_y_continuous(
    breaks = seq(60, 180, by = 40)
  )

# Panel B: Foliar N with finer y-axis breaks

p_foliarN <- p_foliarN +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 4)
  )

# Remove x-axis label/text from stomatal density because it is now Panel B

p_stomata <- p_stomata +
  labs(x = NULL) +
  theme(
    axis.text.x = element_blank()
  )

# Add x-axis label/text to foliar N because it is now Panel C

p_foliarN <- p_foliarN +
  labs(x = "Sampling week") +
  theme(
    axis.text.x = element_text()
  )

#Rebuild figure

fig3_draft3 <- p_lma / p_stomata / p_foliarN +
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(tag_levels = "A")


# windows()
# fig3_draft3

#save
  
  ggsave(
    filename = "figures/figure3_leaf_traits.png",
    plot = fig3_draft3,
    width = 7,
    height = 7,
    units = "in",
    dpi = 600
  )

