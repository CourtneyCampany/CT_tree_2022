
## Among Downtown trees, did available planting-pit soil volume explain variation 
## in gas-exchange physiology during establishment?

##Step: Read and wrangle gas exchange data, merge with pit info-----

library(tidyverse)
library(lubridate)

# Import corrected gas-exchange master file
gx_raw <- read_csv(
  "raw_data/gasexchange_master_clean.csv",
  show_col_types = FALSE,
)

# Import verified tree metadata from growth analysis
tree_metadata <- read_csv(
  "calculated_data/tree_metadata_derived.csv",
  show_col_types = FALSE
)

gx <- gx_raw %>%
  transmute(
    site = site,
    species = species,
    replicate = as.integer(replicate),
    week = as.integer(week),
    
    A = as.numeric(A),          # µmol CO2 m-2 s-1
    gsw = as.numeric(gsw),      # mol H2O m-2 s-1
    E = as.numeric(Emm)         # mmol H2O m-2 s-1
  ) %>%
  mutate(
    tree_id = paste(site, species, replicate, sep = "-"),
    iWUE = A / gsw
  )

tree_metadata <- tree_metadata %>%
  mutate(
    replicate = as.integer(replicate),
    tree_id = paste(site, species, replicate, sep = "-"),
    pit_volume_m3 = 
      (pit_length_cm / 100) *
      (pit_width_cm / 100) *
      (pit_depth_cm / 100),
    pit_size_class = case_when(
      pit_volume_m3 < 0.5 ~ "Small",
      pit_volume_m3 >= 0.5 ~ "Large"
    ),
    pit_size_class = factor(
      pit_size_class,
      levels = c("Small", "Large")
    )
  )

#extract downtown metadata for pit sizes
downtown_metadata <- tree_metadata %>%
  filter(site == "c") %>%
  select(
    tree_id,
    site,
    species,
    replicate,
    pit_size_class,
    pit_volume_m3
  )

downtown_metadata

#seasonal gas exchange summary
downtown_gx_summary <- gx %>%
  filter(site == "c") %>%
  group_by(tree_id, species, replicate) %>%
  summarise(
    n_weeks = n(),
    mean_A = mean(A, na.rm = TRUE),
    mean_gsw = mean(gsw, na.rm = TRUE),
    mean_E = mean(E, na.rm = TRUE),
    mean_iWUE = mean(iWUE, na.rm = TRUE),
    .groups = "drop"
  )

#merge gas exchange with pit info
downtown_gx_pit <- downtown_gx_summary %>%
  left_join(
    downtown_metadata,
    by = c("tree_id", "species", "replicate")
  )

#add interpretable species labels

downtown_gx_pit <- downtown_gx_pit %>%
  mutate(
    species_name = factor(
      dplyr::recode(
        species,
        d = "Dogwood",
        h = "Hawthorn",
        m = "Maple"
      ),
      levels = c("Dogwood", "Hawthorn", "Maple")
    ),
    pit_label = factor(
      dplyr::recode(
        pit_size_class,
        Small = "Small pit (0.31 m³)",
        Large = "Large pit (0.71 m³)"
      ),
      levels = c(
        "Small pit (0.31 m³)",
        "Large pit (0.71 m³)"
      )
    )
  )

##Step: Initial descriptive plot-----

downtown_plot_data <- downtown_gx_pit %>%
  select(
    species_name,
    pit_label,
    mean_A,
    mean_gsw,
    mean_E,
    mean_iWUE
  ) %>%
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "response",
    values_to = "value"
  ) %>%
  mutate(
    response = factor(
      response,
      levels = c("mean_A", "mean_gsw", "mean_E", "mean_iWUE"),
      labels = c(
        "Photosynthesis, A",
        "Stomatal conductance, gsw",
        "Transpiration, E",
        "Intrinsic water-use efficiency, iWUE"
      )
    )
  )

ggplot(
  downtown_plot_data,
  aes(x = pit_label, y = value, color = species_name)
) +
  geom_point(
    position = position_jitter(width = 0.08, height = 0),
    size = 2.3
  ) +
  stat_summary(
    aes(group = species_name),
    fun = mean,
    geom = "line",
    position = position_dodge(width = 0.2),
    linewidth = 0.7
  ) +
  facet_wrap(~ response, scales = "free_y", ncol = 2) +
  labs(
    x = "Downtown planting-pit size class",
    y = "Seasonal tree mean",
    color = "Species"
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 20, hjust = 1),
    strip.background = element_blank()
  )

## Step: Downtown-only models

pit_A <- lm(
  mean_A ~ species_name + pit_size_class,
  data = downtown_gx_pit
)

pit_gsw <- lm(
  mean_gsw ~ species_name + pit_size_class,
  data = downtown_gx_pit
)

pit_E <- lm(
  mean_E ~ species_name + pit_size_class,
  data = downtown_gx_pit
)

pit_iWUE <- lm(
  mean_iWUE ~ species_name + pit_size_class,
  data = downtown_gx_pit
)

anova(pit_A)
anova(pit_gsw)
anova(pit_E)
anova(pit_iWUE)
#Downtown gas-exchange differences, whereas the two planting-pit size classes 
#explained essentially none of the variation in A, gsw, E, or iWUE after 
#accounting for species.

# summary(pit_A)
# summary(pit_gsw)
# summary(pit_E)
# summary(pit_iWUE)


#species-specific temporal trajectories and asks whether pit-size classes show a general difference through time
response ~ species_name * week_f + pit_size_class * week_f +
  Error(tree_id / week_f)

downtown_gx_weekly <- gx %>%
  filter(site == "c") %>%
  left_join(
    downtown_metadata %>%
      select(tree_id, pit_size_class, pit_volume_m3),
    by = "tree_id"
  ) %>%
  mutate(
    species_name = factor(
      dplyr::recode(
        species,
        d = "Dogwood",
        h = "Hawthorn",
        m = "Maple"
      ),
      levels = c("Dogwood", "Hawthorn", "Maple")
    ),
    week_f = factor(week),
    gsw_sqrt = sqrt(gsw),
    E_sqrt = sqrt(E)
  )

rm_pit_gsw <- aov(
  gsw_sqrt ~ species_name * week_f +
    pit_size_class * week_f +
    Error(tree_id / week_f),
  data = downtown_gx_weekly
)

rm_pit_E <- aov(
  E_sqrt ~ species_name * week_f +
    pit_size_class * week_f +
    Error(tree_id / week_f),
  data = downtown_gx_weekly
)

summary(rm_pit_gsw)
summary(rm_pit_E)
#no pit size trends

rm_pit_A <- aov(
  A ~ species_name * week_f +
    pit_size_class * week_f +
    Error(tree_id / week_f),
  data = downtown_gx_weekly
)

rm_pit_iWUE <- aov(
  iWUE ~ species_name * week_f +
    pit_size_class * week_f +
    Error(tree_id / week_f),
  data = downtown_gx_weekly
)

summary(rm_pit_A)
summary(rm_pit_iWUE)
#pit effect for iWUE

#holm correction
pit_week_interaction_p <- c(
  A = 0.185671,
  gsw = 0.158672,
  E = 0.092741,
  iWUE = 0.00347
)

p.adjust(pit_week_interaction_p, method = "holm")

#weekly contrasts for iWUE for pit size

pit_iWUE_week_contrasts <- downtown_gx_weekly %>%
  group_by(week) %>%
  nest() %>%
  mutate(
    model = map(
      data,
      ~ lm(iWUE ~ species_name + pit_size_class, data = .x)
    ),
    pit_test = map(
      model,
      ~ broom::tidy(.x) %>%
        filter(term == "pit_size_classLarge")
    ),
    estimate_large_minus_small = map_dbl(pit_test, ~ .x$estimate),
    std_error = map_dbl(pit_test, ~ .x$std.error),
    statistic = map_dbl(pit_test, ~ .x$statistic),
    p_raw = map_dbl(pit_test, ~ .x$p.value)
  ) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p_raw, method = "holm"),
    significant = if_else(p_holm < 0.05, "Yes", "No")
  ) %>%
  select(
    week,
    estimate_large_minus_small,
    std_error,
    statistic,
    p_raw,
    p_holm,
    significant
  ) %>%
  arrange(week)

pit_iWUE_week_contrasts
