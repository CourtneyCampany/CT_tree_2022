library(tidyverse)
library(lubridate)
library(purrr)

##Step: Functions to build the table one trait at a time with p values from aov model-----

format_p <- function(p) {
  case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

clean_model_term <- function(term) {
  case_when(
    term == "site" ~ "Site",
    term == "species" ~ "Species",
    term == "site:species" ~ "Site × species",
    term == "week_f" ~ "Week",
    term == "site:week_f" ~ "Site × week",
    term == "species:week_f" ~ "Species × week",
    term == "site:species:week_f" ~ "Site × species × week",
    TRUE ~ term
  )
}

extract_primary_aov_pvalues <- function(model,
                                        trait_group,
                                        trait,
                                        response,
                                        units,
                                        scale,
                                        notes = NA_character_) {
  
  model_summary <- summary(model)
  
  stats_full <- purrr::imap_dfr(model_summary, function(stratum, stratum_name) {
    
    as.data.frame(stratum[[1]]) |>
      rownames_to_column("term_raw") |>
      rename(
        df_num = Df,
        sum_sq = `Sum Sq`,
        mean_sq = `Mean Sq`,
        F_value = `F value`,
        p_value = `Pr(>F)`
      ) |>
      mutate(error_stratum = stratum_name)
  })
  
  df_den_lookup <- stats_full |>
    filter(term_raw == "Residuals") |>
    select(error_stratum, df_den = df_num)
  
  stats_full |>
    filter(term_raw != "Residuals") |>
    left_join(df_den_lookup, by = "error_stratum") |>
    mutate(
      trait_group = trait_group,
      trait = trait,
      response = response,
      units = units,
      scale = scale,
      term = clean_model_term(term_raw),
      p_text = format_p(p_value),
      notes = notes
    ) |>
    select(
      trait_group,
      trait,
      response,
      units,
      scale,
      term_raw,
      term,
      df_num,
      df_den,
      F_value,
      p_value,
      p_text,
      notes
    )
}

extract_simple_aov_pvalues <- function(model,
                                       trait_group,
                                       trait,
                                       response,
                                       units,
                                       scale,
                                       notes = NA_character_) {
  
  stats_full <- as.data.frame(summary(model)[[1]]) |>
    rownames_to_column("term_raw") |>
    mutate(
      term_raw = stringr::str_trim(term_raw)
    ) |>
    rename(
      df_num = Df,
      sum_sq = `Sum Sq`,
      mean_sq = `Mean Sq`,
      F_value = `F value`,
      p_value = `Pr(>F)`
    )
  
  df_den <- stats_full |>
    filter(term_raw == "Residuals") |>
    pull(df_num)
  
  # Safety check in case Residuals is not found
  if (length(df_den) == 0) {
    df_den <- NA_real_
    warning("Residual df not found; df_den set to NA.")
  }
  
  stats_full |>
    filter(term_raw != "Residuals") |>
    mutate(
      trait_group = trait_group,
      trait = trait,
      response = response,
      units = units,
      scale = scale,
      error_stratum = "Between-tree",
      term = clean_model_term(term_raw),
      df_den = df_den,
      p_text = format_p(p_value),
      notes = notes
    ) |>
    select(
      trait_group,
      trait,
      response,
      units,
      scale,
      error_stratum,
      term_raw,
      term,
      df_num,
      df_den,
      F_value,
      p_value,
      p_text,
      notes
    )
}

##Step: Growth----

dbh_weekly<- read.csv("raw_data/dbh_clean.csv")
  dbh_weekly$site <- as.factor(dbh_weekly$site)
  dbh_weekly$species <- as.factor(dbh_weekly$species)
  dbh_weekly$date <- as.Date(dbh_weekly$date, "%m/%d/%Y")
  dbh_weekly$replicate <- factor(dbh_weekly$replicate)
  dbh_weekly$tree_id  <- paste(dbh_weekly$site, dbh_weekly$species, dbh_weekly$replicate, sep = "-")
  
  dbh_weekly <- dbh_weekly[order(dbh_weekly$tree_id , dbh_weekly$date), ]  
  #make a week variable
  dbh_weekly$week <- ave(dbh_weekly$dbh_mm, dbh_weekly$tree_id , FUN = seq_along)
  # Create temporary row identifier to preserve the current weekly-data order
  dbh_weekly$row_id <- seq_len(nrow(dbh_weekly))

#growth slopes
dbh_growth_slopes <- dbh_weekly |>
    group_by(tree_id, site, species, replicate) |>
    summarize(
      dbh_start_mm = dbh_mm[which.min(week)],
      dbh_end_mm = dbh_mm[which.max(week)],
      dbh_change_mm = dbh_end_mm - dbh_start_mm,
      dbh_slope_mm_week = coef(lm(dbh_mm ~ week))[["week"]],
      .groups = "drop"
    )
      
# Rank-transform DBH growth slope
dbh_growth_slopes <- dbh_growth_slopes |>
        mutate(
          rank_dbh_slope = rank(dbh_slope_mm_week)
        )

dbh_growth_aov <- aov(
  rank_dbh_slope ~ site * species,
  data = dbh_growth_slopes
)


dbh_growth_stats <- extract_simple_aov_pvalues(
  model = dbh_growth_aov,
  trait_group = "Growth",
  trait = "DBH growth",
  response = "rank_dbh_slope",
  units = "ranked mm week^-1",
  scale = "rank-transformed slope",
  notes = "Tree-level DBH growth slope across 10 measurement weeks"
)

# summary(dbh_growth_aov) 


##STEP: A, gsw, E, iWUE-------

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
  ) %>%
  mutate(
    week_f = factor(week),
    tree_id = interaction(site, species, replicate, sep = "_", drop = TRUE),
    iWUE = A / gsw,
    sqrt_gsw = sqrt(gsw),
    sqrt_E = sqrt(E)
  )

#complete tree filtering
gx_tree_completeness <- gx %>%
  group_by(site, species, replicate, tree_id) %>%
  summarise(
    n_records = n(),
    n_A = sum(!is.na(A)),
    n_gsw = sum(!is.na(gsw)),
    n_E = sum(!is.na(E)),
    n_iWUE = sum(!is.na(iWUE) & is.finite(iWUE)),
    .groups = "drop"
  ) %>%
  arrange(site, species, replicate)

gx_tree_completeness %>%
  filter(n_A < 10 | n_gsw < 10 | n_E < 10 | n_iWUE < 10)

complete_gx_trees <- gx_tree_completeness %>%
  filter(
    n_A == 10,
    n_gsw == 10,
    n_E == 10,
    n_iWUE == 10
  ) %>%
  pull(tree_id)

gx_complete <- gx %>%
  filter(tree_id %in% complete_gx_trees) %>%
  mutate(
    sqrt_gsw = sqrt(gsw),
    sqrt_E = sqrt(E)
  )

#run models
A_aov <- aov(
  A ~ site * species * week_f +
    Error(tree_id / week_f),
  data = gx_complete
)

gsw_aov <- aov(
  sqrt_gsw ~ site * species * week_f +
    Error(tree_id / week_f),
  data = gx_complete
)

E_aov <- aov(
  sqrt_E ~ site * species * week_f +
    Error(tree_id / week_f),
  data = gx_complete
)

iWUE_aov <- aov(
  iWUE ~ site * species * week_f +
    Error(tree_id / week_f),
  data = gx_complete
)

# summary(A_aov) 
# summary(gsw_aov) 
# summary(E_aov) 
# summary(iWUE_aov) 

#extract and add to table
A_stats <- extract_primary_aov_pvalues(
  model = A_aov,
  trait_group = "Gas exchange",
  trait = "Photosynthesis",
  response = "A",
  units = "µmol CO2 m^-2 s^-1",
  scale = "raw",
  notes = "Light-saturated photosynthesis"
)

gsw_stats <- extract_primary_aov_pvalues(
  model = gsw_aov,
  trait_group = "Gas exchange",
  trait = "Stomatal conductance",
  response = "sqrt_gsw",
  units = "mol H2O m^-2 s^-1",
  scale = "square-root transformed",
  notes = "Original variable: gsw"
)

E_stats <- extract_primary_aov_pvalues(
  model = E_aov,
  trait_group = "Gas exchange",
  trait = "Transpiration",
  response = "sqrt_E",
  units = "mmol H2O m^-2 s^-1",
  scale = "square-root transformed",
  notes = "Original variable: E"
)

iWUE_stats <- extract_primary_aov_pvalues(
  model = iWUE_aov,
  trait_group = "Gas exchange",
  trait = "Intrinsic water-use efficiency",
  response = "iWUE",
  units = "µmol CO2 mol^-1 H2O",
  scale = "raw",
  notes = "Calculated as A / gsw"
)

##STEP: LMA------

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

# Complete-tree filtering based on non-missing LMA values

leaf_tree_completeness <- leaf_audit %>%
  group_by(site, species, replicate, tree_id) %>%
  summarise(
    n_records = n(),
    n_lma = sum(!is.na(lma_g_m2)),
    missing_lma_weeks = paste(week[is.na(lma_g_m2)], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(site, species, replicate)


complete_leaf_trees <- leaf_tree_completeness %>%
  filter(n_lma == 10) %>%
  pull(tree_id)

leaf_complete <- leaf_audit %>%
  filter(tree_id %in% complete_leaf_trees) %>%
  mutate(
    log_lma_g_m2 = log(lma_g_m2)
  )


#run model
rm_lma  <- aov(
  log_lma_g_m2 ~ site * species * week_f +
    Error(tree_id / week_f),
  data = leaf_complete
)

# summary(rm_lma)

# Extract LMA stats for manuscript table

lma_stats <- extract_primary_aov_pvalues(
  model = rm_lma,
  trait_group = "Structural",
  trait = "Leaf mass per area",
  response = "log_lma_g_m2",
  units = "g m^-2",
  scale = "natural log",
  notes = "Complete-tree repeated-measures analysis; trees retained only when LMA was present for all 10 weeks"
)

# lma_stats

##Step: Stomatal density-----

stomata_raw <- read.csv("raw_data/stomata_density.csv")

stomata_density_raw <- stomata_raw %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    week_f = factor(week),
    tree_id = paste(site, species, replicate, sep = "_"),
    fov_area_mm2 = pi * (fov_diam_mm / 2)^2,
    stomatal_density_mm2 = stomata_count / fov_area_mm2
  )

#means tree per week

stomata_density_leaf <- stomata_density_raw %>%
  group_by(date, week, week_f, site, species, replicate, tree_id) %>%
  summarise(
    n_count_rows = n(),
    n_nonmissing_counts = sum(!is.na(stomata_count)),
    mean_stomata_count = mean(stomata_count, na.rm = TRUE),
    sd_stomata_count = sd(stomata_count, na.rm = TRUE),
    mean_stomatal_density_mm2 = mean(stomatal_density_mm2, na.rm = TRUE),
    sd_stomatal_density_mm2 = sd(stomatal_density_mm2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    leaf_complete = n_count_rows == 3 & n_nonmissing_counts == 3
  )

#complete tree dataset

expected_weeks <- 2:10

stomata_tree_audit <- stomata_density_leaf %>%
  group_by(site, species, replicate, tree_id) %>%
  summarise(
    n_complete_weeks = sum(leaf_complete),
    missing_weeks = paste(
      setdiff(expected_weeks, sort(unique(week[leaf_complete]))),
      collapse = ","
    ),
    complete_weeks_2_10 = all(expected_weeks %in% week[leaf_complete]),
    .groups = "drop"
  )


stomata_complete_ids <- stomata_tree_audit %>%
  filter(complete_weeks_2_10) %>%
  pull(tree_id)

stomata_density_primary <- stomata_density_leaf %>%
  filter(
    tree_id %in% stomata_complete_ids,
    week %in% expected_weeks,
    leaf_complete
  ) %>%
  mutate(
    log_stomatal_density_mm2 = log(mean_stomatal_density_mm2)
  )

#run model
rm_stomatal_density <- aov(
  mean_stomatal_density_mm2 ~ site * species * week_f +
    Error(tree_id / week_f),
  data = stomata_density_primary
)

# summary(rm_stomatal_density)

# Extract stomatal-density stats for manuscript table

stomatal_density_stats <- extract_primary_aov_pvalues(
  model = rm_stomatal_density,
  trait_group = "Structural",
  trait = "Stomatal density",
  response = "mean_stomatal_density_mm2",
  units = "stomata mm^-2",
  scale = "raw",
  notes = "Complete-tree repeated-measures analysis; weeks 2–10 only"
)

##Step: Chlorophyll------

chl_raw <- read_csv("raw_data/chlorophyll_extract.csv", show_col_types = FALSE)

chl_clean <- chl_raw %>%
  filter(if_any(everything(), ~ !is.na(.x))) %>%
  mutate(
    site = as.character(site),
    species = as.character(species),
    date = mdy(date),
    replicate = as.integer(replicate),
    week = as.integer(week),
    week_f = factor(week),
    tree_id = paste(site, species, replicate, sep = "_")
  )

#calculate total chlorophyll from absorbance
extract_vol_L <- 0.003       # 3 mL DMF

chl_extract <- chl_clean %>%
  mutate(
    # Convert punch diameter from mm to cm, then calculate area of one punch
    punch_diameter_cm = punch_diameter_mm / 10,
    punch_radius_cm = punch_diameter_cm / 2,
    punch_area_cm2 = pi * punch_radius_cm^2,
    total_punch_area_cm2 = punch_num * punch_area_cm2,
    
    # Inskeep & Bloom DMF equations; units = mg chlorophyll L-1 extract
    chla_mg_L_calc = 12.70 * `abs_664.5` - 2.79 * abs_647,
    chlb_mg_L_calc = 20.70 * abs_647 - 4.62 * `abs_664.5`,
    total_chl_mg_L_calc = 17.90 * abs_647 + 8.08 * `abs_664.5`,
    
    # Convert concentration in extract to total chlorophyll amount in sample
    chla_ug_sample = chla_mg_L_calc * extract_vol_L * 1000,
    chlb_ug_sample = chlb_mg_L_calc * extract_vol_L * 1000,
    total_chl_ug_sample = total_chl_mg_L_calc * extract_vol_L * 1000,
    
    # Area-normalized chlorophyll
    chla_ug_cm2 = chla_ug_sample / total_punch_area_cm2,
    chlb_ug_cm2 = chlb_ug_sample / total_punch_area_cm2,
    total_chl_ug_cm2 = total_chl_ug_sample / total_punch_area_cm2,
    
    # Fresh-mass-normalized chlorophyll
    chla_mg_g_fw = (chla_mg_L_calc * extract_vol_L) / (total_mass_mg / 1000),
    chlb_mg_g_fw = (chlb_mg_L_calc * extract_vol_L) / (total_mass_mg / 1000),
    total_chl_mg_g_fw = (total_chl_mg_L_calc * extract_vol_L) / (total_mass_mg / 1000),
    
    # Chlorophyll a:b ratio
    chl_ab_ratio = chla_mg_L_calc / chlb_mg_L_calc
  )

# Complete-tree filtering for total chlorophyll

chl_tree_completeness <- chl_extract %>%
  group_by(site, species, replicate, tree_id) %>%
  summarise(
    n_records = n(),
    n_total_chl = sum(!is.na(total_chl_ug_cm2)),
    missing_total_chl_weeks = paste(week[is.na(total_chl_ug_cm2)], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(site, species, replicate)

complete_chl_trees <- chl_tree_completeness %>%
  filter(n_total_chl == 10) %>%
  pull(tree_id)

chl_complete <- chl_extract %>%
  filter(tree_id %in% complete_chl_trees)

#run model with log (confirmed primary stats script)

rm_total_chl <- aov(
  log(total_chl_ug_cm2) ~ site * species * week_f +
    Error(tree_id / week_f),
  data = chl_complete
)

# summary(rm_total_chl) 

# Extract total chlorophyll stats for manuscript table

total_chl_stats <- extract_primary_aov_pvalues(
  model = rm_total_chl,
  trait_group = "Pigments",
  trait = "Total chlorophyll",
  response = "total_chl_ug_cm2",
  units = "µg cm^-2",
  scale = "raw",
  notes = "Complete-tree repeated-measures analysis; DMF extraction"
)

##Step: Water potential------

water_raw <- read_csv("raw_data/water_potentials.csv", show_col_types = FALSE)

#standardize variables

water_audit <- water_raw %>%
  mutate(
    date = mdy(date),
    site = factor(site),
    species = factor(species),
    replicate = as.factor(replicate),
    week = as.integer(week),
    week_f = factor(week),
    tree_id = factor(paste(site, species, replicate, sep = "_")),
    
    # Pressure chamber values were recorded as positive balancing pressure in bar.
    # Convert to leaf water potential in MPa.
    # 1 bar = 0.1 MPa; leaf water potential is reported as negative.
    wp_mpa = -0.1 * wp_bar
  )

#Create response-scale variables

water_audit <- water_audit %>%
  mutate(
    wp_mag_mpa = abs(wp_mpa),
    log_wp_mag_mpa = log(wp_mag_mpa)
  )

#create complete-tree dataset (all measurments needed for repeated measures)
water_tree_balance <- water_audit %>%
  group_by(site, species, replicate, tree_id) %>%
  summarise(
    n_rows = n(),
    n_weeks = n_distinct(week),
    n_wp_nonmissing = sum(!is.na(wp_mpa)),
    n_wp_missing = sum(is.na(wp_mpa)),
    complete_tree = n_weeks == 10 & n_wp_missing == 0,
    weeks_missing_wp = paste(week[is.na(wp_mpa)], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(site, species, replicate)

water_primary_complete <- water_audit %>%
  semi_join(
    water_tree_balance %>%
      filter(complete_tree) %>%
      select(tree_id),
    by = "tree_id"
  )

water_primary_complete <- water_primary_complete %>%
  mutate(
    wp_mag_mpa = abs(wp_mpa),
    log_wp_mag_mpa = log(wp_mag_mpa)
  )

#run model

rm_wp <- aov(
  wp_mpa ~ site * species * week_f +
    Error(tree_id / week_f),
  data = water_primary_complete
)

# summary(rm_wp)

# Extract midday leaf water potential stats for manuscript table

wp_stats <- extract_primary_aov_pvalues(
  model = rm_wp,
  trait_group = "Water status",
  trait = "Midday leaf water potential",
  response = "wp_mpa",
  units = "MPa",
  scale = "raw",
  notes = "Complete-tree repeated-measures analysis; pressure chamber values converted from bar to MPa and reported as negative leaf water potential"
)

##Step: Nitrogen/CN/13C-----

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

# Complete-tree filtering for chemistry/isotope traits

isotope_tree_completeness <- isotope_data %>%
  group_by(site, species, replicate, tree_id) %>%
  summarise(
    n_records = n(),
    n_weeks = n_distinct(week),
    n_nitro = sum(!is.na(nitro_perc)),
    n_CN = sum(!is.na(CN_ratio)),
    n_c13 = sum(!is.na(c13)),
    missing_nitro_weeks = paste(week[is.na(nitro_perc)], collapse = ", "),
    missing_CN_weeks = paste(week[is.na(CN_ratio)], collapse = ", "),
    missing_c13_weeks = paste(week[is.na(c13)], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(site, species, replicate)

complete_isotope_trees <- isotope_tree_completeness %>%
  filter(
    n_nitro == 10,
    n_CN == 10,
    n_c13 == 10
  ) %>%
  pull(tree_id)

isotope_complete <- isotope_data %>%
  filter(tree_id %in% complete_isotope_trees)

#run models for N, CN, and 13C

rm_nitro <- aov(
  log(nitro_perc) ~ site * species * week_f +
    Error(tree_id / week_f),
  data = isotope_complete
)

rm_CN <- aov(
  log_CN_ratio ~ site * species * week_f +
    Error(tree_id / week_f),
  data = isotope_complete
)

rm_c13 <- aov(
  c13 ~ site * species * week_f +
    Error(tree_id / week_f),
  data = isotope_complete
)

# summary(rm_nitro) 
# summary(rm_CN) 
# summary(rm_c13) 

# Extract chemistry/isotope stats

nitro_stats <- extract_primary_aov_pvalues(
  model = rm_nitro,
  trait_group = "Leaf chemistry",
  trait = "Foliar N",
  response = "log_nitro_perc",
  units = "%",
  scale = "natural log",
  notes = "Complete-tree repeated-measures analysis"
)

CN_stats <- extract_primary_aov_pvalues(
  model = rm_CN,
  trait_group = "Leaf chemistry",
  trait = "C:N",
  response = "log_CN_ratio",
  units = "unitless",
  scale = "natural log",
  notes = "Complete-tree repeated-measures analysis"
)

c13_stats <- extract_primary_aov_pvalues(
  model = rm_c13,
  trait_group = "Leaf isotope",
  trait = "δ13C",
  response = "c13",
  units = "‰",
  scale = "raw",
  notes = "Complete-tree repeated-measures analysis"
)

##Step: Save long form table------

primary_aov_stats_long <- bind_rows(
  dbh_growth_stats,
  A_stats,
  gsw_stats,
  E_stats,
  iWUE_stats,
  lma_stats,
  stomatal_density_stats,
  total_chl_stats,
  wp_stats,
  nitro_stats,
  CN_stats,
  c13_stats
)

write_csv(
  primary_aov_stats_long,
  "figures/charles_town_primary_aov_stats_long.csv"
)

##Step: Make wide format table-----
primary_aov_p_table <- primary_aov_stats_long %>%
  select(trait_group, trait, scale, term, p_text) %>%
  pivot_wider(
    names_from = term,
    values_from = p_text
  )

write_csv(
  primary_aov_p_table,
  "figures/charles_town_primary_aov_p_table.csv"
)

