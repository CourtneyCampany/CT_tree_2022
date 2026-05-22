# Step 1 read and tidy the raw NOAA dataset------

library(tidyverse)
library(lubridate)
library(dplyr)
library(slider)

lcd_raw <- read.csv(
  "raw_data/NOAA_dataset_martinsburg3.csv",
  na.strings = c("", "NA"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Rename duplicated NOAA columns by their positions in the fil
names(lcd_raw)[c(3, 4, 47, 48)] <- c(
  "report_type",
  "source",
  "report_type_duplicate",
  "source_duplicate"
)

# Convert date-time and clean report-type labels
lcd_raw <- lcd_raw |>
  mutate(
    datetime = lubridate::ymd_hms(DATE),
    date = as.Date(datetime),
    report_type = stringr::str_trim(report_type),
    report_type_duplicate = stringr::str_trim(report_type_duplicate)
  )


# Step 2: Extract daily-summary weather record-----

weather_daily_raw <- lcd_raw |>
  filter(report_type == "SOD") |>
  select(
    date,
    DailyAverageDryBulbTemperature,
    DailyMaximumDryBulbTemperature,
    DailyMinimumDryBulbTemperature,
    DailyAverageRelativeHumidity,
    DailyAverageDewPointTemperature,
    DailyPrecipitation,
    DailyAverageWindSpeed,
    DailyPeakWindSpeed,
    DailyWeather
  ) |>
  rename(
    tavg_F = DailyAverageDryBulbTemperature,
    tmax_F = DailyMaximumDryBulbTemperature,
    tmin_F = DailyMinimumDryBulbTemperature,
    rh_mean_pct = DailyAverageRelativeHumidity,
    dewpoint_mean_F = DailyAverageDewPointTemperature,
    precip_original_in = DailyPrecipitation,
    wind_mean_mph = DailyAverageWindSpeed,
    wind_peak_mph = DailyPeakWindSpeed,
    weather_code = DailyWeather
  )


# Check for duplicate dates
weather_daily_raw |>
  count(date) |>
  filter(n > 1)

# Identify dates missing from the daily-summary records
all_dates <- tibble(
  date = seq(
    as.Date("2022-04-01"),
    as.Date("2022-08-31"),
    by = "day"
  )
)

all_dates |>
  anti_join(weather_daily_raw, by = "date")

#Inspect missing values in candidate weather variables
weather_daily_raw |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

#Examine precipitation values, including trace events
weather_daily_raw |>
  count(precip_original_in, sort = TRUE)

weather_daily_raw |>
  summarise(
    n_trace_precip_days = sum(precip_original_in == "T", na.rm = TRUE)
  )


# Step 3: Convert units and calculate daily VPD-----

weather_daily_clean <- weather_daily_raw |>
  mutate(
    # Identify source of these daily rows
    daily_source = "NOAA_SOD",
    
    # Ensure core variables are numeric
    tavg_F = as.numeric(tavg_F),
    tmax_F = as.numeric(tmax_F),
    tmin_F = as.numeric(tmin_F),
    rh_mean_pct = as.numeric(rh_mean_pct),
    dewpoint_mean_F = as.numeric(dewpoint_mean_F),
    wind_mean_mph = as.numeric(wind_mean_mph),
    
    # Preserve original precipitation reporting
    precip_original_in = stringr::str_trim(as.character(precip_original_in)),
    precip_trace = precip_original_in == "T",
    
    # Replace trace precipitation with "0" before numeric conversion
    # while preserving trace events in precip_trace
    precip_in = as.numeric(
      replace(precip_original_in, precip_trace, "0")
    ),
    
    # Temperature conversion: degrees F to degrees C
    tavg_C = (tavg_F - 32) * 5 / 9,
    tmax_C = (tmax_F - 32) * 5 / 9,
    tmin_C = (tmin_F - 32) * 5 / 9,
    dewpoint_mean_C = (dewpoint_mean_F - 32) * 5 / 9,
    
    # Precipitation conversion: inches to millimeters
    precip_mm = precip_in * 25.4,
    
    # Optional wind conversion: miles per hour to meters per second
    wind_mean_ms = wind_mean_mph * 0.44704,
    
    # Saturation vapor pressure calculated from daily mean air temperature
    es_mean_kPa = 0.6108 * exp((17.27 * tavg_C) / (tavg_C + 237.3)),
    
    # Daily VPD calculated from daily mean temperature and RH
    vpd_daily_kPa = es_mean_kPa * (1 - rh_mean_pct / 100)
  ) |>
  select(
    date,
    daily_source,
    tavg_C,
    tmax_C,
    tmin_C,
    rh_mean_pct,
    dewpoint_mean_C,
    vpd_daily_kPa,
    precip_mm,
    precip_trace,
    precip_original_in,
    wind_mean_ms,
    weather_code
  )

# Step 4: Reconstruct missing daily record for 2022-05-31------

may31_hourly <- lcd_raw |>
  filter(
    report_type == "FM-15",
    date == as.Date("2022-05-31")
  ) |>
  transmute(
    datetime,
    date,
    tair_F = as.numeric(HourlyDryBulbTemperature),
    rh_pct = as.numeric(HourlyRelativeHumidity),
    dewpoint_F = as.numeric(HourlyDewPointTemperature),
    precip_hourly_original_in = as.character(HourlyPrecipitation),
    precip_hourly_in = suppressWarnings(as.numeric(HourlyPrecipitation)),
    wind_mph = as.numeric(HourlyWindSpeed)
  )

#Create the reconstructed daily row
may31_reconstructed <- may31_hourly |>
  summarise(
    date = as.Date("2022-05-31"),
    daily_source = "FM-15_hourly_reconstruction",
    
    # Reconstructed thermal and humidity variables
    tavg_C = (mean(tair_F, na.rm = TRUE) - 32) * 5 / 9,
    tmax_C = (max(tair_F, na.rm = TRUE) - 32) * 5 / 9,
    tmin_C = (min(tair_F, na.rm = TRUE) - 32) * 5 / 9,
    rh_mean_pct = mean(rh_pct, na.rm = TRUE),
    dewpoint_mean_C = (mean(dewpoint_F, na.rm = TRUE) - 32) * 5 / 9,
    
    # No measurable precipitation was observed in hourly METAR records.
    # Nineteen hourly precipitation fields reported 0 in, and the
    # remaining five hourly METAR reports indicated clear conditions
    # without precipitation weather codes.
    precip_mm = 0,
    precip_trace = FALSE,
    precip_original_in = NA_character_,
    
    # Optional wind variable reconstructed from complete hourly data
    wind_mean_ms = mean(wind_mph, na.rm = TRUE) * 0.44704,
    
    # No official daily weather code exists for this reconstructed row
    weather_code = NA_character_
  ) |>
  mutate(
    es_mean_kPa = 0.6108 * exp((17.27 * tavg_C) / (tavg_C + 237.3)),
    vpd_daily_kPa = es_mean_kPa * (1 - rh_mean_pct / 100)
  ) |>
  select(
    date,
    daily_source,
    tavg_C,
    tmax_C,
    tmin_C,
    rh_mean_pct,
    dewpoint_mean_C,
    vpd_daily_kPa,
    precip_mm,
    precip_trace,
    precip_original_in,
    wind_mean_ms,
    weather_code
  )

#Append the reconstructed row to the daily dataset

weather_daily_complete <- weather_daily_clean |>
  bind_rows(may31_reconstructed) |>
  arrange(date)

# Step 5: Finalize cleaned daily weather dataset-----

weather_daily_final <- weather_daily_complete |>
  mutate(
    station_name = "Martinsburg Eastern WV Regional Airport",
    station_id = "USW00013734",
    
    temp_rh_status = case_when(
      daily_source == "FM-15_hourly_reconstruction" ~ 
        "reconstructed_from_complete_hourly_records",
      TRUE ~ "official_daily_summary"
    ),
    
    precip_status = case_when(
      daily_source == "FM-15_hourly_reconstruction" ~
        "validated_0_mm_from_hourly_METAR_records",
      precip_trace == TRUE ~
        "trace_reported_as_0_mm",
      TRUE ~
        "official_daily_summary"
    )
  ) |>
  select(
    date,
    station_id,
    station_name,
    daily_source,
    temp_rh_status,
    precip_status,
    tavg_C,
    tmax_C,
    tmin_C,
    rh_mean_pct,
    dewpoint_mean_C,
    vpd_daily_kPa,
    precip_mm,
    precip_trace,
    precip_original_in,
    wind_mean_ms,
    weather_code
  ) |>
  arrange(date)

#save cleanned dataset
write.csv(
  weather_daily_final,
  file = "calculated_data/CharlesTown_weather_daily_cleaned.csv",
  row.names = FALSE,
  na = ""
)


# Step 6: Define study periods and prepare derived weather data-----

# Key biological dates for the experiment
planting_date <- as.Date("2022-05-02")
first_measurement_date <- as.Date("2022-05-23")
final_measurement_date <- as.Date("2022-07-28")

weather_daily_derived <- weather_daily_final |>
  arrange(date) |>
  mutate(
    # Identify dates within the full tree-establishment interval
    in_establishment_period = 
      date >= planting_date & date <= final_measurement_date,
    
    # Identify dates during active physiological measurements
    in_measurement_period = 
      date >= first_measurement_date & date <= final_measurement_date,
    
    # Timing variables useful for later plotting or summaries
    days_since_planting = as.integer(date - planting_date),
    days_since_first_measurement = as.integer(date - first_measurement_date)
  )


# Step 7: Calculate antecedent precipitation variables-----

weather_daily_derived |>
  summarise(
    n_days = n(),
    missing_precipitation = sum(is.na(precip_mm)),
    total_precipitation_mm = sum(precip_mm)
  )

#rolling sum function:
# Return NA if a rolling window contains missing data;
# otherwise calculate the precipitation total.
#This function prevents incomplete rainfall windows from 
#accidentally being treated as complete totals.
sum_complete <- function(x) {
  if (any(is.na(x))) {
    return(NA_real_)
  } else {
    return(sum(x))
  }
}


weather_daily_derived <- weather_daily_derived |>
  arrange(date) |>
  mutate(
    # Measurable precipitation excludes trace precipitation events,
    # which are retained as meteorological events but coded as 0 mm.
    measurable_precip = precip_mm > 0,
    
    # Total precipitation during the seven complete days
    # preceding each date; current-day rainfall is excluded.
    precip_prior_7d_mm = slider::slide_dbl(
      lag(precip_mm),
      sum_complete,
      .before = 6,
      .complete = TRUE
    ),
    
    # Total precipitation during the fourteen complete days
    # preceding each date; current-day rainfall is excluded.
    precip_prior_14d_mm = slider::slide_dbl(
      lag(precip_mm),
      sum_complete,
      .before = 13,
      .complete = TRUE
    )
  )


# Step 8: Calculate days since measurable precipitation----

weather_daily_derived$days_since_measurable_precip_prior <- purrr::map_int(
  seq_along(weather_daily_derived$date),
  function(i) {
    
    # The first date has no preceding weather record
    if (i == 1) {
      return(NA_integer_)
    }
    
    # Identify rows preceding the current date
    prior_rows <- seq_len(i - 1)
    
    # Identify previous dates with measurable precipitation
    previous_measurable_rain_rows <- prior_rows[
      weather_daily_derived$measurable_precip[prior_rows] == TRUE
    ]
    
    # If no earlier measurable rainfall occurs in the available
    # master record, the drying interval cannot be determined
    if (length(previous_measurable_rain_rows) == 0) {
      return(NA_integer_)
    }
    
    # Find the most recent prior measurable rainfall date
    last_rain_row <- max(previous_measurable_rain_rows)
    
    # Count complete intervening days between the last measurable
    # rainfall event and the current date
    as.integer(
      weather_daily_derived$date[i] -
        weather_daily_derived$date[last_rain_row] - 1
    )
  }
)

#Identify the longest drying intervals during establishment
weather_daily_derived |>
  filter(in_establishment_period) |>
  summarise(
    maximum_days_since_measurable_precip =
      max(days_since_measurable_precip_prior, na.rm = TRUE)
  )
#dates of the long dry intervals
weather_daily_derived |>
  filter(
    in_establishment_period,
    days_since_measurable_precip_prior ==
      max(days_since_measurable_precip_prior[in_establishment_period],
          na.rm = TRUE)
  ) |>
  select(
    date,
    precip_mm,
    precip_prior_7d_mm,
    days_since_measurable_precip_prior
  ) |>
  mutate(
    across(
      c(precip_mm, precip_prior_7d_mm),
      ~ round(.x, 2)
    )
  )


# Step 9: Identify hot days and sustained multi-day hot periods-----


# Operational threshold for descriptive high-temperature events:
# daily maximum air temperature of at least 90 degrees F.
hot_day_threshold_C <- (90 - 32) * 5 / 9

# Minimum duration used to identify a sustained multi-day hot period
hot_period_min_days <- 3

weather_daily_derived <- weather_daily_derived |>
  arrange(date) |>
  mutate(
    # Individual high-temperature days
    hot_day_90F = tmax_C >= hot_day_threshold_C,
    
    # Assign an identifier to each sequence of consecutive hot days
    hot_period_id = if_else(
      hot_day_90F,
      cumsum(hot_day_90F & !lag(hot_day_90F, default = FALSE)),
      NA_integer_
    )
  ) |>
  group_by(hot_period_id) |>
  mutate(
    # Number of consecutive hot days in each sequence
    hot_period_length_days = if_else(
      hot_day_90F,
      n(),
      0L
    ),
    
    # Flag days belonging to a sustained hot period
    hot_period_90F_3d = 
      hot_day_90F & hot_period_length_days >= hot_period_min_days
  ) |>
  ungroup()

#summarize hot day exposure
weather_daily_derived |>
  filter(in_establishment_period) |>
  summarise(
    n_establishment_days = n(),
    n_hot_days_90F = sum(hot_day_90F),
    n_days_in_sustained_hot_period = sum(hot_period_90F_3d),
    maximum_tmax_C = max(tmax_C)
  ) |>
  mutate(
    maximum_tmax_C = round(maximum_tmax_C, 2)
  )

#The reconstructed 31 May row contributes to a two-day hot sequence 
#but does not determine the sustained hot-period result; the only ≥3-day 
#hot period is based on official daily-summary rows from 21–24 July.

weather_daily_derived |>
  filter(
    date >= as.Date("2022-07-19"),
    date <= as.Date("2022-07-26")
  ) |>
  select(
    date,
    tmax_C,
    vpd_daily_kPa,
    precip_mm,
    hot_day_90F,
    hot_period_length_days,
    hot_period_90F_3d
  ) |>
  mutate(
    across(
      c(tmax_C, vpd_daily_kPa, precip_mm),
      ~ round(.x, 2)
    )
  )


# Step 10: Calculate antecedent thermal and VPD variables----


# Return NA when an antecedent window is incomplete or contains
# missing observations; otherwise calculate the requested summary.

mean_complete <- function(x) {
  if (any(is.na(x))) {
    return(NA_real_)
  } else {
    return(mean(x))
  }
}

max_complete <- function(x) {
  if (any(is.na(x))) {
    return(NA_real_)
  } else {
    return(max(x))
  }
}

sum_integer_complete <- function(x) {
  if (any(is.na(x))) {
    return(NA_integer_)
  } else {
    return(as.integer(sum(x)))
  }
}


weather_daily_derived <- weather_daily_derived |>
  arrange(date) |>
  mutate(
    # Mean temperature during the seven complete days
    # preceding the current date
    tavg_prior_7d_mean_C = slider::slide_dbl(
      lag(tavg_C),
      mean_complete,
      .before = 6,
      .complete = TRUE
    ),
    
    # Highest daily maximum temperature during the seven
    # complete days preceding the current date
    tmax_prior_7d_max_C = slider::slide_dbl(
      lag(tmax_C),
      max_complete,
      .before = 6,
      .complete = TRUE
    ),
    
    # Mean atmospheric evaporative demand during the seven
    # complete days preceding the current date
    vpd_prior_7d_mean_kPa = slider::slide_dbl(
      lag(vpd_daily_kPa),
      mean_complete,
      .before = 6,
      .complete = TRUE
    ),
    
    # Highest daily VPD during the seven complete days
    # preceding the current date
    vpd_prior_7d_max_kPa = slider::slide_dbl(
      lag(vpd_daily_kPa),
      max_complete,
      .before = 6,
      .complete = TRUE
    ),
    
    # Number of hot days during the seven complete days
    # preceding the current date
    hot_days_prior_7d_n = slider::slide_int(
      lag(as.integer(hot_day_90F)),
      sum_integer_complete,
      .before = 6,
      .complete = TRUE
    )
  )



# Step 11: Add documented supplemental irrigation events-----

# Each tree received 5 US gallons of supplemental water weekly.
# Irrigation occurred on Monday unless Monday was a federal holiday,
# in which case irrigation occurred on Tuesday.

irrigation_volume_gal_per_tree <- 5
irrigation_volume_L_per_tree <- irrigation_volume_gal_per_tree * 3.785411784

irrigation_schedule <- tibble(
  irrigation_date = as.Date(c(
    "2022-05-02",
    "2022-05-09",
    "2022-05-16",
    "2022-05-23",
    "2022-05-31",  # shifted from Memorial Day, Monday 2022-05-30
    "2022-06-06",
    "2022-06-13",
    "2022-06-21",  # shifted from observed Juneteenth, Monday 2022-06-20
    "2022-06-27",
    "2022-07-05",  # shifted from Independence Day, Monday 2022-07-04
    "2022-07-11",
    "2022-07-18",
    "2022-07-25"
  )),
  
  irrigation_schedule_basis = c(
    "scheduled_Monday",
    "scheduled_Monday",
    "scheduled_Monday",
    "scheduled_Monday",
    "shifted_from_Monday_federal_holiday",
    "scheduled_Monday",
    "scheduled_Monday",
    "shifted_from_Monday_federal_holiday",
    "scheduled_Monday",
    "shifted_from_Monday_federal_holiday",
    "scheduled_Monday",
    "scheduled_Monday",
    "scheduled_Monday"
  )
) |>
  mutate(
    irrigation_gal_per_tree = irrigation_volume_gal_per_tree,
    irrigation_L_per_tree = irrigation_volume_L_per_tree
  )

#join to weather dataset

weather_daily_derived <- weather_daily_derived |>
  left_join(
    irrigation_schedule,
    by = c("date" = "irrigation_date")
  ) |>
  mutate(
    # Irrigation is documented only during the establishment period.
    # Dates outside this period are coded NA rather than FALSE or zero.
    irrigated_today = case_when(
      in_establishment_period & !is.na(irrigation_gal_per_tree) ~ TRUE,
      in_establishment_period ~ FALSE,
      TRUE ~ NA
    ),
    
    irrigation_gal_per_tree = case_when(
      irrigated_today == TRUE ~ irrigation_gal_per_tree,
      irrigated_today == FALSE ~ 0,
      TRUE ~ NA_real_
    ),
    
    irrigation_L_per_tree = case_when(
      irrigated_today == TRUE ~ irrigation_L_per_tree,
      irrigated_today == FALSE ~ 0,
      TRUE ~ NA_real_
    ),
    
    irrigation_schedule_basis = case_when(
      irrigated_today == TRUE ~ irrigation_schedule_basis,
      irrigated_today == FALSE ~ "no_scheduled_irrigation",
      TRUE ~ "outside_documented_irrigation_period"
    )
  )

# Days since the most recent irrigation event before the current date.
# Current-day irrigation is excluded, consistent with the antecedent
# precipitation and VPD variables.

weather_daily_derived$days_since_irrigation_prior <- purrr::map2_int(
  weather_daily_derived$date,
  weather_daily_derived$in_establishment_period,
  function(current_date, in_period) {
    
    if (!in_period) {
      return(NA_integer_)
    }
    
    prior_irrigation_dates <- irrigation_schedule$irrigation_date[
      irrigation_schedule$irrigation_date < current_date
    ]
    
    if (length(prior_irrigation_dates) == 0) {
      return(NA_integer_)
    }
    
    as.integer(
      current_date - max(prior_irrigation_dates) - 1
    )
  }
)

# Number of irrigation events during the seven complete days
# preceding the current date.

weather_daily_derived$irrigation_events_prior_7d_n <- purrr::map2_int(
  weather_daily_derived$date,
  weather_daily_derived$in_establishment_period,
  function(current_date, in_period) {
    
    if (!in_period) {
      return(NA_integer_)
    }
    
    sum(
      irrigation_schedule$irrigation_date >= current_date - 7 &
        irrigation_schedule$irrigation_date < current_date
    )
  }
)

# Step 12: Create study-period derived weather datasets-----

weather_establishment_period <- weather_daily_derived |>
  filter(in_establishment_period) |>
  arrange(date)

weather_measurement_period <- weather_daily_derived |>
  filter(in_measurement_period) |>
  arrange(date)


# Step 13: Summarize and export derived weather datasets-----

weather_establishment_summary <- weather_establishment_period |>
  summarise(
    period_start = min(date),
    period_end = max(date),
    n_days = n(),
    
    total_precip_mm = sum(precip_mm),
    trace_precip_days = sum(precip_trace),
    measurable_precip_days = sum(measurable_precip),
    maximum_drying_interval_days =
      max(days_since_measurable_precip_prior),
    
    mean_tavg_C = mean(tavg_C),
    maximum_tmax_C = max(tmax_C),
    minimum_tmin_C = min(tmin_C),
    
    mean_daily_vpd_kPa = mean(vpd_daily_kPa),
    maximum_daily_vpd_kPa = max(vpd_daily_kPa),
    
    hot_days_90F = sum(hot_day_90F),
    days_in_sustained_hot_period = sum(hot_period_90F_3d)
  )

weather_establishment_summary |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  ) |>
  print(width = Inf)


# Export full derived daily-weather record, now including irrigation variables
write.csv(
  weather_daily_derived,
  file = "calculated_data/CharlesTown_weather_daily_derived_full.csv",
  row.names = FALSE,
  na = ""
)

# Export establishment-period dataset, now including irrigation variables
write.csv(
  weather_establishment_period,
  file = "calculated_data/CharlesTown_weather_establishment_period.csv",
  row.names = FALSE,
  na = ""
)

# Export active measurement-period dataset, now including irrigation variables
write.csv(
  weather_measurement_period,
  file = "calculated_data/CharlesTown_weather_measurement_period.csv",
  row.names = FALSE,
  na = ""
)

# Export revised establishment-period summary, now including irrigation totals
write.csv(
  weather_establishment_summary,
  file = "calculated_data/CharlesTown_weather_establishment_summary.csv",
  row.names = FALSE,
  na = ""
)
