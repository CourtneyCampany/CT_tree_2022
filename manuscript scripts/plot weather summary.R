-
# Charles Town urban tree physiology study
# Baseline establishment-period weather figure


library(tidyverse)
library(lubridate)
library(patchwork)

weather_establishment_plot <- read.csv(
  "calculated_data/CharlesTown_weather_establishment_period.csv",
  stringsAsFactors = FALSE
) |>
  mutate(
    date = as.Date(date),
    irrigated_today = as.logical(irrigated_today),
    hot_day_90F = as.logical(hot_day_90F),
    hot_period_90F_3d = as.logical(hot_period_90F_3d)
  )

# Key study dates
planting_date <- as.Date("2022-05-02")
first_measurement_date <- as.Date("2022-05-23")
final_measurement_date <- as.Date("2022-07-28")

# Sustained multi-day hot period identified from daily Tmax >= 90 F
hot_period_start <- as.Date("2022-07-21")
hot_period_end <- as.Date("2022-07-24")


irrigation_plot_data <- weather_establishment_plot |>
  filter(irrigated_today) |>
  select(
    date,
    irrigation_L_per_tree,
    irrigation_schedule_basis
  )

# Step: Build baseline environmental figure panels-----


# Background shading for the sustained hot period.
# The end date is extended by one day so that all of July 24
# is visually included in the shaded interval.
hot_period_shading <- tibble(
  xmin = as.Date("2022-07-21"),
  xmax = as.Date("2022-07-25"),
  ymin = -Inf,
  ymax = Inf
)

# Common formatting for all weather panels
weather_theme <- theme_classic(base_size = 11) +
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0),
    plot.margin = margin(5, 8, 5, 8)
  )

temperature_panel <- ggplot(
  weather_establishment_plot,
  aes(x = date)
) +
  # Shade the sustained late-July hot period
  geom_rect(
    data = hot_period_shading,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = FALSE,
    fill = "grey92"
  ) +
  
  # Daily minimum-to-maximum temperature range
  geom_ribbon(
    aes(
      ymin = tmin_C,
      ymax = tmax_C
    ),
    fill = "grey75",
    alpha = 0.8
  ) +
  
  # Daily mean air temperature
  geom_line(
    aes(y = tavg_C),
    linewidth = 0.6
  ) +
  
  # Beginning of repeated physiological measurements
  geom_vline(
    xintercept = first_measurement_date,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  
  scale_x_date(
    limits = c(planting_date, final_measurement_date),
    date_breaks = "2 weeks",
    date_labels = "%b %d",
    expand = expansion(mult = c(0.005, 0.005))
  ) +
  
  labs(
    x = NULL,
    y = expression("Air temperature (" * degree * "C)"),
    title = "A"
  ) +
  
  weather_theme

temperature_panel

ggsave(
  filename = "figures/weather_temperature_panel_draft.png",
  plot = temperature_panel,
  width = 7,
  height = 3,
  units = "in",
  dpi = 300
)


# Step: Create daily VPD panel-----

vpd_panel <- ggplot(
  weather_establishment_plot,
  aes(x = date)
) +
  # Shade the sustained late-July hot period
  geom_rect(
    data = hot_period_shading,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = FALSE,
    fill = "grey92"
  ) +

  # Daily VPD calculated from daily mean temperature and RH
  geom_line(
    aes(y = vpd_daily_kPa),
    linewidth = 0.6
  ) +
  
  # Beginning of repeated physiological measurements
  geom_vline(
    xintercept = first_measurement_date,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  
  scale_x_date(
    limits = c(planting_date, final_measurement_date),
    date_breaks = "2 weeks",
    date_labels = "%b %d",
    expand = expansion(mult = c(0.005, 0.005))
  ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = NULL,
    y = expression("Daily VPD (kPa)"),
    title = "B"
  ) +
  
  weather_theme

vpd_panel
  
ggsave(
  filename = "figures/weather_VPD_panel_draft.png",
  plot = vpd_panel,
  width = 7,
  height = 2.5,
  units = "in",
  dpi = 300
)  
  
  