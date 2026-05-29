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

# Step: figure panel defaults-----


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


# Step: Daily Air temperature panel----
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
  filename = "figures/weather_temperature_panel.png",
  plot = temperature_panel,
  width = 7,
  height = 3,
  units = "in",
  dpi = 300
)



# Step: Daily VPD panel-----

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
  filename = "figures/weather_VPD_panel.png",
  plot = vpd_panel,
  width = 7,
  height = 2.5,
  units = "in",
  dpi = 300
)  


# Step: Daily Precipitation panel (not used)-----

# precipitation_panel <- ggplot(
#   weather_establishment_plot,
#   aes(x = date)
# ) +
#   # Shade the sustained late-July hot period
#   geom_rect(
#     data = hot_period_shading,
#     aes(
#       xmin = xmin,
#       xmax = xmax,
#       ymin = ymin,
#       ymax = ymax
#     ),
#     inherit.aes = FALSE,
#     fill = "grey92"
#   ) +
#   
#   # Daily measurable precipitation recorded at the NOAA station
#   geom_col(
#     aes(y = precip_mm),
#     width = 0.85,
#     fill = "grey35"
#   ) +
#   
#   # Beginning of repeated physiological measurements
#   geom_vline(
#     xintercept = first_measurement_date,
#     linetype = "dashed",
#     linewidth = 0.4
#   ) +
#   
#   scale_x_date(
#     limits = c(planting_date, final_measurement_date),
#     date_breaks = "2 weeks",
#     date_labels = "%b %d",
#     expand = expansion(mult = c(0.005, 0.005))
#   ) +
#   
#   scale_y_continuous(
#     limits = c(0, NA),
#     expand = expansion(mult = c(0, 0.05))
#   ) +
#   
#   labs(
#     x = NULL,
#     y = "Precipitation (mm)",
#     title = "C"
#   ) +
#   
#   weather_theme
# 
# precipitation_panel
# 
# ggsave(
#   filename = "figures/weather_precipitation_panel_draft.png",
#   plot = precipitation_panel,
#   width = 7,
#   height = 2.5,
#   units = "in",
#   dpi = 300
# )



# Step: Precipitation + Supplemental irrigation panel (not used)-----

# irrigation_marker_y <- max(weather_establishment_plot$precip_mm, na.rm = TRUE) * 1.08
# 
# irrigation_plot_data <- weather_establishment_plot |>
#   filter(irrigated_today) |>
#   mutate(
#     irrigation_marker_y = irrigation_marker_y,
#     irrigation_type = case_when(
#       irrigation_schedule_basis == "shifted_from_Monday_federal_holiday" ~
#         "Holiday-shifted irrigation",
#       TRUE ~
#         "Scheduled irrigation"
#     )
#   )
# 
# 
# precipitation_panel2 <- ggplot(
#   weather_establishment_plot,
#   aes(x = date)
# ) +
#   # Shade the sustained late-July hot period
#   geom_rect(
#     data = hot_period_shading,
#     aes(
#       xmin = xmin,
#       xmax = xmax,
#       ymin = ymin,
#       ymax = ymax
#     ),
#     inherit.aes = FALSE,
#     fill = "grey92"
#   ) +
#   
#   # Daily measurable precipitation recorded at the NOAA station
#   geom_col(
#     aes(y = precip_mm),
#     width = 0.85,
#     fill = "grey35"
#   ) +
#   
#   # Supplemental irrigation-event markers.
#   # Marker height is graphical only; irrigation was applied as
#   # 18.93 L tree-1 and is not expressed as rainfall-equivalent mm.
#   geom_point(
#     data = irrigation_plot_data,
#     aes(
#       x = date,
#       y = irrigation_marker_y
#     ),
#     inherit.aes = FALSE,
#     shape = 25,
#     size = 2.5,
#     fill = "black",
#     color = "black"
#   ) +
#   
#   # Beginning of repeated physiological measurements
#   geom_vline(
#     xintercept = first_measurement_date,
#     linetype = "dashed",
#     linewidth = 0.4
#   ) +
#   
#   scale_x_date(
#     limits = c(planting_date, final_measurement_date),
#     date_breaks = "2 weeks",
#     date_labels = "%b %d",
#     expand = expansion(mult = c(0.005, 0.005))
#   ) +
#   
#   scale_y_continuous(
#     limits = c(0, irrigation_marker_y * 1.07),
#     expand = expansion(mult = c(0, 0))
#   ) +
#   
#   annotate(
#     "text",
#     x = planting_date + 2,
#     y = irrigation_marker_y,
#     label = "\u25BC  irrigation",
#     hjust = 0,
#     vjust = -0.2,
#     size = 3.2
#   ) +
#   
#   labs(
#     x = "Date",
#     y = "Precipitation (mm)",
#     title = "C"
#   ) +
#   
#   weather_theme
# 
# precipitation_panel2



# Step: Precipitation + Supplemental water drop irrigation panel-----

irrigation_marker_y <- max(weather_establishment_plot$precip_mm, na.rm = TRUE) * 1.08

precipitation_panel3 <- ggplot(
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
  
  # Daily measurable precipitation recorded at the NOAA station
  geom_col(
    aes(y = precip_mm),
    width = 0.85,
    fill = "grey35"
  ) +
  
  # Supplemental irrigation-event markers.
  # Marker height is graphical only; irrigation was applied as
  # 18.93 L tree-1 and is not expressed as rainfall-equivalent mm.
  geom_text(
    data = irrigation_plot_data,
    aes(
      x = date,
      y = irrigation_marker_y,
      label = "\U0001F4A7"
    ),
    inherit.aes = FALSE,
    size = 4
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
    limits = c(0, irrigation_marker_y * 1.07),
    expand = expansion(mult = c(0, 0))
  ) +
  
  annotate(
    "text",
    x = planting_date + 60,
    y = irrigation_marker_y-6,
    label = "\U0001F4A7  irrigation",
    hjust = 0,
    vjust = -0.2,
    size = 3.2
  ) +
  
  labs(
    x = "Date",
    y = "Precipitation (mm)",
    title = "C"
  ) +
  
  weather_theme

precipitation_panel3

ggsave(
  filename = "figures/weather_precipitation_panel.png",
  plot = precipitation_panel,
  width = 7,
  height = 2.5,
  units = "in",
  dpi = 300
)


#Step: Make panel figure-----

# Upper panels do not need repeated date-axis labels
temperature_panel_combined <- temperature_panel +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

vpd_panel_combined <- vpd_panel +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

# Retain date-axis labels only on the bottom panel
precipitation_panel_combined <- precipitation_panel3 +
  labs(x = "Date")


weather_summary_figure <- 
  temperature_panel_combined /
  vpd_panel_combined /
  precipitation_panel_combined +
  plot_layout(
    heights = c(1.3, 1, 1.15)
  )

weather_summary_figure

ggsave(
  filename = "figures/CharlesTown_weather_summary_figure_draft.png",
  plot = weather_summary_figure,
  width = 7.2,
  height = 7.0,
  units = "in",
  dpi = 300
)

ggsave(
  filename = "figures/CharlesTown_weather_summary_figure_draft.pdf",
  plot = weather_summary_figure,
  width = 7.2,
  height = 7.0,
  units = "in"
)


##water drop likely will not render on PDF, go back to basic symbol

##if I want to remove the starting measurment date, comment out "geom_vline"
