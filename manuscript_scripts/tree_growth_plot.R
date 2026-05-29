source("scripts/functions.R")
source("scripts/plot_objects.R")

##Step 1: read and prepare the data

dbh_plot<- read.csv("raw_data/dbh_clean.csv")
  # Format variables
  dbh_plot$date <- as.Date(
    dbh_plot$date,
    format = "%m/%d/%Y"
  )
  
  dbh_plot$site <- factor(dbh_plot$site)
  dbh_plot$species <- factor(dbh_plot$species)
  dbh_plot$replicate <- factor(dbh_plot$replicate)
  
  # Create tree identifier
  dbh_plot$tree_id <- with(
    dbh_plot,
    paste(site, species, replicate, sep = "-")
  )
  
  # Sort by tree and date
  dbh_plot <- dbh_plot[
    order(dbh_plot$tree_id, dbh_plot$date),
  ]
  
  # Create within-tree measurement week
  dbh_plot$week <- ave(
    dbh_plot$dbh_mm,
    dbh_plot$tree_id,
    FUN = seq_along
  )
  
  # Calculate DBH change from initial measurement for each tree
  dbh_plot$initial_dbh_mm <- ave(
    dbh_plot$dbh_mm,
    dbh_plot$tree_id,
    FUN = function(x) x[1]
  )
  
  dbh_plot$dbh_change_mm <- 
    dbh_plot$dbh_mm - dbh_plot$initial_dbh_mm
  
##plotting
  
growth_plot_summary <- do.call(
    rbind,
    lapply(
      split(
        dbh_plot,
        list(dbh_plot$site, dbh_plot$species, dbh_plot$week),
        drop = TRUE
      ),
      function(x) {
        data.frame(
          site = unique(x$site),
          species = unique(x$species),
          week = unique(x$week),
          n = nrow(x),
          mean_change_mm = mean(x$dbh_change_mm),
          sd_change_mm = sd(x$dbh_change_mm),
          se_change_mm = sd(x$dbh_change_mm) / sqrt(nrow(x))
        )
      }
    )
  )
  
  row.names(growth_plot_summary) <- NULL
  
  growth_plot_summary <- growth_plot_summary[
    order(
      growth_plot_summary$site,
      growth_plot_summary$species,
      growth_plot_summary$week
    ),
  ]

## PLOTTING

mean_growth <- aggregate(
  dbh_change_from_initial ~ site + species + week,
  data = dbh,
  FUN = mean
)

se_growth <- aggregate(
  dbh_change_from_initial ~ site + species + week,
  data = dbh,
  FUN = function(x) sd(x) / sqrt(length(x))
)

names(mean_growth)[4] <- "mean_change"
names(se_growth)[4] <- "se_change"

growth_summary <- merge(
  mean_growth,
  se_growth,
  by = c("site", "species", "week")
)

growth_summary <- growth_summary[order(growth_summary$site,
                                       growth_summary$species,
                                       growth_summary$week), ]



# Relabel planting sites for presentation
growth_plot_summary$species_label <- factor(
  growth_plot_summary$species,
  levels = c("d", "h", "m"),
  labels = c("Dogwood", "Hawthorn", "Maple")
)

dbh_growth_fig <- ggplot(
  growth_plot_summary,
  aes(
    x = week,
    y = mean_change_mm,
    color = species_label,
    shape = species_label,
    group = species_label
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    color = "grey40"
  ) +
  geom_errorbar(
    aes(
      ymin = mean_change_mm - se_change_mm,
      ymax = mean_change_mm + se_change_mm
    ),
    width = 0.12,
    linewidth = 0.45
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2.2,
    stroke = 0.8
  ) +
  facet_wrap(
    ~ site_label,
    nrow = 1
  ) +
  scale_x_continuous(
    breaks = 1:10
  ) +
  scale_color_manual(
    values = c(
      "Dogwood" = "#0072B2",
      "Hawthorn" = "#D55E00",
      "Maple" = "#009E73"
    ),
    name = "Species"
  ) +
  scale_shape_manual(
    values = c(
      "Dogwood" = 16,
      "Hawthorn" = 17,
      "Maple" = 15
    ),
    name = "Species"
  ) +
  labs(
    x = "Measurement week",
    y = "Change in DBH from initial measurement (mm)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.spacing = unit(1.2, "lines")
  )

dbh_growth_fig


ggsave(
  filename = "Figure_stem_growth_trajectory.tiff",
  plot = dbh_growth_fig,
  width = 7.5,
  height = 4.5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  filename = "Figure_stem_growth_trajectory.png",
  plot = dbh_growth_fig,
  width = 7.5,
  height = 4.5,
  units = "in",
  dpi = 300
)