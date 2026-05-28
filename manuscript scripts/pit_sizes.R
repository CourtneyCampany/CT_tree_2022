pit_sizes <- read.csv("raw_data/pit_sizes.csv")

pit_sizes$site <- factor(pit_sizes$site)
pit_sizes$species <- factor(pit_sizes$species)
pit_sizes$replicate <- factor(pit_sizes$replicate)
pit_sizes$pit_size_class <- factor(pit_sizes$pit_size_class)

pit_sizes$tree_id <- with(
  pit_sizes,
  paste(site, species, replicate, sep = "_")
)

pit_sizes$pit_area_m2 <- with(
  pit_sizes,
  (pit_length_cm / 100) * (pit_width_cm / 100)
)

pit_sizes$pit_volume_m3 <- with(
  pit_sizes,
  (pit_length_cm / 100) *
    (pit_width_cm / 100) *
    (pit_depth_cm / 100)
)

str(pit_sizes)

unique(
  subset(
    pit_sizes,
    site == "c",
    select = c(
      pit_size_class,
      pit_length_cm,
      pit_width_cm,
      pit_depth_cm,
      pit_area_m2,
      pit_volume_m3
    )
  )
)

with(
  subset(pit_sizes, site == "c"),
  table(species, pit_size_class)
)