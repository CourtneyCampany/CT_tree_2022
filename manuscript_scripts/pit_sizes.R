pit_sizes <- read.csv("raw_data/pit_sizes.csv")

pit_sizes$site <- factor(pit_sizes$site)
pit_sizes$species <- factor(pit_sizes$species)
pit_sizes$replicate <- factor(pit_sizes$replicate)
pit_sizes$pit_size_class <- factor(pit_sizes$pit_size_class)

pit_sizes$tree_id <- with(
  pit_sizes,
  paste(site, species, replicate, sep = "-")
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


#create a derived dataset for further use (w/area and volume)

tree_metadata <- pit_sizes

tree_metadata$pit_area_m2 <- with(
  tree_metadata,
  (pit_length_cm / 100) * (pit_width_cm / 100)
)

tree_metadata$pit_volume_m3 <- with(
  tree_metadata,
  (pit_length_cm / 100) *
    (pit_width_cm / 100) *
    (pit_depth_cm / 100)
)


# Reorder columns for readability
tree_metadata <- tree_metadata[, c(
  "tree_id",
  "site",
  "species",
  "replicate",
  "pit_size_class",
  "pit_length_cm",
  "pit_width_cm",
  "pit_depth_cm",
  "pit_area_m2",
  "pit_volume_m3"
)]

# Save the reusable derived metadata file
write.csv(
  tree_metadata,
  "calculated_data/tree_metadata_derived.csv",
  row.names = FALSE,
  na = "NA"
)