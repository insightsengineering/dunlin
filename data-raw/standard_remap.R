# standard_remap ----

standard_remap <- yaml::read_yaml(
  system.file("extdata", "standard_remap.yaml", package = "dunlin")
)

usethis::use_data(standard_remap, overwrite = TRUE)
