# standard_remap ----

standard_remap <- read_rules(
  system.file("extdata", "standard_remap.yaml", package = "dunlin")
)

usethis::use_data(standard_remap, overwrite = TRUE)
