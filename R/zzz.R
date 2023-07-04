whisker_env <- NULL

.onLoad <- function(libname, pkgname) {
  default_whiskers <- c(patient_label = "patients")
  whisker_env <<- new.env(parent = globalenv())
  add_whisker(default_whiskers)
}
