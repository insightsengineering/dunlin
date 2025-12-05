whisker_env <- NULL

.onLoad <- function(libname, pkgname) {
  default_whiskers <- c(
    patients_label = "patients",
    patient_label = "patient"
  )
  whisker_env <<- new.env(parent = globalenv())
  add_whisker(default_whiskers)
}
