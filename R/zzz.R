whisker_env <- NULL

.onLoad <- function(libname, pkgname) {
  whisker_env <<- new.env(parent = globalenv())
  add_whisker(default_whiskers)
}