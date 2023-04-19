# dm_unite ----

test_that("dm_unite works as expected with characters", {
  expect_warning(x <- dm_unite(dm::dm_nycflights13(), "airlines", c("carrier", "name"), new = "FUSION"))
  checkmate::expect_factor(x$airlines$FUSION)
  expect_identical(x$airlines$FUSION, factor(paste(x$airlines$carrier, x$airlines$name, sep = ".")))
})

test_that("dm_unite works as expected with factors", {
  dat <- dm::dm_nycflights13()
  dat <- dat %>%
    dm_zoom_to("airlines") %>%
    mutate(
      carrier_fct = forcats::fct_relevel(factor(carrier), "MQ"),
      name_fct = factor(name)
    ) %>%
    dm_update_zoomed()

  expect_warning(res <- dm_unite(dat, "airlines", c("carrier_fct", "name_fct"), new = "FUSION"))
  checkmate::expect_factor(res$airlines$FUSION)
  expect_identical(as.character(res$airlines$FUSION[[1]]), "9E.Endeavor Air Inc.")
  expect_identical(levels(res$airlines$FUSION)[[1]], "MQ.Envoy Air")
})

test_that("dm_unite works as expected with more than 2 columns", {
  dat <- dm::dm_nycflights13()
  dat <- dat %>%
    dm_zoom_to("airports") %>%
    mutate(
      faa_fct = forcats::fct_relevel(factor(faa), "TYS")
    ) %>%
    dm_update_zoomed()

  expect_warning(res <- dm_unite(dat, "airports", c("faa_fct", "name", "dst"), new = "FUSION"))
  checkmate::expect_factor(res$airports$FUSION)
  expect_identical(as.character(res$airports$FUSION[[1]]), "ALB.Albany Intl.A")
  expect_identical(levels(res$airports$FUSION)[[1]], "TYS.Mc Ghee Tyson.A")
})

# ls_unite ----

test_that("ls_unite works as expected with factors", {
  df1 <- data.frame(
    "Day" = factor(
      c("Tu", "Tu", "Th", "Sat", "Sat", "Sun"),
      levels = c("Mon", "Tu", "Wed", "Th", "Fri", "Sat", "Sun")
    ),
    "Month" = factor(
      c("Feb", "Apr", "Feb", "Jan", "Jan", "Jan"),
      levels = c("Jan", "Feb", "Apr")
    ),
    "Timepoint" = factor(
      c("1", "2", "1", "0", "2", "1")
    )
  )

  df2 <- data.frame()

  db <- list(
    df1 = df1,
    df2 = df2
  )

  x <- ls_unite(db, "df1", c("Month", "Day"), new = "FUSION")

  expected <- factor(
    paste(db$df1$Month, db$df1$Day, sep = "."),
    levels = c("Jan.Sat", "Jan.Sun", "Feb.Tu", "Feb.Th", "Apr.Tu")
  )

  checkmate::expect_factor(x$df1$FUSION)
  expect_identical(
    x$df1$FUSION,
    expected
  )
})

test_that("ls_unite works as expected with more than 2 columns", {
  df1 <- data.frame(
    "Day" = factor(
      c("Tu", "Tu", "Th", "Sat", "Sat", "Sun"),
      levels = c("Mon", "Tu", "Wed", "Th", "Fri", "Sat", "Sun")
    ),
    "Month" = factor(
      c("Feb", "Apr", "Feb", "Jan", "Jan", "Jan"),
      levels = c("Jan", "Feb", "Apr")
    ),
    "Timepoint" = factor(
      c("1", "2", "1", "0", "2", "1")
    )
  )

  df2 <- data.frame()

  db <- list(
    df1 = df1,
    df2 = df2
  )

  x <- ls_unite(db, "df1", c("Month", "Day", "Timepoint"), new = "FUSION")

  expected <- factor(
    c("Feb.Tu.1", "Apr.Tu.2", "Feb.Th.1", "Jan.Sat.0", "Jan.Sat.2", "Jan.Sun.1"),
    levels = c("Jan.Sat.0", "Jan.Sat.2", "Jan.Sun.1", "Feb.Tu.1", "Feb.Th.1", "Apr.Tu.2")
  )

  checkmate::expect_factor(x$df1$FUSION)
  expect_identical(
    x$df1$FUSION,
    expected
  )
})
