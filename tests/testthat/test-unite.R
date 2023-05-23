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

  y <- ls_unite(db, "df1", c("Month", "Day"))

  expect_identical(x$df1$FUSION, y$df1$Month.Day)
})

test_that("ls_unite works as expected with character", {
  df1 <- data.frame(
    "Day" = c("Tu", "Tu", "Th", "Sat", "Sat", "Sun"),
    "Month" = c("Feb", "Apr", "Feb", "Jan", "Jan", "Jan"),
    "Timepoint" = c("1", "2", "1", "0", "2", "1")
  )

  df2 <- data.frame()

  db <- list(
    df1 = df1,
    df2 = df2
  )

  x <- ls_unite(db, "df1", c("Month", "Day"), new = "FUSION")

  expected <- factor(
    c("Feb.Tu", "Apr.Tu", "Feb.Th", "Jan.Sat", "Jan.Sat", "Jan.Sun"),
    levels = c("Feb.Tu", "Feb.Th", "Apr.Tu", "Jan.Sat", "Jan.Sun")
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
