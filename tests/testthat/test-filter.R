# log_filter ----

test_that("log_filter.data.frame works as expected", {
  df1 <- expect_silent(log_filter(iris, Sepal.Length >= 7))
  df2 <- subset(iris, Sepal.Length >= 7)
  expect_identical(df1, df2, ignore_attr = TRUE)
  df1_attr <- attr(df1, "rows")
  expect_identical(df1_attr, list("Sepal.Length >= 7" = list(init = 150L, final = 13L, suffix = NULL)))

  df3 <- expect_silent(log_filter(df1, Sepal.Width >= 3))
  df4 <- subset(df2, Sepal.Width >= 3)
  expect_identical(df3, df4, ignore_attr = TRUE)
  df3_attr <- attr(df3, "rows")
  expect_identical(
    df3_attr,
    list(
      "Sepal.Length >= 7" = list(init = 150L, final = 13L, suffix = NULL),
      "Sepal.Width >= 3" = list(init = 13L, final = 9L, suffix = NULL)
    )
  )

  df3 <- expect_silent(log_filter(df1, Sepal.Width >= 3, "Sepal filter"))
  df4 <- subset(df2, Sepal.Width >= 3)
  expect_identical(df3, df4, ignore_attr = TRUE)
  df3_attr <- attr(df3, "rows")
  expect_identical(
    df3_attr,
    list(
      "Sepal.Length >= 7" = list(init = 150L, final = 13L, suffix = NULL),
      "Sepal.Width >= 3" = list(init = 13L, final = 9L, suffix = "Sepal filter")
    )
  )
})

test_that("log_filter.data.frame fails if variable outside data/environment", {
  expect_error(log_filter(iris, a >= 7), "Variable a not found")
  x <- list(threshold = 3.9)
  expect_silent(
    withr::with_environment(
      x,
      log_filter(iris, Sepal.Width >= threshold)
    )
  )
})

test_that("log_filter.list works as expected", {
  df_raw <- list(iris = iris)
  df1 <- expect_silent(log_filter(df_raw, Sepal.Length >= 7, "iris", by = NULL))
  df2 <- subset(iris, Sepal.Length >= 7)
  expect_identical(df1$iris, df2, ignore_attr = TRUE)
  df1_attr <- attr(df1$iris, "rows")
  expect_identical(df1_attr, list("Sepal.Length >= 7" = list(init = 150L, final = 13L, suffix = NULL)))
  df3 <- expect_silent(log_filter(df1, Sepal.Width >= 3, "iris", by = NULL))
  df4 <- subset(df2, Sepal.Width >= 3)
  expect_identical(df3$iris, df4, ignore_attr = TRUE)
  df3_attr <- attr(df3$iris, "rows")
  expect_identical(
    df3_attr,
    list(
      "Sepal.Length >= 7" = list(init = 150L, final = 13L, suffix = NULL),
      "Sepal.Width >= 3" = list(init = 13L, final = 9L, suffix = NULL)
    )
  )
  df5 <- expect_silent(log_filter(df3, Sepal.Width >= 2, "iris", by = NULL, suffix = "Sepal filter"))
  df5_attr <- attr(df5$iris, "rows")
  expect_identical(
    df5_attr,
    list(
      "Sepal.Length >= 7" = list(init = 150L, final = 13L, suffix = NULL),
      "Sepal.Width >= 3" = list(init = 13L, final = 9L, suffix = NULL),
      "Sepal.Width >= 2" = list(init = 9L, final = 9L, suffix = "Sepal filter")
    )
  )

  # Verbose argument prints the filter log.
  out <- capture.output(
    res <- log_filter(df_raw, Sepal.Width >= 2, "iris", by = NULL, suffix = "Sepal filter min", verbose = TRUE)
  )
  expect_snapshot(out)

  out2 <- capture.output(
    res2 <- log_filter(res, Sepal.Width < 4, "iris", by = NULL, suffix = "Sepal filter max", verbose = TRUE)
  )
  expect_snapshot(out2)
})

test_that("log_filter.list fails as expected", {
  df_raw <- list(iris = iris)
  expect_error(
    log_filter(df_raw, Sepal.Length >= 7, "x", by = NULL),
    "Expected table names: x not in data"
  )
})

test_that("log_filter.list subsets USUBJID", {
  dfa <- data.frame(USUBJID = letters[1:10], b = 1:10)
  dfb <- data.frame(USUBJID = letters[1:10], c = 10:1)
  df_raw <- list(adsl = dfa, dfb = dfb)
  df1 <- expect_silent(log_filter(df_raw, b >= 7, "adsl", by = "USUBJID"))
  expect_identical(df1$adsl$USUBJID, c("g", "h", "i", "j"))
  expect_identical(df1$dfb$USUBJID, c("g", "h", "i", "j"))

  expect_identical(attr(df1$adsl, "rows"), list("b >= 7" = list(init = 10L, final = 4L, suffix = NULL)))
  expect_identical(
    attr(df1$dfb, "rows"),
    list("Filtered by adsl: b >= 7" = list(init = 10L, final = 4L, suffix = NULL))
  )

  # The by argument can be automatically determined.
  df2 <- expect_silent(log_filter(df_raw, b >= 7, "adsl", by = NULL))
  expect_identical(df1, df2)
})

test_that("log_filter.list preserves label attribute in all tables", {
  dfa <- data.frame(USUBJID = letters[1:10], b = 1:10)
  dfb <- data.frame(USUBJID = letters[1:10], c = 10:1)

  attr(dfa$USUBJID, "label") <- "usubjid_dfa"
  attr(dfb$USUBJID, "label") <- "usubjid_dfb"

  df_raw <- list(adsl = dfa, dfb = dfb)
  res <- expect_silent(log_filter(df_raw, b >= 7, "adsl", by = "USUBJID"))

  expected_adsl <- c("g", "h", "i", "j")
  attr(expected_adsl, "label") <- "usubjid_dfa"
  expect_identical(res$adsl$USUBJID, expected_adsl)

  expected_dfb <- c("g", "h", "i", "j")
  attr(expected_dfb, "label") <- "usubjid_dfb"
  expect_identical(res$dfb$USUBJID, expected_dfb)

  df_raw <- list(adsl = dfa, dfb = dfb)
  res <- expect_silent(log_filter(df_raw, c >= 7, "dfb", by = "USUBJID"))

  expect_identical(res$adsl$USUBJID, df_raw$adsl$USUBJID)

  expected_dfb <- c("a", "b", "c", "d")
  attr(expected_dfb, "label") <- "usubjid_dfb"
  expect_identical(res$dfb$USUBJID, expected_dfb)
})

test_that("log_filter works with long conditions", {
  df1 <- expect_silent(log_filter(iris, Sepal.Length >= 7 & Sepal.Length >= 7 & Sepal.Length >= 7 & Sepal.Length >= 7))
  df2 <- subset(iris, Sepal.Length >= 7)
  expect_identical(df1, df2, ignore_attr = TRUE)
})

test_that("log_filters works with custom `mode` argument", {
  dfa <- data.frame(USUBJID = letters[5:14], b = 1:10)
  dfb <- data.frame(USUBJID = letters[1:10], c = 1:10)

  attr(dfa$USUBJID, "label") <- "usubjid_dfa"
  attr(dfb$USUBJID, "label") <- "usubjid_dfb"

  df_raw <- list(adsl = dfa, dfb = dfb)
  res <- expect_silent(log_filter(df_raw, c >= 7, "dfb", by = "USUBJID", mode = "all"))
  expect_equal(nrow(res$dfb), 4)
  expect_equal(nrow(res$adsl), 4)

  res <- expect_silent(log_filter(df_raw, c >= 7, "dfb", by = "USUBJID", mode = "unique"))
  expect_equal(nrow(res$dfb), 4)
  expect_equal(nrow(res$adsl), 10)
})

# get_log ----

test_that("get_log works as expected", {
  dfa <- data.frame(USUBJID = letters[1:10], b = 1:10)
  dfb <- data.frame(USUBJID = letters[1:10], c = 10:1)
  df_raw <- list(adsl = dfa, dfb = dfb)
  df1 <- expect_silent(log_filter(df_raw, b >= 7, "adsl", by = "USUBJID"))
  res <- expect_silent(get_log(df1))

  expect_identical(res$adsl, "b >= 7 [10 --> 4 rows.]")
  expect_identical(res$dfb, "Filtered by adsl: b >= 7 [10 --> 4 rows.]")

  df2 <- expect_silent(log_filter(df1, c >= 3, "dfb", by = "USUBJID"))
  res2 <- expect_silent(get_log(df2))

  expect_identical(res2$adsl, "b >= 7 [10 --> 4 rows.]")
  expect_identical(res2$dfb, c("Filtered by adsl: b >= 7 [10 --> 4 rows.]", "c >= 3 [4 --> 2 rows.]"))
})

test_that("get_log works as expected when incl.adsl = FALSE", {
  dfa <- data.frame(USUBJID = letters[1:10], b = 1:10)
  dfb <- data.frame(USUBJID = letters[1:10], c = 10:1)
  df_raw <- list(adsl = dfa, dfb = dfb)
  df1 <- expect_silent(log_filter(df_raw, b >= 7, "adsl", by = "USUBJID"))
  res <- expect_silent(get_log(df1, incl.adsl = FALSE))

  expect_identical(res$adsl, "b >= 7 [10 --> 4 rows.]")
  expect_identical(res$dfb, "No filtering [4 rows.]")
})

test_that("get_log works as expected with suffix", {
  dfa <- data.frame(USUBJID = letters[1:10], b = 1:10)
  dfb <- data.frame(USUBJID = letters[1:10], c = 10:1)
  df_raw <- list(adsl = dfa, dfb = dfb)
  df1 <- expect_silent(log_filter(df_raw, b >= 7, "adsl", by = "USUBJID", "b filter"))
  res <- expect_silent(get_log(df1))

  expect_identical(res$adsl, "b filter: b >= 7 [10 --> 4 rows.]")
  expect_identical(res$dfb, "b filter: Filtered by adsl: b >= 7 [10 --> 4 rows.]")

  df2 <- expect_silent(log_filter(df1, c >= 3, "dfb", by = "USUBJID"))
  res2 <- expect_silent(get_log(df2))

  expect_identical(res2$adsl, "b filter: b >= 7 [10 --> 4 rows.]")
  expect_identical(res2$dfb, c("b filter: Filtered by adsl: b >= 7 [10 --> 4 rows.]", "c >= 3 [4 --> 2 rows.]"))
})

# print_log ----

test_that("print_log.list works as expected", {
  dfa <- data.frame(USUBJID = letters[1:10], b = 1:10)
  dfb <- data.frame(USUBJID = letters[1:10], c = 10:1)
  df_raw <- list(adsl = dfa, dfb = dfb)

  df1 <- expect_silent(log_filter(df_raw, b >= 7, "adsl", by = "USUBJID"))
  res <- capture.output(print_log(df1))

  expect_identical(
    res,
    c(
      "Filter Log:",
      "  - adsl:",
      "  b >= 7 [10 --> 4 rows.]",
      "  - dfb:",
      "  Filtered by adsl: b >= 7 [10 --> 4 rows.]"
    )
  )

  df2 <- expect_silent(log_filter(df1, c >= 3, "dfb", by = "USUBJID", "C filter"))
  res2 <- capture.output(print_log(df2))

  expect_identical(
    res2,
    c(
      "Filter Log:",
      "  - adsl:",
      "  b >= 7 [10 --> 4 rows.]",
      "  - dfb:",
      "  Filtered by adsl: b >= 7 [10 --> 4 rows.] ",
      "  C filter: c >= 3 [4 --> 2 rows.]"
    )
  )
})


test_that("print_log.list works as expect when no filtering is performed", {
  dfa <- data.frame(USUBJID = letters[1:10], b = 1:10)
  dfb <- data.frame(USUBJID = letters[1:10], c = 10:1)
  df_raw <- list(adsl = dfa, dfb = dfb)
  res_ori <- capture.output(print_log(df_raw))

  expect_identical(
    res_ori,
    c(
      "Filter Log:",
      "  - adsl:",
      "  No filtering [10 rows.]",
      "  - dfb:",
      "  No filtering [10 rows.]"
    )
  )

  res_ori <- capture.output(print_log(df_raw, incl = FALSE))

  expect_identical(
    res_ori,
    c(
      "Filter Log:",
      "  No filtering"
    )
  )

  df1 <- expect_silent(log_filter(df_raw, c >= 7, "dfb", by = "USUBJID"))
  res <- capture.output(print_log(df1))

  expect_identical(
    res,
    c(
      "Filter Log:",
      "  - adsl:",
      "  No filtering [10 rows.]",
      "  - dfb:",
      "  c >= 7 [10 --> 4 rows.]"
    )
  )

  res <- capture.output(print_log(df1, incl = FALSE))

  expect_identical(
    res,
    c(
      "Filter Log:",
      "  - dfb:",
      "  c >= 7 [10 --> 4 rows.]"
    )
  )

  df2 <- expect_silent(log_filter(df_raw, c >= 7, "dfb", by = "USUBJID", "C filter"))
  res <- capture.output(print_log(df2))

  expect_identical(
    res,
    c(
      "Filter Log:",
      "  - adsl:",
      "  No filtering [10 rows.]",
      "  - dfb:",
      "  C filter: c >= 7 [10 --> 4 rows.]"
    )
  )
})

test_that("print_log.data.frame works as expected", {
  dfa <- data.frame(USUBJID = letters[1:10], b = 1:10)
  dfb <- data.frame(USUBJID = letters[1:10], c = 10:1)
  df_raw <- list(adsl = dfa, dfb = dfb)

  df1 <- expect_silent(log_filter(df_raw, b >= 7, "adsl", by = "USUBJID"))

  res <- capture.output(print_log(df1[["dfb"]]))
  expect_identical(
    res,
    c(
      "Filter Log:",
      "  Filtered by adsl: b >= 7 [10 --> 4 rows.]"
    )
  )

  res <- capture.output(print_log(df1[["dfb"]], incl.adsl = FALSE))
  expect_identical(
    res,
    c(
      "Filter Log:",
      "  No filtering [4 rows.]"
    )
  )

  res <- capture.output(print_log(df1[["dfb"]], incl = FALSE, incl.adsl = FALSE))
  expect_identical(
    res,
    c(
      "Filter Log:",
      "  "
    )
  )
})
