# log_filter ----

test_that("log_filter works for single data.frame", {
  df1 <- expect_silent(log_filter(iris, Sepal.Length >= 7))
  df2 <- subset(iris, Sepal.Length >= 7)
  expect_identical(df1, df2, ignore_attr = TRUE)
  df1_attr <- attr(df1, "rows")
  expect_identical(df1_attr, list("Sepal.Length >= 7" = c(150L, 13L)))
  df3 <- expect_silent(log_filter(df1, Sepal.Width >= 3))
  df4 <- subset(df2, Sepal.Width >= 3)
  expect_identical(df3, df4, ignore_attr = TRUE)
  df3_attr <- attr(df3, "rows")
  expect_identical(
    df3_attr,
    list(
      "Sepal.Length >= 7" = c(150L, 13L),
      "Sepal.Width >= 3" = c(13L, 9L)
    )
  )
})

test_that("log_filter fails if variable outside data/environment", {
  expect_error(log_filter(iris, a >= 7), "Variable a not found")
  threshold <- 7
  expect_silent(log_filter(iris, Sepal.Width >= threshold))
})

test_that("log_filter works for list of data.frame", {
  df_raw <- list(iris = iris)
  df1 <- expect_silent(log_filter(df_raw, "iris", Sepal.Length >= 7, by = NULL))
  df2 <- subset(iris, Sepal.Length >= 7)
  expect_identical(df1$iris, df2, ignore_attr = TRUE)
  df1_attr <- attr(df1$iris, "rows")
  expect_identical(df1_attr, list("Sepal.Length >= 7" = c(150L, 13L)))
  df3 <- expect_silent(log_filter(df1, "iris", Sepal.Width >= 3, by = NULL))
  df4 <- subset(df2, Sepal.Width >= 3)
  expect_identical(df3$iris, df4, ignore_attr = TRUE)
  df3_attr <- attr(df3$iris, "rows")
  expect_identical(df3_attr, list("Sepal.Length >= 7" = c(150L, 13L), "Sepal.Width >= 3" = c(13L, 9L)))
})

test_that("log_filter subset USUBJID for list of data.frame", {
  dfa <- data.frame(USUBJID = letters[1:10], b = 1:10)
  dfb <- data.frame(USUBJID = letters[1:10], c = 10:1)
  df_raw <- list(adsl = dfa, dfb = dfb)
  df1 <- expect_silent(log_filter(df_raw, "adsl", b >= 7, by = "USUBJID"))
  expect_identical(df1$adsl$USUBJID, c("g", "h", "i", "j"))
  expect_identical(df1$dfb$USUBJID, c("g", "h", "i", "j"))
})
