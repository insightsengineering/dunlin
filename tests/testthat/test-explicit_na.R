# ls _explicit_na ----

test_that("ls_explicit_na works as expected with default options.", {
  df1 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA),
    "no_missing" = letters[1:6]
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  db <- list(df1 = df1, df2 = df2)

  res <- expect_silent(ls_explicit_na(db))

  checkmate::expect_data_frame(res$df1, types = c("factor", "logical"), nrows = nrow(df1), ncols = ncol(df1))
  checkmate::expect_factor(res$df1$char,
    levels = c("a", "b", "k", "x", "<Missing>"),
    empty.levels.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::expect_factor(
    res$df1$fact,
    levels = c("f1", "f2", "<Missing>"),
    empty.levels.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::expect_logical(res$df1$logi, any.missing = TRUE)
  checkmate::expect_factor(res$df1$no_missing,
    levels = c(letters[1:6], "<Missing>"), # To discuss
    empty.levels.ok = TRUE,
    any.missing = FALSE
  )

  checkmate::expect_data_frame(res$df2, types = c("factor", "integer"), nrows = nrow(df2), ncols = ncol(df2))
  checkmate::expect_factor(res$df2$char,
    levels = c("a", "b", "k", "x", "<Missing>"),
    empty.levels.ok = FALSE, any.missing = FALSE
  )
  checkmate::expect_factor(
    res$df2$fact,
    levels = c("f1", "f2", "<Missing>"),
    empty.levels.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::expect_integer(res$df2$num, any.missing = FALSE)
})

test_that("ls_explicit_na works as expected with optional arguments.", {
  df1 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA),
    "num" = 1:6,
    "char2" = c("a", "b", NA, "a", "k", "x")
  )

  df1 <- attr_label_df(df1, letters[1:5])

  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  df2 <- attr_label_df(df2, LETTERS[1:3])

  db <- list(df1 = df1, df2 = df2)

  res <- ls_explicit_na(db,
    char_as_factor = FALSE,
    omit_tables = c("df2", "non existing table"),
    omit_columns = c("char2", "non existing column"),
    na_level = "Not Present"
  )

  checkmate::expect_data_frame(res$df1,
    types = c("character", "factor", "logical", "numeric", "logical"),
    nrows = nrow(df1),
    ncols = ncol(df1)
  )
  expect_equal(unlist(lapply(res$df1, attr, "label"), use.names = FALSE), letters[1:5])
  checkmate::expect_character(res$df1$char, any.missing = TRUE)
  checkmate::expect_factor(
    res$df1$fact,
    levels = c("f1", "f2", "Not Present"),
    empty.levels.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::expect_logical(
    res$df1$logi,
    any.missing = TRUE
  )
  checkmate::expect_integer(res$df1$num, any.missing = FALSE)
  expect_identical(res$df1$char2, db$df1$char2)

  checkmate::expect_data_frame(
    res$df2,
    types = c("character", "factor", "integer"),
    nrows = nrow(df2),
    ncols = ncol(df2)
  )
  expect_equal(unlist(lapply(res$df2, attr, "label"), use.names = FALSE), LETTERS[1:3])
  checkmate::expect_character(res$df2$char, any.missing = TRUE)
  checkmate::expect_factor(res$df2$fact, levels = c("f1", "f2"), empty.levels.ok = TRUE, any.missing = TRUE)
  checkmate::expect_integer(res$df2$num, any.missing = FALSE)
})

test_that("ls_explicit_na skip if all datasets excluded", {
  df1 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA),
    "num" = 1:6,
    "char2" = c("a", "b", NA, "a", "k", "x")
  )

  df1 <- attr_label_df(df1, letters[1:5])

  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  df2 <- attr_label_df(df2, LETTERS[1:3])

  db <- list(df1 = df1, df2 = df2)
  res <- expect_silent(ls_explicit_na(db,
    char_as_factor = FALSE,
    omit_tables = c("df1", "df2"),
    omit_columns = "char2",
    na_level = "Not Present"
  ))
  expect_identical(res, db)
})
