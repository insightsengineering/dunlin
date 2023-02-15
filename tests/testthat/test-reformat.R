# h_reformat_tab ----

test_that("h_reformat_tab works as expected", {
  df1 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
  )

  db <- dm::dm(df1, df2)
  dic_map <- rule(A = "a", B = "b", Missing = NA)
  res <- expect_silent(h_reformat_tab(db, "df1", "char", dic_map))

  expected <- c("A", "B", "Missing", "A", "k", "x")

  expect_identical(res$df1$char, expected)
})

# apply_reformat ----

test_that("apply_reformat works as expected with other All spelling", {
  df1 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
  )

  db <- dm::dm(df1, df2)

  my_map <- list(
    df1 = list(
      char = rule(
        "A" = c("a", "k"),
        "B" = "b"
      )
    ),
    All = list(
      fact = rule(
        "F1" = "f1",
        "F2" = "f2",
        "<Missing>" = NA
      ),
      other = rule(
        "x" = "X"
      )
    )
  )

  res <- expect_silent(apply_reformat(db, my_map))

  expected_char <- c("A", "B", NA, "A", "A", "x")
  expect_identical(res$df1$char, expected_char)

  expected_fact <- factor(
    c("F1", "F2", "<Missing>", "<Missing>", "F1", "F1"),
    levels = c("F1", "F2", "<Missing>")
  )
  expect_identical(res$df1$fact, expected_fact)
  expect_identical(res$df2$fact, expected_fact)
})


test_that("apply_reformat works as expected with NULL values", {
  df1 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
  )

  db <- dm::dm(df1, df2)

  my_map <- list(
    df1 = NULL,
    df2 = list(
      fact = empty_rule
    )
  )

  res <- expect_silent(apply_reformat(db, my_map))

  expect_identical(res$df1, db$df1)
  expect_identical(res$df2$fact, db$df2$fact)
})

# empty strings ----

test_that("apply_reformat works with empty strings", {
  df1 <- data.frame(
    "char" = c("", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
  )
  db <- dm::dm(df1, df2)

  test_map <- list(
    df1 = list(
      char = rule(
        "A" = c("a", "k"),
        "EMPTY STRING" = ""
      ),
      logi = empty_rule
    ),
    df2 = NULL
  )
  res <- apply_reformat(db, test_map)

  expect_identical(
    res$df1$char[1],
    "EMPTY STRING"
  )
})

# attributes ----

test_that("apply_reformat preserves labels", {
  char <- c("", "b", NA, "a", "k", "x")
  attr(char, "label") <- "my_label"

  df1 <- data.frame(
    "char" = char,
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
  )
  db <- dm::dm(df1, df2)

  test_map <- list(
    df1 = list(
      char = rule(
        "A" = c("a", "k"),
        "isNA" = NA,
        "EMPTY STRING" = ""
      ),
      logi = empty_rule
    ),
    df2 = NULL
  )

  res <- apply_reformat(db, test_map)

  expected_char <- c("EMPTY STRING", "b", "isNA", "A", "A", "x")
  attr(expected_char, "label") <- "my_label"

  expect_identical(
    res$df1$char,
    expected_char
  )

})

test_that("apply_reformat works as expected with empty list", {
  df1 <- data.frame(
    "char" = c("", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"), levels = c("f2", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
  )
  db <- dm::dm(df1, df2)

  test_map <- list(
    df1 = list(
      char = rule(),
      fact = rule(),
      logi = rule()
    ),
    df2 = empty_rule
  )

  res <- apply_reformat(db, test_map)

  # Character are converted to factors with levels in alphabetic order.
  expect_identical(
    res$df1$char,
    c("", "b", NA, "a", "k", "x")
  )

  # Logical are converted to factors with levels in alphabetic order.
  expect_identical(
    res$df1$logi,
    c(NA, FALSE, TRUE, NA, FALSE, NA)
  )

  # Factor are unaltered.
  expect_identical(
    res$df1$fact,
    db$df1$fact,
  )

  expect_identical(
    res$df2,
    db$df2
  )
})

# reformat ----

## reformat character ----

test_that("reformat for characters works as expected", {
  x <- c("a", "a", "b", "", NA)
  r <- rule(x = "a", y = "", z = NA)
  expect_identical(
    reformat(x, r),
    c("x", "x", "b", "y", "z")
  )
})


test_that("reformat for factors works as expected", {
  x <- factor(c("a", "a", "b", "", NA), levels = c("a", "b", ""))
  r <- rule(x = "a", y = "", z = NA)
  expect_identical(
    reformat(x, r),
    factor(c("x", "x", "b", "y", "z"), c("x", "b", "y", "z"))
  )
  r <- rule(x = "a", y = "")
  expect_identical(
    reformat(x, r),
    factor(c("x", "x", "b", "y", NA), c("x", "b", "y"))
  )
  r <- rule(x = "a", y = c("", NA))
  expect_identical(
    reformat(x, r),
    factor(c("x", "x", "b", "y", "y"), c("x", "b", "y"))
  )
})
