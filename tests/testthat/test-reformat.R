# reformat ----

## reformat non supported type ----

test_that("reformat fails for numeric or logical", {
  x <- c(0, 1, 2)
  r <- rule(a = 1, b = 2)
  expect_warning(res <- reformat(x, r), "Not implemented! Only empty rule allowed.")
})

## reformat character ----

test_that("reformat for characters works as expected", {
  x <- c("a", "a", "b", "", NA)
  r <- rule(x = "a", y = "", z = NA)
  expect_identical(
    reformat(x, r),
    c("x", "x", "b", "y", "z")
  )
})

## reformat factor ----

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

# reformat list ----

test_that("reformat for list works as expected", {
  df1 <- data.frame(
    "char" = c("", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"), levels = c("f2", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "another_char" = c("a", "b", NA, "a", "k", "x"),
    "another_fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
  )

  db <- list(df1 = df1, df2 = df2)
  attr(db$df1$char, "label") <- "my label"

  test_map <- list(
    df1 = list(
      char = rule("X" = "", "B" = "b", "Not Available" = NA),
      logi = rule()
    )
  )

  expect_silent(res <- reformat(db, test_map))

  expected <- c("X", "B", "Not Available", "a", "k", "x")
  attr(expected, "label") <- "my label"

  expect_identical(res$df1$char, expected) # normal reformatting keeps attribute.
  expect_identical(res$df1$fact, db$df1$fact) # No rules to apply.
  expect_identical(res$df1$fact, db$df1$fact) # Empty rule changes nothing.
  expect_identical(res$df2, db$df2) # No rules associated with the table, hence no change.
})

# reformat using empty_rule ----

test_that("empty_rule do nothing to input", {
  a <- c("1", "2")
  expect_identical(a, reformat(a, empty_rule))
  b <- factor(c("1", "2"))
  expect_identical(b, reformat(b, empty_rule))
})
