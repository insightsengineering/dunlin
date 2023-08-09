# reformat ----

## reformat non supported type ----

test_that("reformat fails for numeric or logical", {
  x <- c(0, 1, 2)
  r <- rule(a = "x", b = "y")
  expect_warning(res <- reformat(x, r), "Not implemented for class: numeric!")
})

## reformat character ----

test_that("reformat for characters works as expected when .string_as_fct is FALSE", {
  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", y = "", z = NA)
  expect_identical(
    reformat(x, r, .string_as_fct = FALSE),
    c("b", "x", "b", "y", "z", "x")
  )
})

test_that("reformat for characters works as expected when .to_NA is NULL", {
  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", y = "", z = NA)
  expect_identical(
    reformat(x, r, .string_as_fct = FALSE),
    c("b", "x", "b", "y", "z", "x")
  )
})

test_that("reformat for characters works as expected when .to_NA is not NULL", {
  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", y = "", z = NA)
  expect_identical(
    reformat(x, r, .string_as_fct = FALSE, .to_NA = "b"),
    c(NA, "x", NA, "y", "z", "x")
  )
})

test_that("reformat for characters works as expected when the .to_NA rule attribute is not NULL ", {
  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", y = "", z = NA, .to_NA = "b")
  expect_identical(
    reformat(x, r, .string_as_fct = FALSE),
    c(NA, "x", NA, "y", "z", "x")
  )
})

test_that("reformat arguments have priorities over the rule attributes", {
  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", y = "", z = NA, .to_NA = "xxx")
  expect_identical(
    reformat(x, r, .string_as_fct = FALSE, .to_NA = "b"),
    c(NA, "x", NA, "y", "z", "x")
  )
})

test_that("reformat for characters works as expected when .string_as_fct is TRUE", {
  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", y = "", z = NA)
  expect_identical(
    reformat(x, r, .string_as_fct = TRUE),
    factor(c("b", "x", "b", "y", "z", "x"), levels = c("x", "y", "b", "z"))
  )
})

test_that("reformat for characters works as expected when .string_as_fct is TRUE and .na_last is false", {
  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", y = "", z = NA)
  expect_identical(
    reformat(x, r, .string_as_fct = TRUE, .na_last = FALSE),
    factor(c("b", "x", "b", "y", "z", "x"), levels = c("x", "y", "z", "b"))
  )

  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", y = c("", NA))
  expect_identical(
    reformat(x, r, .string_as_fct = TRUE, .na_last = FALSE),
    factor(c("b", "x", "b", "y", "y", "x"), levels = c("x", "y", "b"))
  )
})

## reformat factor ----

test_that("reformat for factors works as expected", {
  x <- factor(c("a", "", "b", "a", NA), levels = c("a", "", "b"))
  r <- rule(x = "a", y = "", z = NA)
  expect_identical(
    reformat(x, r),
    factor(c("x", "y", "b", "x", "z"), c("x", "y", "b", "z"))
  )
  r <- rule(x = "a", y = "")
  expect_identical(
    reformat(x, r),
    factor(c("x", "y", "b", "x", NA), c("x", "y", "b"))
  )
  r <- rule(x = "a", y = c(NA, ""))
  expect_identical(
    reformat(x, r),
    factor(c("x", "y", "b", "x", "y"), c("x", "b", "y"))
  )
  
  r <- rule(x = "a")
  expect_identical(
    reformat(x, r),
    factor(c("x", NA, "b", "x", NA), c("x", "b"))
  )
})

test_that("reformat factor works as expected when the level doesn't exist", {
  x <- factor(c("a", "a", "b", "", NA), levels = c("a", "b", ""))
  r <- rule(x = "a", y = "", z = NA, "Not a level" = "Not here")
  expect_silent(res <- reformat(x, r))
  expect_identical(
    res,
    factor(c("x", "x", "b", "y", "z"), levels = c("x", "y", "Not a level", "b", "z"))
  )
})

test_that("reformat factor works as expected when the level doesn't exist and .drop = TRUE", {
  x <- factor(c("a", "a", "b", "", NA), levels = c("a", "b", ""))
  r <- rule(x = "a", y = "", z = NA, "Not a level" = "Not here")
  expect_silent(res <- reformat(x, r, .drop = TRUE))
  expect_identical(
    res,
    factor(c("x", "x", "b", "y", "z"), levels = c("x", "y", "b", "z"))
  )

  r <- rule(x = "a", y = "", z = NA, "Not a level" = "Not here", .drop = TRUE)
  expect_silent(res <- reformat(x, r, .drop = TRUE))
  expect_identical(
    res,
    factor(c("x", "x", "b", "y", "z"), levels = c("x", "y", "b", "z"))
  )
})

test_that("reformat factor works as expected when .na_last = FALSE", {
  x <- factor(c("a", "a", "b", "", NA), levels = c("a", "", "b"))
  r <- rule(x = "a", y = c("", NA))
  expect_silent(res <- reformat(x, r, .na_last = FALSE))
  expect_identical(
    res,
    factor(c("x", "x", "b", "y", "y"), levels = c("x", "y", "b"))
  )
})

test_that("reformat factor works as expected when .to_NA is not NULL", {
  x <- factor(c("a", "a", "b", "", NA), levels = c("a", "", "b"))
  r <- rule(x = "a", z = NA)
  expect_silent(res <- reformat(x, r, .na_last = FALSE, .to_NA = ""))
  expect_identical(
    res,
    factor(c("x", "x", "b", NA, "z"), levels = c("x", "z", "b"))
  )
})

test_that("reformat factor works as expected when .to_NA is passed via a rule", {
  x <- factor(c("a", "a", "b", "", NA), levels = c("a", "", "b"))
  r <- rule(x = "a", z = NA, .to_NA = "")
  expect_silent(res <- reformat(x, r, .na_last = FALSE))
  expect_identical(
    res,
    factor(c("x", "x", "b", NA, "z"), levels = c("x", "z", "b"))
  )
})


test_that("reformat factor works as expected when the level doesn't exist and .na_last is false.", {
  x <- factor(c("a", "a", "b", "", NA), levels = c("a", "b", ""))
  r <- rule(x = "a", y = "", z = NA, "Not a level" = "Not here")
  expect_silent(res <- reformat(x, r, .na_last = FALSE))
  expect_identical(
    res,
    factor(c("x", "x", "b", "y", "z"), levels = c("x", "y", "z", "Not a level", "b"))
  )
})

# reformat list ----

test_that("reformat for list works as expected", {
  df1 <- data.frame(
    "char" = c("", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"), levels = c("f2", "f1"))
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
      char = rule("Empty" = "", "B" = "b", "Not Available" = NA)
    ),
    df2 = list(
      char = rule()
    )
  )

  expect_silent(res <- reformat(db, test_map))
  expected <- factor(c("Empty", "B", "Not Available", "a", "k", "x"), c("Empty", "B", "a", "k", "x", "Not Available"))
  attr(expected, "label") <- "my label"

  expect_identical(res$df1$char, expected) # normal reformatting keeps attribute.
  expect_identical(res$df1$fact, db$df1$fact) # No rules to apply.
  expect_identical(res$df1$fact, db$df1$fact) # Empty rule changes nothing.
  expect_identical(res$df2$char, as.factor(db$df2$char)) # Empty rule changes character to factor by default.
})

test_that("reformat for list works as does not change the data for no rules", {
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

  test_map <- list()

  expect_silent(res <- reformat(db, test_map))
  expect_identical(res, db)
})
