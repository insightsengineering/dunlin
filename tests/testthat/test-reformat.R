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
  r <- rule(x = "a", z = NA)
  expect_identical(
    reformat(x, r, .string_as_fct = FALSE, .to_NA = NULL),
    c("b", "x", "b", "", "z", "x")
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

test_that(".to_NA attribute of rule is used if not specified in reformat", {
  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", z = NA, .to_NA = NULL)
  expect_identical(
    reformat(x, r, .string_as_fct = FALSE),
    c("b", "x", "b", "", "z", "x")
  )
})

test_that("setting .to_NA to NULL in reformat prevents conversion to NA specified in rule", {
  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", z = NA, .to_NA = "")
  expect_identical(
    reformat(x, r, .string_as_fct = FALSE, .to_NA = NULL),
    c("b", "x", "b", "", "z", "x")
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

test_that("reformat as character works as expected with verbose = TRUE", {
  x <- c("b", "a", "b", "", NA, "a")
  r <- rule(x = "a", y = "", z = NA, .string_as_fct = FALSE)

  out <- capture.output(
    res <- reformat(x, r, verbose = TRUE)
  )

  expected <- capture.output(print(r))
  expect_identical(out, expected)
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

test_that("reformat factor works as expected when .to_NA is NULL", {
  x <- c("a", "a", "b", "", NA)
  r <- rule(x = "a", .to_NA = NULL)
  expect_silent(res <- reformat(x, r, .string_as_fct = FALSE))
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

test_that("reformat as factor works as expected with verbose = TRUE", {
  x <- factor(c("a", "a", "b", "", NA), levels = c("a", "b", ""))
  r <- rule(x = "a", y = "", z = NA, .string_as_fct = FALSE)

  out <- capture.output(
    res <- reformat(x, r, verbose = TRUE)
  )

  expected <- capture.output(print(r))
  expect_identical(out, expected)
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

test_that("reformat for list works with all_datasets keyword", {
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

  df3 <- data.frame(
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
  )

  db <- list(df1 = df1, df2 = df2, df3 = df3)
  attr(db$df1$char, "label") <- "my label"

  test_map <- list(
    df1 = list(
      char = rule("Empty" = "", "B" = "b", "Not Available" = NA),
      fact = rule(Y = "f1")
    ),
    df2 = list(
      char = rule()
    ),
    all_datasets = list(
      fact = rule(X = "f1")
    )
  )

  expect_silent(res <- reformat(db, test_map))
  expected_char <- factor(
    c("Empty", "B", "Not Available", "a", "k", "x"),
    c("Empty", "B", "a", "k", "x", "Not Available")
  )
  attr(expected_char, "label") <- "my label"

  expected_fact <- factor(c("Y", "f2", NA, NA, "Y", "Y"), levels = c("Y", "f2"))
  expected_fact2 <- factor(c("X", "f2", NA, NA, "X", "X"), levels = c("X", "f2"))

  expect_identical(res$df1$char, expected_char) # normal reformatting keeps attribute.
  expect_identical(res$df1$fact, expected_fact) # specific reformatting has priority over all_dataset reformatting.
  expect_identical(res$df2$fact, expected_fact2) # All dataset rule applies by default.
  expect_identical(res$df2$char, as.factor(db$df2$char)) # Empty rule changes character to factor by default.
  # Datasets not explicitly mentioned in rule are also reformatted by all_datasets rules.
  expect_identical(res$df3$fact, expected_fact2)
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

test_that("reformat for list works as expected when verbose is TRUE", {
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

  expect_no_message(
    out <- capture.output(
      res <- reformat(db, test_map, verbose = TRUE)
    )
  )

  expected <- capture.output(print(test_map))[1:10]
  expected[1] <- ""
  expected[2] <- "Data frame `df1`, column `char`:"

  expect_identical(out[1:10], expected)

  expected <- capture.output(print(test_map))[14:21]
  expected[1] <- "Data frame `df2`, column `char`:"
  expect_identical(out[12:19], expected)
})

test_that("reformat for list works as expected when verbose is TRUE and tables are missing", {
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
      x = rule("Empty" = "", "B" = "b", "Not Available" = NA)
    ),
    df_absent = list(
      char = rule()
    )
  )
  expect_message(
    out <- capture.output(
      res <- reformat(db, test_map, verbose = TRUE)
    ),
    "Tables df_absent absent from data set.\nColumns x absent from table df1."
  )
  expect_identical(out, "")

  test_map2 <- list(
    df1 = list(
      char = rule("Empty" = "", "B" = "b", "Not Available" = NA)
    ),
    df_absent = list(
      char = rule()
    )
  )
  expect_message(
    out <- capture.output(
      res <- reformat(db, test_map2, verbose = TRUE)
    ),
    "Tables df_absent absent from data set."
  )
  expect_identical(length(out), 11L)
})

# h_expand_all_datasets ----

test_that("h_expand_all_datasets works as expected", {
  r <- rule(x = "a", z = NA, .to_NA = NULL)

  format_list <- list(
    adae = list(
      AEDECOD = r,
      AEBODSYS = r
    ),
    all_datasets = list(AETOX = r)
  )

  expect_silent(
    res <- h_expand_all_datasets(format_list, ls_datasets = c("adsl", "adae"))
  )

  expect_identical(
    res,
    list(
      adsl = list(
        AETOX = r
      ),
      adae = list(
        AETOX = r,
        AEDECOD = r,
        AEBODSYS = r
      )
    )
  )
})

test_that("h_expand_all_datasets works as expected when all_datasets is NULL", {
  r <- rule(x = "a", z = NA, .to_NA = NULL)

  format_list <- list(
    adae = list(
      AEDECOD = r,
      AEBODSYS = r
    )
  )

  expect_silent(
    res <- h_expand_all_datasets(format_list, ls_datasets = c("adsl", "adae"))
  )

  expect_identical(
    res,
    format_list
  )
})

test_that("h_expand_all_datasets works as expected when ls_datasets is NULL", {
  r <- rule(x = "a", z = NA, .to_NA = NULL)

  format_list <- list(
    adae = list(
      AEDECOD = r,
      AEBODSYS = r
    ),
    all_datasets = list(
      ARM = r
    )
  )

  expect_silent(
    res <- h_expand_all_datasets(format_list, ls_datasets = NULL)
  )

  expect_identical(
    res,
    format_list["adae"]
  )
})
