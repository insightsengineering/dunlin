# rule ----

test_that("rule create works with arguments", {
  r <- rule(a = "1", b = "2")
  expect_s3_class(r, "rule")
  expect_identical(r, c(a = "1", b = "2"), ignore_attr = TRUE)
  r2 <- rule(.lst = list(a = "1", b = "2"))
  expect_identical(r2, r)
})

test_that("rule coerce logical/numeric to character", {
  r <- rule(a = 1)
  expect_s3_class(r, "rule")
  expect_identical(r, c(a = "1"), ignore_attr = TRUE)
  r2 <- rule(a = NA)
  expect_s3_class(r2, "rule")
  expect_identical(r2, c(a = NA_character_), ignore_attr = TRUE)
})

test_that("rule works for multiple map", {
  r <- rule(a = 1, b = c(2, 3))
  expect_s3_class(r, "rule")
  expect_identical(r, c(a = "1", b = "2", b = "3"), ignore_attr = TRUE)
})

test_that("rule fails when one value is mapped to multiple", {
  expect_error(
    rule(a = 1, b = c(1, 2)),
    "Assertion on 'vals' failed: Contains duplicated values, position 2"
  )
  expect_error(
    rule(a = NA, b = c(NA, 2)),
    "Assertion on 'vals' failed: Contains duplicated values, position 2"
  )
})

test_that("rule fails for values that is not character/logical/numeric", {
  expect_error(
    rule(a = list(1)),
    "May only contain the following types: \\{character,numeric,logical\\}"
  )
})

test_that("rule printed correctly", {
  expect_snapshot(rule(a = 1, b = NA))
})

# empty_rule ----

test_that("emtpy_rule is length 0 character", {
  expect_identical(empty_rule, character(0), ignore_attr = TRUE)
})

test_that("emtpy_rule printed correctly", {
  expect_snapshot(empty_rule)
})

# rules ----

test_that("rules is a named list of rule", {
  r <- rules(a = rule(a = 1))
  expect_s3_class(r, "rules")
  expect_identical(r, list(rule(a = 1)), ignore_attr = TRUE)
})

test_that("rules failes if element is not rule", {
  expect_error(
    rules(a = list(a = 1)),
    "May only contain the following types: \\{rule\\}"
  )
})

test_that("rules elements must have different names", {
  expect_error(
    rules(rule(a = 1)),
    "Must have names"
  )
  expect_error(
    rules(a = rule(a = 1), a = rule(a = 2)),
    "Must have unique names, but element 2 is duplicated"
  )
})

test_that("rules printed correctly", {
  expect_snapshot(rules(a = rule(a = 1), b = rule(a = 2)))
})

# append_rules ----

test_that("append_rules works as expected", {
  a <- rules(a = rule(a = 1))
  b <- rules(a = rule(a = 2))
  expect_identical(
    append_rules(a, b),
    rules(a = rule(a = 2))
  )
  d <- rules(a = rule(a = 2), b = rule(a = 3))
  expect_identical(
    append_rules(a, d),
    rules(a = rule(a = 2), b = rule(a = 3))
  )
})

# rule reading/writing ----

test_that("rules are read and written correctly", {
  tf <- tempfile()
  r1 <- rules(
    a = rule(a = 1, b = 2),
    b = rule(a = 3, b = 4)
  )
  write_rules(r1, file = tf, append = TRUE)
  r2 <- read_rules(tf)
  expect_identical(r1, r2)
  r3 <- rules(
    a = rule(a = 2, b = 3:4),
    d = rule(a = NA)
  )
  write_rules(r3, file = tf, append = TRUE)
  expect_identical(
    read_rules(tf),
    rules(
      a = rule(a = 2, b = 3:4),
      b = rule(a = 3, b = 4),
      d = rule(a = NA)
    )
  )
  write_rules(r3, tf, FALSE)
  expect_identical(
    read_rules(tf),
    r3
  )
  on.exit(file.remove(tf))
})