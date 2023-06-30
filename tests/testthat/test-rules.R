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
    "May only contain the following types: \\{character,numeric,logical,NULL\\}"
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

# list2rules ----

test_that("list2rules works as expected", {
  r1 <- list(
    rule_a = list(a = 1, b = 2),
    rule_b = list(a = 3, b = 4),
    rule_c = list()
  )

  expect_silent(res <- list2rules(r1))

  checkmate::expect_list(res, type = "rule", len = 3)
  expect_identical(names(res), c("rule_a", "rule_b", "rule_c"))
})

test_that("list2rules fails as expected", {
  r1 <- list(
    rule_a = list(a = 1, b = 2),
    rule_b = list(a = 3, b = 4),
    rule_a_again = list(a = 1, b = 2),
    rule_b = list("X" = "x")
  )

  res <- expect_error(capture_output_lines(list2rules(r1), width = 200, print = FALSE))

  expect_match(
    res$message,
    "* Variable 'obj': Contains duplicated values, position 3.",
    fixed = TRUE
  )
  expect_match(
    res$message,
    "* Variable 'names(obj)': Must have unique names, but element 4 is duplicated.",
    fixed = TRUE
  )
})

# as.list ----

test_that("as.list convert rules into list correctly", {
  test_rule <- rule(a = c("a", "b"), b = c("c", "d"))
  expected <- list(a = c("a", "b"), b = c("c", "d"), .drop = FALSE, .to_NA = NULL)
  expect_identical(as.list(test_rule), expected)
})

test_that("as.list and rule are reversible", {
  test_rule <- rule(a = c("a", "b"), b = c("c", "d"), .drop = FALSE, .to_NA = NULL)
  expect_identical(rule(.lst = as.list(test_rule)), test_rule)
})

# rule reading ----

test_that("list of rules are read correctly", {
  tf <- tempfile()
  r1 <- list(
    rule_a = list(a = 1, b = 2),
    rule_b = list(a = 3, b = 4)
  )
  yaml::write_yaml(r1, file = tf)

  r2 <- read_rules(tf)

  checkmate::expect_list(r2, type = "rule", len = 2)
  expect_identical(names(r2), c("rule_a", "rule_b"))
})
