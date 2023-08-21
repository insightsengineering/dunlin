# rule ----

test_that("rule create works with arguments", {
  r <- rule(a = "1", b = "2")
  expect_s3_class(r, "rule")
  expect_identical(r, c(a = "1", b = "2"), ignore_attr = TRUE)
  r2 <- rule(.lst = list(a = "1", b = "2"))
  expect_identical(r2, r)
})

test_that("rule coerce NA to NA_character_", {
  r <- rule(a = NA)
  expect_s3_class(r, "rule")
  expect_identical(r, c(a = NA_character_), ignore_attr = TRUE)
})

test_that("rule works for multiple map", {
  r <- rule(a = "1", b = c("2", "3"))
  expect_s3_class(r, "rule")
  expect_identical(r, c(a = "1", b = "2", b = "3"), ignore_attr = TRUE)
})

test_that("rule fails when one value is mapped to multiple", {
  expect_error(
    rule(a = "1", b = c("1", "2")),
    "Assertion on 'vals' failed: Contains duplicated values, position 2"
  )
  expect_error(
    rule(a = NA, b = c(NA, "2")),
    "Assertion on 'vals' failed: Contains duplicated values, position 2"
  )
})

test_that("rule fails for values that is not character/logical/numeric", {
  expect_error(
    rule(a = list(1)),
    "Value mapping may only contain the type: \\{character\\}"
  )
})

test_that("rule printed correctly", {
  expect_snapshot(rule(a = "1", b = NA))
})

# list2rules ----

test_that("list2rules works as expected", {
  r1 <- list(
    rule_a = list(a = "1", b = "2"),
    rule_b = list(a = "3", b = "4"),
    rule_c = list()
  )

  expect_silent(res <- list2rules(r1))

  checkmate::expect_list(res, types = "rule", len = 3)
  expect_identical(names(res), c("rule_a", "rule_b", "rule_c"))
})

test_that("list2rules works as expected with additional arguments", {
  r1 <- list(
    rule_a = list(a = "1", b = "2", .string_as_fct = FALSE, .na_last = FALSE),
    rule_b = list(a = "3", b = "4", .to_NA = NULL),
    rule_c = list()
  )

  expect_silent(res <- list2rules(r1))

  checkmate::expect_list(res, types = "rule", len = 3)
  expect_identical(names(res), c("rule_a", "rule_b", "rule_c"))

  expect_identical(
    res$rule_a,
    rule(a = "1", b = "2", .string_as_fct = FALSE, .na_last = FALSE, .to_NA = "")
  )
  expect_identical(
    unlist(attr(res$rule_a, ".to_NA")),
    ""
  )
  expect_null(
    unlist(attr(res$rule_b, ".to_NA"))
  )
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
  expected <- list(a = c("a", "b"), b = c("c", "d"), .string_as_fct = TRUE, .na_last = TRUE, .drop = FALSE, .to_NA = "")
  expect_identical(as.list(test_rule), expected)
})

test_that("as.list and rule are reversible", {
  test_rule <- rule(a = c("a", "b"), b = c("c", "d"), .drop = FALSE, .na_last = TRUE)
  expect_identical(do.call(rule, as.list(test_rule)), test_rule)
})

test_that("as.list and rule are reversible when .to_NA is NULL", {
  test_rule <- rule(a = c("a", "b"), b = c("c", "d"), .drop = FALSE, .na_last = TRUE, .to_NA = NULL)
  expect_identical(do.call(rule, as.list(test_rule)), test_rule)
})
