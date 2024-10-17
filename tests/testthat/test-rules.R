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

test_that("list2rules can handle duplicated rules", {
  r1 <- list(
    rule_a = list(a = 1, b = 2),
    rule_b = list(a = 3, b = 4),
    rule_a_again = list(a = 1, b = 2),
    rule_b = list("X" = "x")
  )

  res <- expect_error(capture_output_lines(list2rules(r1), width = 200, print = FALSE))

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

# combine_rules ----

test_that("combine_rules works as expected", {
  r1 <- rule(a = "1", b = "2", .to_NA = "x", .drop = TRUE, .na_last = FALSE)
  r2 <- rule(a = "3", c = "4", .to_NA = "y", .drop = FALSE)

  res <- combine_rules(r1, r2)
  expect_s3_class(res, "rule")
  expect_identical(res, rule(a = "3", c = "4", b = "2", .to_NA = "y", .drop = FALSE, .na_last = TRUE))
})

test_that("combine_rules works as expected with `NULL` values", {
  r1 <- NULL
  r2 <- rule(a = "3", c = "4", .to_NA = "y")

  res <- combine_rules(r1, r2)
  expect_s3_class(res, "rule")
  expect_identical(res, r2)
})

test_that("combine_rules works as expected with `NULL` values", {
  r1 <- rule(a = "1", b = "2", .to_NA = "x", .drop = TRUE, .na_last = FALSE)
  r2 <- NULL

  res <- combine_rules(r1, r2)
  expect_s3_class(res, "rule")
  expect_identical(res, r1)
})

test_that("combine_rules works as expected when both rules are `NULL` values", {
  r1 <- NULL
  r2 <- NULL
  expect_error(combine_rules(r1, r2), "Both rules are NULL.")

  res <- combine_rules(r1, r2, safe = FALSE)
  expect_s3_class(res, "rule")
  expect_identical(res, rule())
})

# combineListRules ----

test_that("combineListRules works as expected", {
  l1 <- list(
    r1 = rule(
      "first" = c("will be overwritten", "WILL BE OVERWRITTEN"),
      "last" = c(NA, "last")
    ),
    r2 = rule(
      ANYTHING = "anything"
    )
  )

  l2 <- list(
    r1 = rule(
      "first" = c("F", "f"),
      "second" = c("S", "s"),
      "third" = c("T", "t"),
      .to_NA = "something"
    ),
    r3 = rule(
      SOMETHING = "something"
    )
  )

  res <- combineListRules(l1, l2)
  expect_list(res, types = "rule", len = 3, names = "named")
  expect_identical(names(res), c("r1", "r2", "r3"))

  expect_identical(
    res$r1,
    rule(
      "first" = c("F", "f"),
      "second" = c("S", "s"),
      "third" = c("T", "t"),
      "last" = c(NA, "last"),
      .to_NA = "something"
    )
  )

  expect_identical(
    res$r2,
    rule(
      ANYTHING = "anything"
    )
  )

  expect_identical(
    res$r3,
    rule(
      SOMETHING = "something"
    )
  )
})


test_that("combineListRules fails as expected when elements are not rules", {
  l1 <- list(
    r1 = NULL
  )

  l2 <- list(
    r1 = rule(
      "first" = c("F", "f"),
      "second" = c("S", "s"),
      "third" = c("T", "t"),
      .to_NA = "something"
    )
  )

  expect_error(res <- combineListRules(l1, l2))
})
