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

# rule reading/writing ----

test_that("nested list of rules are read and written correctly", {
  tf <- tempfile()
  r1 <- list(
    df1 = list(
      a = rule(a = 1, b = 2),
      b = rule(a = 3, b = 4)
    )
  )
  write_format(r1, file = tf)

  r2 <- read_format(tf)
  expect_identical(r1, r2)

  on.exit(file.remove(tf))
})

test_that("nested list of rules are read and written correctly in the case of empty rule", {
  tf <- tempfile()
  r1 <- list(
    df1 = list(
      a = rule(a = 1, b = 2),
      b = empty_rule
    )
  )
  write_format(r1, file = tf)

  r2 <- read_format(tf)
  expect_identical(r1, r2)

  on.exit(file.remove(tf))
})
