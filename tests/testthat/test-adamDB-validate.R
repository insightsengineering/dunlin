# validate_tables ----

test_that("validate_tables works as expected", {
  test_list <- list(mtcars = mtcars, iris = iris)

  test_criteria <- list(
    "mtcars" = c("mpg", "hp"),
    "iris" = c("Species")
  )

  res <- expect_silent(validate_tables(test_list, test_criteria))
  expect_null(res)
})

test_that("validate_tables fails when required columns are nor present", {
  test_list <- list(mtcars = mtcars, iris = iris)

  test_criteria <- list(
    "mtcars" = c("mpg", "hp", "brand", "speed"),
    "iris" = c("Species", "Val"),
    "supplementary_table" = c("Supp")
  )

  expected_msg <- c(
    "Required tables are missing: supplementary_table \n",
    "In table mtcars required columns are missing:  brand, speed \n",
    "In table iris required columns are missing:  Val \n"
  )

  res <- expect_silent(validate_tables(test_list, test_criteria))
  expect_identical(res, expected_msg)
})

# validate_primary_key ----

test_that("validate_primary_key works as expected when key value is missing and keys are duplicated", {
  primary_tab <- iris[130:131, ]
  test_list <- list(primary_tab = primary_tab, foreign_tab = iris)

  expected_msg <- c(
    "Duplicated keys Species in primary table primary_tab : virginica \n",
    "The following key values for Species are missing in table primary_tab : setosa, versicolor \n"
  )

  res <- expect_silent(validate_primary_key(test_list, "primary_tab", "Species"))
  expect_identical(res, expected_msg)
})
