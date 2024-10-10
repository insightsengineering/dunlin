# attr_label ----

test_that("attr_label works as expected", {
  x <- c(1:10)
  res <- attr_label(x, "my_label")
  checkmate::expect_numeric(res, len = length(x))
  expect_identical(attr(res, "label"), "my_label")
  expect_error(attr_label(x, NULL), "Assertion on 'label' failed: Must be of type 'character', not 'NULL'.")
})

# attr_label_df ----

test_that("attr_label_df works as expected", {
  x <- mtcars
  res <- attr_label_df(x, letters[1:11])
  expect_identical(unlist(lapply(res, attr, "label"), use.names = FALSE), letters[1:11])
  expect_error(attr_label_df(x, letters[1:3]), "Assertion on 'label' failed: Must have length 11, but has length 3.")
})

# get_args ----

test_that("get_args works as expected", {
  expect_null(get_arg())

  expect_silent(res <- get_arg("my.option", "MY_VAR"))
  expect_null(res)

  expect_silent(res <- get_arg("my.option", "MY_VAR", "my_default"))
  expect_identical(res, "my_default")

  withr::with_options(
    c("my.option" = 123),
    {
      expect_silent(res <- get_arg("my.option", "MY_VAR", "my_default"))
      expect_identical(res, 123)
    }
  )

  # Check priority
  withr::with_envvar(
    c("MY_VAR" = "abc"),
    withr::with_options(
      c("my.option" = 123),
      {
        expect_silent(res <- get_arg("my.option", "MY_VAR", "my_default"))
        expect_identical(res, 123)
      }
    )
  )
})
