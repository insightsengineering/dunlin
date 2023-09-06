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
