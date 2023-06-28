# cut_by_group ----

test_that("cut_by_group works as expected.", {
  group <- list(
    list(
      "Height",
      c(-Inf, 150, 170, Inf),
      c("=<150", "150-170", ">170")
    ),
    list(
      "Weight",
      c(-Inf, 65, Inf),
      c("=<65", ">65")
    ),
    list(
      "Age",
      c(-Inf, 31, Inf),
      c("=<31", ">31")
    )
  )

  data <- data.frame(
    SUBJECT = rep(letters[1:10], 3),
    PARAM = rep(c("Height", "Weight", "Age"), each = 10),
    AVAL = c(seq(140, 180, length.out = 10), seq(50, 100, length.out = 10), seq(18, 100, length.out = 10)),
    index = 1:30
  )

  res <- expect_silent(cut_by_group(data, "AVAL", "PARAM", group, "my_new_categories"))

  checkmate::expect_character(res[, "my_new_categories"], len = 30)
  expect_equal(unique(res[, "my_new_categories"]), c("=<150", "150-170", ">170", "=<65", ">65", "=<31", ">31"))
  expect_equal(res[3:4, "my_new_categories"], c("=<150", "150-170"))
})


test_that("cut_by_group works as expected when supplementary group information are provided.", {
  group <- list(
    list(
      "Height",
      c(-Inf, 150, 170, Inf),
      c("=<150", "150-170", ">170")
    ),
    list(
      "Weight",
      c(-Inf, 65, Inf),
      c("=<65", ">65")
    ),
    list(
      "Age",
      c(-Inf, 31, Inf),
      c("=<31", ">31")
    ),
    list(
      "Precondition",
      c(0, 2, Inf),
      c("less than 2", "more that two")
    )
  )

  data <- data.frame(
    SUBJECT = rep(letters[1:10], 3),
    PARAM = rep(c("Height", "Weight", "Age"), each = 10),
    AVAL = c(seq(140, 180, length.out = 10), seq(50, 100, length.out = 10), seq(18, 100, length.out = 10)),
    index = 1:30
  )

  res <- expect_silent(cut_by_group(data, "AVAL", "PARAM", group, "my_new_categories"))

  checkmate::expect_character(res[, "my_new_categories"], len = 30)
  expect_equal(unique(res[, "my_new_categories"]), c("=<150", "150-170", ">170", "=<65", ">65", "=<31", ">31"))
  expect_equal(res[3:4, "my_new_categories"], c("=<150", "150-170"))
})

test_that("cut_by_group returns NA for parameters values that are covered.", {
  group <- list(
    list(
      "Height",
      c(-Inf, 150, 170, Inf),
      c("=<150", "150-170", ">170")
    ),
    list(
      "Weight",
      c(-Inf, 65, Inf),
      c("=<65", ">65")
    )
  )

  data <- data.frame(
    SUBJECT = rep(letters[1:10], 3),
    PARAM = rep(c("Height", "Weight", "Age"), each = 10),
    AVAL = c(seq(140, 180, length.out = 10), seq(50, 100, length.out = 10), seq(18, 100, length.out = 10)),
    index = 1:30
  )

  res <- expect_silent(cut_by_group(data, "AVAL", "PARAM", group, "my_new_categories"))

  checkmate::expect_character(res[, "my_new_categories"], len = 30)
  expect_equal(unique(res[, "my_new_categories"]), c("=<150", "150-170", ">170", "=<65", ">65", NA))
  expect_equal(res[3:4, "my_new_categories"], c("=<150", "150-170"))
  expect_equal(res[21:30, "my_new_categories"], as.character(rep(NA, 10)))
})

test_that("cut_by_group fails when the number of labels doesn't fit the number of breaks.", {
  group <- list(
    list(
      "Height",
      c(-Inf, 150, 170, Inf),
      c("=<150", "150-170")
    ),
    list(
      "Weight",
      c(-Inf, 65, Inf),
      c("=<65", ">65")
    )
  )

  data <- data.frame(
    SUBJECT = rep(letters[1:10], 3),
    PARAM = rep(c("Height", "Weight", "Age"), each = 10),
    AVAL = c(seq(140, 180, length.out = 10), seq(50, 100, length.out = 10), seq(18, 100, length.out = 10)),
    index = 1:30
  )

  expect_error(
    cut_by_group(data, "AVAL", "PARAM", group, "my_new_categories"),
    "number of intervals and length of 'labels' differ"
  )
})
