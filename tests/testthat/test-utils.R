# h_ws_to_na ----

test_that("h_ws_to_na works as expected.", {
  char1 <- c(" ", "    ", "a", "b", "", "", NA, " b ")
  res <- expect_silent(h_ws_to_na(char1))
  expect_character(res,
                   any.missing = TRUE,
                   len = length(char1))
  fact1 <- as.factor(c(" ", "    ", "a", "b", "", "", NA, " b "))
  res <- expect_silent(h_ws_to_na(fact1))
  expect_factor(res,
                n.levels = 3,
                levels = c("a", "b", " b "),
                empty.levels.ok = FALSE,
                len = length(char1))
  expect_identical(which(is.na(res)), c(1L, 2L, 5L, 6L, 7L))
})

# h_ws_to_explicit_na ----

test_that("h_ws_to_explicit_na works as expected.", {
  char1 <- c(" ", "    ", "a", "b", "", "", NA, " b ")
  res <- expect_silent(h_ws_to_explicit_na(char1))
  expect_factor(res,
                n.levels = 4,
                levels = c("a", "b", " b ", "<Missing>"),
                empty.levels.ok = FALSE,
                len = length(char1))
  res <- expect_silent(h_ws_to_explicit_na(char1, "Not here"))
  expect_factor(res,
                n.levels = 4,
                levels = c("a", "b", " b ", "Not here"),
                empty.levels.ok = FALSE,
                len = length(char1))
})

# h_as_factor ----

test_that("h_as_factor works as expected", {

    char1 <- c(" ", "    ", "a", "b", "", "", NA, "<Missing>", " b ")
    res <- expect_silent(h_as_factor(char1))
    expect_factor(res,
                  n.levels = 4,
                  levels = c("a", "b", " b ", "<Missing>"),
                  empty.levels.ok = FALSE,
                  len = length(char1))
    expect_identical(NULL, attr(res, "label"))
    expect_identical(levels(res)[4], "<Missing>")

    char2 <- c(" ", "    ", "a", "b", "", "", NA, " b ")
    attr(char2, "label") <- "my_label"
    res <- expect_silent(h_as_factor(char2))
    expect_factor(res,
                  n.levels = 4,
                  levels = c("a", "b", " b ", "<Missing>"),
                  empty.levels.ok = FALSE,
                  len = length(char2))
    expect_identical("my_label", attr(res, "label"))
    expect_identical(levels(res)[4], "<Missing>")

    fact1 <- as.factor(char1)
    attr(fact1, "label") <- "my_label"
    res <- expect_silent(h_as_factor(fact1))
    expect_factor(res,
                  n.levels = 4,
                  levels = c("a", "b", " b ", "<Missing>"),
                  empty.levels.ok = FALSE,
                  len = length(char1))
    expect_identical("my_label", attr(res, "label"))
    expect_identical(levels(res)[4], "<Missing>")

    int1 <- 1:10
    attr(int1, "label") <- "my_label"
    res <- expect_silent(h_as_factor(int1))
    expect_factor(res,
                  n.levels = 10,
                  levels = as.character(1:10),
                  len = length(int1))
})

# attr_label ----

test_that("attr_label works as expected", {

  x <- c(1:10)
  res <- attr_label(x, "my_label")
  expect_numeric(res, len = length(x))
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
