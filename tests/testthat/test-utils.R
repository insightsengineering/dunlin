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
