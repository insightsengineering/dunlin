test_that("propagate.list works as expected", {
  df1 <- data.frame(
    "id" = c("a", "b", NA, "a", "k", "x"),
    "id2" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "val" = letters[1:6]
  )
  df2 <- data.frame(
    "id" = c("a", "b", NA, "a", "k", "x"),
    "id2" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  db <- list(df1 = df1, df2 = df2)

  res <- propagate(db, "df1", "val", c("id", "id2"))
  expect_snapshot(res)
})

test_that("propagate.list works as expected in safe mode", {
  df1 <- data.frame(
    "id" = c("a", "a", NA, "a", "k", "x"),
    "id2" = factor(c("f1", "f1", NA, NA, "f1", "f1")),
    "val" = letters[1:6]
  )
  df2 <- data.frame(
    "id" = c("a", "b", NA, "a", "k", "x"),
    "id2" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  db <- list(df1 = df1, df2 = df2)

  expect_error(propagate(db, "df1", "val", c("id", "id2"), safe = TRUE), "Duplicated key")
  res <- propagate(db, "df1", "val", c("id", "id2"), safe = FALSE)
  expect_snapshot(res)
})


# propagate.dm ----

test_that("propagate.dm works as expected", {
  df1 <- data.frame(
    "id" = c("a", "b", NA, "a", "k", "x"),
    "id2" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "val" = letters[1:6]
  )
  df2 <- data.frame(
    "id" = c("a", "b", NA, "a", "k", "x"),
    "id2" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  db <- dm::dm(df1, df2)

  expect_warning(res <- propagate(db, "df1", "val", c("id", "id2")))
  expect_snapshot(res)
})

test_that("propagate.dm works as expected in safe mode", {
  df1 <- data.frame(
    "id" = c("a", "a", NA, "a", "k", "x"),
    "id2" = factor(c("f1", "f1", NA, NA, "f1", "f1")),
    "val" = letters[1:6]
  )
  df2 <- data.frame(
    "id" = c("a", "b", NA, "a", "k", "x"),
    "id2" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  db <- dm::dm(df1, df2)

  expect_warning(expect_error(propagate(db, "df1", "val", c("id", "id2"), safe = TRUE), "Duplicated key"))
  expect_warning(res <- propagate(db, "df1", "val", c("id", "id2"), safe = FALSE))
  expect_snapshot(res)
})
