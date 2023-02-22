test_that("propagate works as expected", {
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

  res <- propagate(db, "df1", "val", c("id", "id2"))
  expect_snapshot(res)
})

test_that("propagate works as expected in safe mode", {
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

  expect_error(propagate(db, "df1", "val", c("id", "id2"), safe = TRUE), "Duplicated key")
  res <- propagate(db, "df1", "val", c("id", "id2"), safe = FALSE)
  expect_snapshot(res)
})
