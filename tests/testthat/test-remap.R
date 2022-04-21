# assert_remap ----

test_that("assert_remap works as expected", {
  test_map <- list(
    table1 = list(
      var1 = list(
        "A" = c("a", "k"),
        "B" = "b"
      ),
      var2 = list(
        "A" = c("a", "k"),
        "B" = "b"
      )
    ),
    table2 = list(
      var1 = list(
        "11" = "1",
        "22" = NA
      )
    )
  )

  expect_silent(assert_remap(test_map))
})

test_that("assert_remap does not tolerate duplicated table names, variables or mapping", {
  test_map <- list(
    table1 = list(
      var1 = list(
        "A" = c("a", "k"),
        "B" = "a"
      ),
      var2 = list(
        "A" = c("a", "k"),
        "B" = "b"
      ),
      var2 = list(
        "A" = c("a", "k"),
        "B" = "b"
      )
    ),
    table1 = list(
      var1 = list(
        "11" = "1",
        "22" = NA
      )
    ),
    table2 = list(
      var1 = list(
        "11" = "1",
        "22" = NA
      ),
      var1 = list(
        "11" = "1",
        "22" = NA
      )
    )
  )

  expect_error(
    assert_remap(test_map),
    "Duplicated table names: table1
Duplicated Variable name inside table: table1, table2
Duplicated mapping inside: table1.var1"
  )
})

# h_remap_tab ----

test_that("h_remap_tab works as expected", {
  df1 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  db <- dm(df1, df2)
  dic_map <- setNames(c("A", "B", "Missing"), c("a", "b", NA))

  res <- expect_silent(h_remap_tab(db, "df1", "char", dic_map))

  expected <- factor(c("A", "B", "Missing", "A", "k", "x"), levels = c("A", "B", "k", "x", "Missing"))

  expect_identical(res$df1$char, expected)
})

# remap ----

test_that("remap works as expected", {
  df1 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  db <- dm(df1, df2)

  my_map <- list(
    df1 = list(
      char = list(
        "A" = c("a", "k"),
        "B" = "b"
      )
    ),
    df2 = list(
      num = list(
        "11" = "1",
        "22" = "2"
      )
    ),
    All = list(
      fact = list(
        "F1" = "f1",
        "F2" = "f2",
        "FX" = "fx",
        "<Missing>" = NA
      ),
      other = list(
        "x" = "X"
      )
    )
  )

  res <- expect_silent(remap(db, my_map))

  expected_char <- factor(c("A", "B", NA, "A", "A", "x"), levels = c("A", "B", "x"))
  expect_identical(res$df1$char, expected_char)

  expected_num <- factor(c(11, 22, 3, 4, 5, 6), levels = c(11, 22, 3, 4, 5, 6))
  expect_identical(res$df2$num, expected_num)

  expected_fact <- factor(
    c("F1", "F2", "<Missing>", "<Missing>", "F1", "F1"),
    levels = c("F1", "F2", "FX", "<Missing>")
  )
  expect_identical(res$df1$fact, expected_fact)
  expect_identical(res$df2$fact, expected_fact)
})
