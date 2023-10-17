# safe_transfomer ----

test_that("safe_transformer works as expected", {
  e <- environment()
  label <- "abc"
  expect_identical(safe_transformer("label", e), "abc")
  expect_identical(safe_transformer("Label", e), "Abc")
  expect_identical(safe_transformer("LABEL", e), "ABC")
})

# add_whisker ----

test_that("add_whisker and remove_whisker works", {
  add_whisker(c(a = "1", b = "2"))
  expect_identical(get("a", envir = whisker_env), c(a = "1"))
  expect_identical(get("b", envir = whisker_env), c(b = "2"))
  remove_whisker(c("a", "b"))
  expect_false(exists("a", envir = whisker_env, inherit = FALSE))
  expect_false(exists("b", envir = whisker_env, inherit = FALSE))
})

# render_safe ----

test_that("render_safe works", {
  expect_identical(render_safe("patient label is {Patient_label}"), "patient label is Patients")
  expect_identical(render_safe("patient label is {patient_label}"), "patient label is patients")
  expect_identical(render_safe("patient label is {PATIENT_LABEL}"), "patient label is PATIENTS")
  expect_identical(render_safe("patient label is {misspell}"), "patient label is misspell")
})

test_that("render_safe works with custom whiskers", {
  add_whisker(c(Patient_label = "Perfect Match!"))

  expect_identical(render_safe("patient label is {Patient_label}"), "patient label is Perfect Match!")
  expect_identical(render_safe("patient label is {patient_label}"), "patient label is patients")
  expect_identical(render_safe("patient label is {PATIENT_LABEL}"), "patient label is PATIENTS")
  expect_identical(render_safe("patient label is {misspell}"), "patient label is misspell")

  remove_whisker(c("Patient_label"))
})

# show_whisker ---

test_that("show_whisker works", {
  res <- capture_output(show_whisker())
  expect_identical(
    res,
    "patient_label --> patients"
  )

  add_whisker(c(Patient_label = "Perfect Match!"))
  res <- capture_output(show_whisker())
  expect_identical(
    res,
    c("Patient_label --> Perfect Match!\npatient_label --> patients")
  )
  remove_whisker(c("Patient_label"))
})
