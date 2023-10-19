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
  expect_error(add_whisker(c(a = "1", b = list())))
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

test_that("render_safe works as expected when matching a non-string object", {
  assign("Placeholder", list(), envir = whisker_env)
  expect_identical(render_safe("This is {Placeholder}"), "This is Placeholder")
  remove_whisker(c("Placeholder"))

  assign("placeholder", c("one", "two"), envir = whisker_env)
  expect_identical(render_safe("This is {Placeholder}"), "This is One, Two")
  remove_whisker(c("placeholder"))

  expect_silent(render_safe("This is {Placeholder}"))
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

test_that("show_whisker works when non-string value are present in the whisker environment", {
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

  assign("Placeholder", 123, envir = whisker_env)
  res <- capture_output(show_whisker())
  expect_identical(
    res,
    "patient_label --> patients"
  )
  remove_whisker(c("Placeholder"))
  
  assign("placeholder", c("a", "b"), envir = whisker_env)
  res <- capture_output(show_whisker())
  expect_identical(
    res,
    "patient_label --> patients\nplaceholder --> a, b"
  )
  remove_whisker(c("placeholder"))
})
