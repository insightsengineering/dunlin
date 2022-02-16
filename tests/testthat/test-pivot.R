# mini_pivot_wider ----

test_that("mini_pivot_wider works as expected.", {
  
  test_data <- data.frame(
    the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
    the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
    the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "precondition"),
    the_val = c(65, 165, "M", 66, "F", 166, 155, TRUE)
  )
  
  expected_data <- data.frame(
    id = c("A", "B", "C", "D"),
    gender = c("M", "F", NA, NA),
    height = c("165", "166", "155", NA),
    precondition = c(NA, NA, NA, "TRUE"),
    weight = c("65", "66", NA, NA)
  )
  
  res <- expect_silent(mini_pivot_wider(test_data, "the_obs", "the_param", "the_val"))
  expect_identical(expected_data, res)
})

test_that("mini_pivot_wider returns an error when required column are not present.", {
  
  test_data <- data.frame(
    the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
    the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
    the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "precondition"),
    the_val = c(65, 165, "M", 66, "F", 166, 155, TRUE)
  )
  
  expect_error(mini_pivot_wider(
    test_data, 
    "Must be a subset of {'the_obs','the_obs2','the_param','the_val'}, but is {'the_obs2','the_param2','the_val2'}.")
  )
})

test_that("mini_pivot_wider returns an error when data are duplicated.", {
  
  test_data <- data.frame(
    the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
    the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
    the_param = c("weight", "weight", "gender", "weight", "gender", "height", "height", "precondition"),
    the_val = c(65, 65, "M", 66, "F", 166, 155, TRUE)
  )
  
  expect_error(mini_pivot_wider(
    test_data, 
    "Assertion on 'any(duplicated(data[, c(obs_id, param_from)]))' failed: Must be FALSE.")
  )
})

# multi_pivot_wider ----

test_that("multi_pivot_wider works as expected.", {
  
  test_data <- data.frame(
    the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
    the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
    the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "precondition"),
    the_val = c(65, 165, "M", 66, "F", 166, 155, NA)
  )
  
  expected_data <- data.frame(
    the_obs = c("A", "B", "C", "D"),
    the_obs2 = c("Ax", "Bx", "Cx", "Dx"),
    gender = c("M", "F", NA, NA),
    height = c("165", "166", "155", NA),
    precondition = as.character(c(NA, NA, NA, NA)),
    weight = c("65", "66", NA, NA)
  )
  
  res <- expect_silent(multi_pivot_wider(test_data, c("the_obs", "the_obs2"), "the_param", "the_val"))
  expect_identical(expected_data, res)
})

test_that("multi_pivot_wider works as expected with drop_na argument.", {
  
  test_data <- data.frame(
    the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
    the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
    the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "precondition"),
    the_val = c(65, 165, "M", 66, "F", 166, 155, NA)
  )
  
  expected_data <- data.frame(
    the_obs = c("A", "B", "C", "D"),
    the_obs2 = c("Ax", "Bx", "Cx", "Dx"),
    gender = c("M", "F", NA, NA),
    height = c("165", "166", "155", NA),
    weight = c("65", "66", NA, NA)
  )
  
  res <- expect_silent(multi_pivot_wider(test_data, c("the_obs", "the_obs2"), "the_param", "the_val", drop_na = TRUE))
  expect_identical(expected_data, res)
})

test_that("multi_pivot_wider works as expected with drop_na argument.", {
  
  test_data <- data.frame(
    the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
    the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
    the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "precondition"),
    the_val = c(65, 165, "M", 66, "F", 166, 155, NA)
  )
  
  expected_data <- data.frame(
    the_obs = c("A", "B", "C", "D"),
    the_obs2 = c("Ax", "Bx", "Cx", "Dx"),
    gender = c("M", "F", NA, NA),
    height = c("165", "166", "155", NA),
    weight = c("65", "66", NA, NA)
  )
  
  res <- expect_silent(multi_pivot_wider(test_data, c("the_obs", "the_obs2"), "the_param", "the_val", drop_na = TRUE))
  expect_identical(expected_data, res)
})

test_that("multi_pivot_wider works as expected when the unique identification deoends on several columns", {
  
  test_data <- data.frame(
    the_obs = c("A", "A", "B"),
    the_obs2 = c("Ax2", "Ax2", "Ax"),
    the_param = c("weight", "weight", "weight"),
    the_val = c(65, 65, 65)
  )
  
  expect_error(multi_pivot_wider(test_data, c("the_obs", "the_obs2"), "the_param", "the_val"))
  
  test_data2 <- data.frame(
    the_obs = c("A", "A", "B"),
    the_obs2 = c("Ax", "Ax2", "Ax"),
    the_param = c("weight", "weight", "weight"),
    the_val = c(65, 65, 65)
  )
  
  expected_data <- data.frame(
    the_obs = c("A", "A", "B"),
    the_obs2 = c("Ax", "Ax2", "Ax"),
    weight = c(65, 65, 65)
  )
  
  res <- expect_silent(multi_pivot_wider(test_data2, c("the_obs", "the_obs2"), "the_param", "the_val"))
  expect_identical(res, expected_data)
})

# poly_pivot_wider ----

test_that("poly_pivot_wider works as expected.", {
  
  test_data <- data.frame(
    the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
    the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
    the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "precondition"),
    the_label = c("Weight (Kg)", "Height (cm)", "Gender", 
                  "Weight (Kg)", "Gender", "Height (cm)", "Height (cm)", "Pre-condition"),
    the_val = c(65, 165, NA, 66, NA, 166, 155, NA),
    the_val2 = c(65, 165, "M", 66, "F", 166, 155, TRUE)
  )
  
  res <- poly_pivot_wider(test_data, c("the_obs", "the_obs2"), "the_param", c("the_val", "the_val2"), "the_label")
  
  expect_list(res, len = 2)
  
    the_obs <- c("A", "B", "C", "D")
    the_obs2 <- c("Ax", "Bx", "Cx", "Dx")
    height <- c(165, 166, 155, NA)
    weight <- c(65, 66, NA, NA)
    
    attr(the_obs, "label") <- "the_obs"
    attr(the_obs2, "label") <- "the_obs2"
    attr(height, "label") <- "Height (cm)"
    attr(weight, "label") <- "Weight (Kg)"
  
    expected_data1 <- data.frame(
      the_obs = the_obs,
      the_obs2 = the_obs2,
      height = height,
      weight = weight
    )
  
  expect_identical(res[[1]], expected_data1)
})


test_that("poly_pivot_wider works as expected with default label.", {
  
  test_data <- data.frame(
    the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
    the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
    the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "precondition"),
    the_label = c("Weight (Kg)", "Height (cm)", "Gender", "Weight (Kg)", 
                  "Gender", "Height (cm)", "Height (cm)", "Pre-condition"),
    the_val = c(65, 165, NA, 66, NA, 166, 155, NA),
    the_val2 = c(65, 165, "M", 66, "F", 166, 155, TRUE)
  )
  
  res <- poly_pivot_wider(test_data, c("the_obs", "the_obs2"), "the_param", c("the_val", "the_val2"))
  
  expect_list(res, len = 2)
  
    the_obs <- c("A", "B", "C", "D")
    the_obs2 <- c("Ax", "Bx", "Cx", "Dx")
    height <- c(165, 166, 155, NA)
    weight <- c(65, 66, NA, NA)
    
    attr(the_obs, "label") <- "the_obs"
    attr(the_obs2, "label") <- "the_obs2"
    attr(height, "label") <- "height"
    attr(weight, "label") <- "weight"
  
    expected_data1 <- data.frame(
      the_obs = the_obs,
      the_obs2 = the_obs2,
      height = height,
      weight = weight
    )
  
  expect_identical(res[[1]], expected_data1)
})
