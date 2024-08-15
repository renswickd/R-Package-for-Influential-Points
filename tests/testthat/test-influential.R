model <- lm(mpg ~ wt + hp, data = mtcars)

# Test cases - Positive Scenario
test_that("Valid input: Cook's distance", {
  output <- influential(model, type = "cooks")
  expect_true(is.list(output))
  expect_true("Influence_Values" %in% names(output))
  expect_true("Influential_Points" %in% names(output))
  expect_true(length(output$Influence_Values) == nrow(mtcars))
})

test_that("Valid input: DFFITS", {
  output <- influential(model, type = "dffits", threshold = 0.5)
  expect_true(is.list(output))
  expect_true("Influence_Values" %in% names(output))
  expect_true("Influential_Points" %in% names(output))
  expect_true(length(output$Influence_Values) == nrow(mtcars))
})

test_that("Valid input: Hadi's influence measure", {
  output <- influential(model, type = "hadi", threshold = 0.5)
  expect_true(is.list(output))
  expect_true("Influence_Values" %in% names(output))
  expect_true("Influential_Points" %in% names(output))
  expect_true(length(output$Influence_Values) == nrow(mtcars))
})

# Test cases - Exception Scenario
test_that("Invalid model object", {
  expect_error(influential(NULL, type = "cooks"), "Input need to be a non NULL value.")
  expect_error(influential(mtcars, type = "cooks"), "'model' argument must be an object of class 'lm'.")
})

test_that("Model with NA values", {
  mtcars_with_na <- mtcars
  model_with_na <- lm(mpg ~ wt + hp, data = mtcars_with_na)
  model_with_na$model[1, ] <- NA
  expect_error(influential(model_with_na, type = "cooks"), "Invalid input. 'model' argument contains NA values.")
})

test_that("Model with Inf values", {
  mtcars_with_inf <- mtcars
  model_with_inf <- lm(mpg ~ wt + hp, data = mtcars_with_inf)
  model_with_inf$model[1, ] <- Inf
  expect_error(influential(model_with_inf, type = "cooks"), "Invalid input. 'model' argument contains Inf values.")
})

test_that("Invalid type argument", {
  expect_error(influential(model, type = "invalid"), "Invalid input. 'type' must be from 'cooks', 'dffits', or 'hadi'.")
})

test_that("Invalid threshold", {
  expect_error(influential(model, type = "cooks", threshold = "random"), "The 'threshold' argument must be a single numeric value.")
  expect_error(influential(model, type = "cooks", threshold = c(0.3, 0.4)), "The 'threshold' argument must be a single numeric value.")
})

test_that("Out-of-range threshold", {
  expect_warning(influential(model, type = "cooks", threshold = 100), "The specified 'threshold' is out of the range values of influence scores.")
})

