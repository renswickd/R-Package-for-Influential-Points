model <- lm(mpg ~ wt + hp, data = mtcars)

# Test cases - Positive Scenario
test_that("Valid input: Cook's distance", {
  output <- influential(model, type = "cooks", threshold = 0.3)
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
  expect_error(influential(mtcars, type = "cooks"), "'model' argument must be an object of class 'lm'.")
})

# test_that("Model with NA values", {
#   mtcars_with_na <- mtcars
#   mtcars_with_na[1, 1] <- NA
#   model_with_na <- lm(mpg ~ wt + hp, data = mtcars_with_na)
#   expect_error(influential(model_with_na, type = "cooks"), "Invalid input. 'model' argument contains NA values.")
# })
#
# test_that("Model with Inf values", {
#   mtcars_with_inf <- mtcars
#   mtcars_with_inf[1, 1] <- Inf
#   model_with_inf <- lm(mpg ~ wt + hp, data = mtcars_with_inf)
#   expect_error(influential(model_with_inf, type = "cooks"), "Invalid input. 'model' argument contains Inf values.")
# })

test_that("Invalid type argument", {
  expect_error(influential(model, type = "invalid"), "Invalid input. 'type' must be from 'cooks', 'dffits', or 'hadi'.")
})

test_that("Invalid threshold: non-numeric", {
  expect_error(influential(model, type = "cooks", threshold = "high"), "The 'threshold' argument must be a single numeric value.")
})

test_that("Invalid threshold: multiple values", {
  expect_error(influential(model, type = "cooks", threshold = c(0.3, 0.4)), "The 'threshold' argument must be a single numeric value.")
})

test_that("Out-of-range threshold", {
  expect_warning(influential(model, type = "cooks", threshold = 100), "The specified 'threshold' is out of the range values of influence scores.")
})

# test_that("NA values in influence scores", {
#   model_with_na_scores <- model
#   model_with_na_scores$residuals[1] <- NA
#   output <- influential(model_with_na_scores, type = "cooks")
#   expect_warning(output, "NA values detected and ignored in the influence scores.")
# })
#
# test_that("Inf values in influence scores", {
#   model_with_inf_scores <- model
#   model_with_inf_scores$residuals[1] <- Inf
#   output <- influential(model_with_inf_scores, type = "cooks")
#   expect_warning(output, "Infinite values detected & ignored in the influence scores.")
# })
