#' Influential Measures Calculation
#'
#' The Influential Points Analysis package includes effective methods for identifying and evaluating significant data points in linear regression models. This program helps users in identifying data points with a significant effect on the regression model's parameters by computing several influence measures such as Cook's Distance, DFFITS, and Hadi's Influence Measure. The package additionally includes visualization features that allow users to quickly identify influential points using appealing lollipop charts.
#'
#' @param model lm model object
#' @param type name of influence measure
#' @param threshold optional threshold value
#'
#' @return list contains influence scores and influential points, plot visualizing influential points
#' @importFrom graphics points
#' @importFrom graphics abline
#' @importFrom stats sd
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' output1 <- influential(model, type = "cooks", threshold = 0.3)
#' output2 <- influential(model, type = "dffits", threshold = 0.5)
#' output3 <- influential(model, type = "hadi", threshold = 0.5)

influential <- function(model, type, threshold = NULL) {

  # Input validation - model
  # Check if the model is an lm object
  if (all(is.null(model))) stop("Input need to be a non NULL value.")
  # if (all(is.na(data))) stop("Input need to be a non NA value.")
  # if (all(is.infinite(data))) stop("Input need to be a non Inf value")
  if (!inherits(model, "lm")) stop("'model' argument must be an object of class 'lm'.")
  if (length(dim(model$model)) != 2) stop("'model' argument must have 2D shape.")

  # Check for NA or Inf values in model components
  if (anyNA(model$model)) stop("Invalid input. 'model' argument contains NA values.")
  if (any(is.infinite(as.matrix(model$model)))) stop("Invalid input. 'model' argument contains Inf values.")

  # Input validation - type
  # Check if 'type' is valid
  if (!(type %in% c("cooks", "dffits", "hadi"))) stop("Invalid input. 'type' must be from 'cooks', 'dffits', or 'hadi'.")

  # Input validation - threshold
  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1) {
      stop("The 'threshold' argument must be a single numeric value.")
    }
  }

  # Extract necessary components
  X <- as.matrix(model$model)

  if (length(dim(X)) != 2) stop("Invalid input dimension.")

  n <- nrow(X)
  p <- ncol(X)
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  h <- diag(H)
  residuals <- residuals(model)
  SSR <- sum(residuals^2) / (n - p)

  # Calculate influence measure based on 'type'
  if (type == "cooks") {
    influence_scores <- custom_cooks_distance(p, h, residuals, SSR)
    measure_name <- "Cook's Distance"
  } else if (type == "dffits") {
    influence_scores <- custom_dffits(p, h, residuals, SSR)
    measure_name <- "DFFITS"
  } else if (type == "hadi") {
    influence_scores <- hadi_influence_measure(p, h, residuals, SSR)
    measure_name <- "Hadi's Influence Measure"
  }

  # Check for NA or Inf values in influence scores
  if (anyNA(influence_scores)) {
    warning("NA values detected and ignored in the influence scores.")
    influence_scores <- na.omit(influence_scores)
  }

  if (any(is.infinite(influence_scores))) {
    warning("Infinite values detected & ignored in the influence scores.")
    influence_scores <- influence_scores[!is.infinite(influence_scores)]
  }

  if (!is.null(threshold)) {
    if (threshold > max(influence_scores, na.rm = TRUE) || threshold < min(influence_scores, na.rm = TRUE)) {
      warning("The specified 'threshold' is out of the range values of influence scores.")
    }
  }

  # Plot the influence measure
  influence_df <- data.frame(Observation = 1:length(influence_scores), Influence = influence_scores)
  plot(influence_df$Observation, influence_df$Influence, type = "h",
       main = paste(measure_name, "for the Data Points"), xlab = "Observation", ylab = measure_name, col = "blue")

  # If a threshold is provided, highlight points above it
  if (!is.null(threshold)) {
    abline(h = threshold, col = "red", lty = 2)
    points(which(influence_scores > threshold), influence_scores[influence_scores > threshold],
           col = "red", pch = 19)
  }

  # Determine influential points based on threshold or default criteria
  # influential_points <- which(influence_scores > (threshold %||% mean(influence_scores) + 2 * sd(influence_scores)))
  influential_points <- influence_scores[which(influence_scores >= threshold)]

  # Return a list of influence scores and influential points
  return(list(Influence_Values = influence_scores, Influential_Points = influential_points))
}
