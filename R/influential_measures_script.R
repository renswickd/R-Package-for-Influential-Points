influential <- function(model, type, threshold = NULL) {

  n <- nrow(model$model)
  p <- length(coef(model))
  h <- hatvalues(model)
  residuals <- residuals(model)
  SSR <- sum(residuals^2) / (n - p)

  if (type == "cooks") {
    influence_scores <- custom_cooks_distance(p, h, residuals, SSR)
    measure_name <- "Cook's Distance"
  } else if (type == "dffits") {
    influence_scores <- custom_dffits(p, h, residuals, SSR)
    measure_name <- "DFFITS"
  } else if (type == "hadi") {
    influence_scores <- hadi_influence_measure(p, h, residuals, SSR)
    measure_name <- "Hadi's Influence Measure"
  } else {
    stop("Invalid type specified. Choose from 'cooks', 'dffits', or 'hadi'.")
  }

  # Plot the influence measure
  influence_df <- data.frame( Observation = 1:length(influence_scores), Influence = influence_scores)
  plot(influence_df$Observation, influence_df$Influence, type = "h",
       main = paste(measure_name, "for the Data Points"), xlab = "Observation", ylab = measure_name, col = "blue")

  if (!is.null(threshold)) {
    abline(h = threshold, col = "red", lty = 2)
    points(which(influence_scores > threshold), influence_scores[influence_scores > threshold],
           col = "red", pch = 19)
  }

  # Return a list of influential points
  influential_points <- which(influence_scores > (threshold %||% mean(influence_scores) + 2 * sd(influence_scores)))
  return(list(Influence_Values = influence_scores, Influential_Points = influential_points))
}
