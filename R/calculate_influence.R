# To calculate Cook's Distance manually
custom_cooks_distance <- function(n.coefs, fitted.values, residuals, SSR) {
  cooks_distance_measure <- (residuals^2 / (n.coefs * SSR)) * (fitted.values / (1 - fitted.values)^2)
  return(cooks_distance_measure)
}

# To calculate DFFITS manually
custom_dffits <- function(n.coefs, fitted.values, residuals, SSR) {
  standardized_residuals <- residuals / sqrt(SSR * (1 - fitted.values))
  dffits_measure <- standardized_residuals * sqrt(fitted.values / (1 - fitted.values))
  return(dffits_measure)
}

# To calculate Hadi's Influence Measure
hadi_influence_measure <- function(n.coefs, fitted.values, residuals, SSR) {
  standardized_residuals <- residuals / sqrt(SSR * (1 - fitted.values))
  val1 <- fitted.values / (1 - fitted.values)
  val2 <- (standardized_residuals^2) / (n.coefs * (1 - fitted.values))
  hadi_measure <- val1 + val2
  return(hadi_measure)
}
