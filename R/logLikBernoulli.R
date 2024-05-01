#' Calculate the parameter p that maximizes the log-likelihood for Bernoulli trials
#'
#' This function takes a vector of Bernoulli trials and calculates the parameter p
#' that maximizes the log-likelihood.
#'
#' @param data A vector of Bernoulli trials (0 or 1).
#' @return The parameter p that maximizes the log-likelihood.
#' @export
#' @examples
#' data <- c(1, 0, 0, 0, 1, 1, 1)
#' best_p <- logLikBernoulli(data)
#' print(best_p)
#' @export
logLikBernoulli <- function(data) {
  max_log_likelihood <- -Inf
  best_p <- 0.5  # Initialize with a default value
  p_values <- seq(0.001, 0.999, by = 0.001)  # Avoid 0 and 1 to prevent log(0) and log(1)
  for (p in p_values) {
    if (p == 0 || p == 1) next  # Skip values of 0 and 1
    log_likelihood <- sum(data * log(p) + (1 - data) * log(1 - p))
    if (log_likelihood > max_log_likelihood) {
      max_log_likelihood <- log_likelihood
      best_p <- p
    }
  }
  return(best_p)
}


