#' Calculate minimum sample size for t-test
#'
#' This function calculates the minimum sample size needed for a t-test with specified power and alpha level.
#' @param x1 A vector of observations for the first sample.
#' @param x2 A vector of observations for the second sample (optional).
#' @return The minimum sample size needed for the t-test.
#' @importFrom pwr pwr.t2n.test
#' @examples
#' # One-sample case
#' minimumN(c(5.2, 4.8, 5.1, 4.9, 5.3))
#'
#' # Two-sample case
#' minimumN(c(5.2, 4.8, 5.1, 4.9, 5.3), c(4.7, 5.1, 4.9, 5.2, 4.8))
#' @export
minimumN <- function(x1, x2 = NULL) {
  if (is.null(x2)) {
    # One sample case: testing if mean equals 0
    result <- pwr::pwr.t.test(d = mean(x1), sig.level = 0.05, power = 0.8, type = "one.sample", alternative = "two.sided")$n
  } else {
    # Two sample case: testing if means are equal
    pooled_var <- ((length(x1) - 1) * var(x1) + (length(x2) - 1) * var(x2)) / (length(x1) + length(x2) - 2)
    cohens_d <- abs(mean(x1) - mean(x2)) / sqrt(pooled_var)
    result <- pwr::pwr.t.test(d = cohens_d, sig.level = 0.05, power = 0.8, alternative = "two.sided")$n
  }

  return(ceiling(result))
}
