#' Reverse the Centering and Scaling of a Numeric Vector
#'
#' This function reverses the scaling and centering transformations applied to a numeric vector,
#' typically done by the \code{scale()} function. It checks for and uses the attributes 'scaled:center' and
#' 'scaled:scale' stored in the scaled vector to reconstruct the original data.
#'
#' @param x A numeric vector that has been scaled and centered.
#' @return The original unscaled and uncentered vector.
#' @export
#' @examples
#' original <- c(1, 2, 3, 4, 5)
#' scaled <- scale(original)
#' unscale(scaled)
#' # Should return the original vector
#' @export
unscale <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  if (is.null(attr(x, "scaled:center")) || is.null(attr(x, "scaled:scale"))) {
    stop("Input vector does not appear to be scaled.")
  }
  original <- (x * attr(x, "scaled:scale")) + attr(x, "scaled:center")
  return(original)
}
