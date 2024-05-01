#' Approximate Data Using Principal Components
#'
#' This function approximates the input data `x` using a specified number of
#' principal components (PCs).
#'
#' @param x A numeric matrix or data frame representing the input data.
#' @param npc An integer specifying the number of principal components to use
#'   for approximation.
#'
#' @return A numeric matrix of the same dimensions as `x`, containing the
#'   approximated data based on the specified number of PCs.
#'
#' @examples
#' # Generate some sample data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 5), nrow = 100, ncol = 5)
#'
#' # Approximate the data using 3 PCs
#' x_approx <- pcApprox(x, npc = 3)
#'
#' @import stats
#'
#' @export
pcApprox <- function(x, npc) {
  # Check if npc is greater than the number of columns in x
  if (npc > ncol(x)) {
    warning("npc is greater than the number of columns in x. Returning the original data.")
    return(x)
  }

  # Perform principal component analysis
  pca_res <- stats::prcomp(x, center = TRUE, scale. = TRUE)

  # Extract principal components and scores
  pcs <- pca_res$rotation[, 1:npc]
  scores <- pca_res$x[, 1:npc]

  # Reconstruct the approximated data
  x_approx <- scores %*% t(pcs)

  # Rescale and recenter the approximated data
  x_approx <- sweep(x_approx, 2, pca_res$center, "+")
  x_approx <- sweep(x_approx, 2, pca_res$scale, "*")

  return(x_approx)
}
