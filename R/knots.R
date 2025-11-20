#' The function of interior knots sequence
#'
#' This function computes a knot sequence using the quantiles of the data.
#'
#' @param x Numeric vector of the data
#' @param dimension Number of the basis functions
#' @param degree Degree of spline (default= 3)
#'
#' @return Numeric vector with interior knots sequence
#' @export
#'
#' @examples
#' set.seed(924)
#' x = runif(100, 0, 1)
#' knots = knots_quantile(x, 5, 3)
#' print(knots)

knots_quantile<- function(x, dimension, degree = 3) {
  dimension = max(dimension, degree + 1)
  num_interior = dimension - degree - 1
  if (num_interior > 0) {
    probs<- (1:num_interior)/(num_interior+1)
    interior_knots<- quantile(x, probs, type= 1)
  } else {
    interior_knots<- numeric(0)
  }

  return(interior_knots)
}

#' Add boundary knots to interior knots.

add_boundary_knots <- function(x, interior_knots, degree = 3, tiny = 1e-5) {
  knots<- c(rep(min(x) - tiny, degree + 1), interior_knots, rep(max(x) + tiny, degree + 1))
  return(knots)
}
