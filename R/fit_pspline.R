#' Fitting P-spline model
#'
#' This function computes the coefficients P-spline estimator.
#'
#' @param x_values Numeric vector of x (input)
#' @param y_values Numeric vector of y (response)
#' @param interior_knots Vector of interior knots
#' @param degree Degree of the B-spline basis (default = 3)
#' @param lambda Smoothing parameter (default = 1e-2)
#' @param diff_order Order of the difference penalty (default = 2)
#'
#' @return List contains the spline coefficients, knots, degree, lambda, penalty matrix.
#'
#' @examples
#' set.seed(924)
#' n= 100
#' x_values= sort(runif(n, 0, 1))
#' y_values= sin(2*pi*x_values) + cos(4*pi*x_values) + rnorm(n, sd= 0.2)
#' # spline degree specification
#' degree= 3
#' # knot generation
#' num_interior= 5
#' interior_knots= knots_quantile(x_values, num_interior)
#' # setting the value of lambda and the difference penalty order
#' lambda= 1e-2
#' diff_order= 2
#' # model fitting
#' model= fit_pspline(x_values, y_values, interior_knots, degree, lambda, diff_order)
#' print(model)
#'
#'
#' @export
fit_pspline <- function(x_values, y_values, interior_knots,
                        degree = 3, lambda = 1e-2, diff_order = 2){

  # 1. Set the whole knots
  knots <- add_boundary_knots(x_values, interior_knots, degree)

  # 2. Compute the design matrix of B-spline
  G <- create_design_matrix(x_values, knots, degree)

  # 3. 차분 행렬 D 생성
  num_basis <- ncol(G)
  D <- diff(diag(num_basis), differences = diff_order)

  # 4. Compute the penalty matrix (P = D^T D)
  P <- t(D) %*% D

  # 5. Penalized Least Squares
  beta_hat <- solve(t(G) %*% G + lambda * P) %*% t(G) %*% y_values

  return(list(beta = beta_hat, knots = knots, degree = degree,
              lambda = lambda, penalty = P))
}
