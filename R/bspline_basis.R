#' B-spline basis function

b_spline <- function(x, knots, degree, i) {
  # Base case: 0th degree
  if (degree == 0) {
    return(ifelse(knots[i] <= x & x < knots[i + 1], 1, 0))
  }

  # Recursive case: degree > 0
  B_i_d1 <- b_spline(x, knots, degree - 1, i)
  B_i1_d1 <- b_spline(x, knots, degree - 1, i + 1)

  denom1 <- knots[i + degree] - knots[i]
  denom2 <- knots[i + degree + 1] - knots[i + 1]

  term1 <- if (denom1 == 0) 0 else ((x - knots[i]) / denom1)*B_i_d1
  term2 <- if (denom2 == 0) 0 else ((knots[i + degree + 1] - x) / denom2)*B_i1_d1

  return(term1 + term2)
}

#' Create design matrix of B-spline basis

create_design_matrix <- function(x_values, knots, degree){
  n <- length(x_values) # number of the data
  num_basis <- length(knots) - degree - 1 # number of the basis function

  design_matrix <- matrix(0, nrow = n, ncol = num_basis)

  for (j in 1:num_basis){
    for (i in 1:n){
      design_matrix[i, j] <- b_spline(x_values[i], knots, degree, j)
    }
  }

  return(design_matrix)
}
