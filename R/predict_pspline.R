#' Predicting the values of y for given x based on P-spline
#'
#' This function computes the predicted values of y for given x based on P-spline.
#'
#' @param model Function that fit P-spline with given data points
#'
#' @param new_x Numeric vector expressing a grid of evaluation points
#'
#' @return Numeric vector with predicted values at new_x
#'
#' @export
predict_pspline = function(model, new_x){
  G_new = create_design_matrix(new_x, model$knots, model$degree)
  y_pred = G_new %*% model$beta
  return(y_pred)
}
