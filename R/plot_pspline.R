#' Plotting P-spline with scatter plots
#'
#' This function provides a plot of the P-spline.
#'
#' @param x_values,y_values Numeric vector
#' @param model Function that fit P-spline with given data points
#' @param grid_x Numeric vector with a grid of evaluation points
#'
#' @export
#' @import ggplot2
plot_pspline = function(x_values, y_values, model, grid_x)
{
  y_pred = predict_pspline(model, grid_x)
  data_plot = data.frame(x = x_values, y = y_values)
  spline_plot = data.frame(x = grid_x, y = y_pred)

  ggplot() +
    geom_point(data = data_plot, aes(x, y), color = "blue") +
    geom_line(data = spline_plot, aes(x, y), color = "red", linewidth = 1.2) +
    labs(title = "Fitted P-spline Regression", x = "x", y = "y") +
    xlim(c(min(x_values), max(x_values)))
}
