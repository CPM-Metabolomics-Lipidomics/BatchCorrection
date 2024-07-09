#' visualization
#'
#' @title Create trend plot
#'
#' @description Create trend plot with plotly
#'
#' @return A plotly object
#'
#' @author Rico Derks
#'
#' @importFrom plotly plot_ly
#'
#' @noRd
trend_plot = function() {
  print("Make dummy plot")
  pl <- plotly::plot_ly(x = 1:10,
                        y = 1:10,
                        type = "scatter",
                        mode = "markers")

  return(pl)
}
