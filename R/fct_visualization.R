#' visualization
#'
#' @title Create trend plot
#'
#' @description Create trend plot with plotly
#'
#' @param data data.frame in long format created by prepare_trend_data().
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param batch_col character(1), name of the batch column.
#' @param yaxis character(1), one of "log2fc" or "log2fc_batch".
#'
#' @return A plotly object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_minimal labs
#'   geom_hline guides guide_legend theme element_text
#'
#' @export
trend_plot <- function(data = NULL,
                       sampleid_raw_col = NULL,
                       batch_col = NULL,
                       yaxis = c("log2fc", "log2fc_batch")) {
  yaxis <- match.arg(arg = yaxis,
                     choices = c("log2fc", "log2fc_batch"))

  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[sampleid_raw_col]],
                                 y = .data[[yaxis]],
                                 color = as.factor(.data[[batch_col]]),
                                 group = .data[["featureNames"]])) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::geom_line(alpha = 0.3) +
    ggplot2::geom_hline(yintercept = c(-0.5, 0.5),
                        linetype = 2,
                        color = "black") +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Batch")) +
    ggplot2::labs(x = "Sample ID",
                  y = "Log2(fold change)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1))

  return(p)
}


#' @title Create a Hotelling T2 ellipse for a PCA score plot
#'
#' @description This function can be used to create a confidence (Hotelling T2) interval for a
#' PCA score plot.
#'
#' @param x x vector
#' @param y y vector
#' @param alpha confidence interval
#' @param len number of points to create the ellipse
#'
#' @return A data frame is returned with the points to create the ellipse.
#'
#' @details This is a helper function which is used to create a confidence (Hotelling T2) interval for a
#' PCA score plot.
#'
#' @importFrom stats var qf
#'
#' @author Rico Derks
#'
#' @noRd
simple_ellipse <- function(x, y, alpha = 0.95, len = 200) {
  N <- length(x)
  mypi <- seq(0, 2 * pi, length = len)

  r1 <- sqrt(var(x) * qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  r2 <- sqrt(var(y) * qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))

  result <- data.frame(x = (r1 * cos(mypi) + mean(x)),
                       y = (r2 * sin(mypi) + mean(y)))

  return(result)
}


#' @title PCA scores plot
#'
#' @description
#' PCA scores plot.
#'
#' @param data list from output of prepare_pca_data().
#' @param sampletype_col character(1), name of the sample type column.
#' @param batch_col character(1), name of the batch column.
#' @param xaxis character(1), which PC to show on the x-axis.
#' @param yaxis character(1), which PC to show on the y-axis.
#'
#' @return ggplot2 object, the scores plot with density plots around it.
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline geom_polygon .data
#'   geom_point guides theme_minimal theme theme_void geom_density element_line
#'   element_text labs
#' @importFrom patchwork plot_spacer plot_layout
#'
#' @export
pca_scores_plot <- function(data = NULL,
                            sampletype_col = NULL,
                            batch_col = NULL,
                            xaxis = "PC1",
                            yaxis = "PC2") {
  pc_main <- data$scores |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[xaxis]],
                                 y = .data[[yaxis]])) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey") +
    ggplot2::geom_vline(xintercept = 0,
                        color = "grey") +
    ggplot2::geom_polygon(data = simple_ellipse(x = data$scores[[xaxis]],
                                                y = data$scores[[yaxis]],
                                                alpha = 0.95),
                          ggplot2::aes(x = .data$x,
                                       y = .data$y),
                          colour = "gray",
                          fill = "white",
                          alpha = 0.3) +
    ggplot2::geom_point(ggplot2::aes(colour = as.factor(.data[[batch_col]]),
                                     shape = .data[[sampletype_col]]),
                        size = 3) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Batch"),
                    shape = ggplot2::guide_legend(title = "Sample type")) +
    ggplot2::labs(x = sprintf("%s (%0.1f%%)", xaxis,  data$model@R2[xaxis] * 100),
                  y = sprintf("%s (%0.1f%%)", yaxis, data$model@R2[yaxis] * 100)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")


  pc_x_dens <- data$scores |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[xaxis]],
                                 fill = as.factor(.data[[batch_col]]))) +
    ggplot2::geom_density(alpha = 0.3,
                          linewidth = 0.1) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   axis.line.x = ggplot2::element_line(),
                   axis.title.y = ggplot2::element_text(angle = 90))

  pc_y_dens <- data$scores |>
    ggplot2::ggplot(ggplot2::aes(y = .data[[yaxis]],
                                 fill = as.factor(.data[[batch_col]]))) +
    ggplot2::geom_density(alpha = 0.3,
                          linewidth = 0.1) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   axis.line.y = ggplot2::element_line(),
                   axis.title.x = ggplot2::element_text(angle = 180))

  p <- pc_x_dens +
    patchwork::plot_spacer() +
    pc_main +
    pc_y_dens +
    patchwork::plot_layout(ncol = 2,
                           widths = c(5, 1),
                           heights = c(1, 5))

  return(p)
}


#' @title PCA loadings plot
#'
#' @description
#' PCA loadings plot.
#'
#' @param data list from output of prepare_pca_data().
#' @param xaxis character(1), which PC to show on the x-axis.
#' @param yaxis character(1), which PC to show on the y-axis.
#'
#' @return ggplot2 object, the loadings plot.
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline .data
#'   geom_point  theme_minimal theme labs
#'
#' @export
pca_loadings_plot <- function(data = NULL,
                              xaxis = "PC1",
                              yaxis = "PC2") {
  p <- data$loadings |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[xaxis]],
                                 y = .data[[yaxis]])) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey") +
    ggplot2::geom_vline(xintercept = 0,
                        color = "grey") +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(x = sprintf("%s (%0.1f%%)", xaxis, data$model@R2[xaxis] * 100),
                  y = sprintf("%s (%0.1f%%)", yaxis, data$model@R2[yaxis] * 100)) +
    ggplot2::theme_minimal()

  return(p)
}


#' @title Histogram plot
#'
#' @description
#' Histogram plot.
#'
#' @param data list from output of prepare_histogram_data().
#'
#' @return ggplot2 object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes geom_vline .data
#'   theme_minimal theme labs facet_wrap
#' @importFrom patchwork wrap_plots
#'
#' @export
histogram_plot <- function(data = NULL) {
  p1 <- data$overall |>
    ggplot2::ggplot(ggplot2::aes(x = .data$rsd)) +
    ggplot2::geom_histogram(binwidth = 0.05) +
    ggplot2::geom_vline(xintercept = 0.3,
                        linetype = 2,
                        color = "red") +
    ggplot2::labs(x = "Relative standard deviation",
                  title = "Over all batches") +
    ggplot2::theme_minimal()

  p2 <- data$batch |>
    ggplot2::ggplot(ggplot2::aes(x = .data$rsd)) +
    ggplot2::geom_histogram(binwidth = 0.05) +
    ggplot2::geom_vline(xintercept = 0.3,
                        linetype = 2,
                        color = "red") +
    ggplot2::labs(x = "Relative standard deviation",
                  title = "Per batch") +
    ggplot2::facet_wrap(. ~ Batch,
                        ncol = 2) +
    ggplot2::theme_minimal()

  patchwork::wrap_plots(p1, p2,
                        ncol = 2)
}


#' @title Relative log expression plot
#'
#' @description
#' Relative log expression plot.
#'
#' @param data data.frame from output of prepare_rle_data().
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param batch_col character(1), name of the batch column in the meta data.
#'
#' @return ggplot2 object.
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes geom_hline .data geom_boxplot
#'   theme_minimal theme labs guides guide_legend
#'
#' @export
rle_plot <- function(data = NULL,
                     sampleid_raw_col = NULL,
                     batch_col = NULL) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[sampleid_raw_col]],
                                 y = log10(.data[["value"]]),
                                 fill = as.factor(.data[[batch_col]]))) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey") +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Sample ID",
                  y = "Deviation") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Batch")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1,
                                                       size = 5),
                   legend.position = "bottom")

  return(p)
}


#' @title Missing values plot
#'
#' @description
#' Missing values plot.
#'
#' @param data data.frame from output of prepare_missing_data().
#' @param title character(1), title of the plot.
#'
#' @return ggplot2 object.
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes .data geom_col theme_minimal labs
#'     scale_fill_manual coord_flip
#'
#' @export
missing_plot <- function(data = NULL,
                         title = NULL) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$featureNames,
                                 y = .data$proportion,
                                 fill = .data$missing)) +
    ggplot2::geom_col(width = 1) +
    ggplot2::scale_fill_manual(name = "",
                               values = c('steelblue', 'tomato3'),
                               labels = c("Present", "Missing")) +
    ggplot2::coord_flip() +
    ggplot2::labs(y = "Proportion missing",
                  x = "Feature names",
                  title = title) +
    ggplot2::theme_minimal()

  return(p)
}
