#' visualization
#'
#' @title Create trend plot
#'
#' @description Create trend plot with plotly
#'
#' @param data data.frame in long format created by prepare_trend_data().
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param batch_col character(1), name of the batch column.
#'
#' @return A plotly object
#'
#' @author Rico Derks
#'
#' @importFrom plotly plot_ly add_lines layout
#'
#' @noRd
trend_plot <- function(data = NULL,
                       sampleid_raw_col = NULL,
                       batch_col = NULL) {
  data <- data[!is.na(data$log2fc), ]

  pl <- plotly::plot_ly(x = data[, sampleid_raw_col],
                        y = data[, "log2fc"],
                        color = as.factor(data[, batch_col])) |>
    plotly::add_lines(
      split = data[, "featureNames"],
      opacity = 0.3
    ) |>
    plotly::layout(
      showlegend = FALSE,
      shapes = list(
        list(
          type = "line",
          y0 = 0.5,
          y1 = 0.5,
          xref = "paper",
          x0 = 0,
          x1 = 1,
          line = list(color = "black",
                      width = 1,
                      dash = "dot")
        ),
        list(
          type = "line",
          y0 = -0.5,
          y1 = -0.5,
          xref = "paper",
          x0 = 0,
          x1 = 1,
          line = list(color = "black",
                      width = 1,
                      dash = "dot")
        )
      )
    )

  return(pl)
}


#' @title Prepare data for trend plot
#'
#' @description
#' Prepare data (wide format) for creating a trend plot (long format).
#'
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param sampleid_meta_col character(1), name of the sample id column in the meta data.
#' @param order_col character(1), name of the acquistion order column.
#' @param batch_col character(1), name of the batch column.
#' @param id_qcpool character() vector with the names of the pooled sample id's.
#'
#' @details
#' The reference is the first QCpool (pooled sample) measured in the all data set.
#'
#' @return data.frame in long format
#'
#' @author Rico Derks
#'
#' @importFrom tidyr pivot_longer contains
#'
#' @noRd
prepare_trend_data <- function(data = NULL,
                               meta_data = NULL,
                               sampleid_raw_col = NULL,
                               sampleid_meta_col = NULL,
                               order_col = NULL,
                               batch_col = NULL,
                               id_qcpool = NULL) {
  # get the data of the first pooled sample
  pool_meta <- meta_data[meta_data[, sampleid_meta_col] %in% id_qcpool, ]
  pool1 <- pool_meta[pool_meta[, order_col] == min(pool_meta[, order_col]), sampleid_meta_col]

  # make long data
  pool_data <- data[data[, sampleid_raw_col] %in% id_qcpool, ]
  pool_data <- merge(
    x = pool_data,
    y = pool_meta[, c(sampleid_meta_col, order_col, batch_col)],
    by.x = sampleid_raw_col,
    by.y = sampleid_meta_col
  )

  data_long <- pool_data |>
    tidyr::pivot_longer(
      cols = !tidyr::contains(c(sampleid_raw_col, order_col, batch_col)),
      names_to = "featureNames",
      values_to = "value"
    )

  # reference data over all batches
  ref_data <- data_long[data_long[, sampleid_raw_col] == pool1, ]
  ref_data <- ref_data[, c("featureNames", "value")]
  colnames(ref_data)[2] <- "refValue"

  # merge all
  data_long <- base::merge(
    x = data_long,
    y = ref_data,
    by = "featureNames",
    all.x = TRUE
  )

  data_long$log2fc <- log2(data_long$value / data_long$refValue)

  # make sure order of pooled samples is correct
  pool_order <- pool_meta[order(pool_meta[, order_col]), sampleid_meta_col]
  data_long[, sampleid_raw_col] <- factor(x = data_long[, sampleid_raw_col],
                                          levels = pool_order,
                                          labels = pool_order)

  # fix the batch transitions in the plot,
  # duplicate first sample of each bacht (> 1) and set previous batch
  batches <- sort(unique(data_long[, batch_col]))
  if(length(batches) >= 2) {
    for(b in 2:length(batches)) {
      batch_data <- pool_meta[pool_meta[, batch_col] == b, ]
      pool1_batch <- batch_data[batch_data[, order_col] == min(batch_data[, order_col]), sampleid_meta_col]

      fix <- data_long[data_long[, sampleid_raw_col] == pool1_batch, ]
      fix[, batch_col] <- b - 1

      data_long <- rbind(data_long, fix)
    }
  }

  return(data_long)
}


#' @title Prepare data for heatmap plot
#'
#' @description
#' Prepare data for creating a heatmap.
#'
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param sampleid_meta_col character(1), name of the sample id column in the meta data.
#' @param sampletype_col character(1), name of the sample type column.
#' @param batch_col character(1), name of the batch column.
#' @param id_samples character() vector with the names of the sample samples id's.
#' @param id_qcpool character() vector with the names of the pooled sample id's.
#'
#' @return list containing: data matrix, annotations and colors.
#'
#' @author Rico Derks
#'
#' @noRd
prepare_heatmap_data <- function(data = NULL,
                                 meta_data = NULL,
                                 sampleid_raw_col = NULL,
                                 sampleid_meta_col = NULL,
                                 sampletype_col = NULL,
                                 batch_col = NULL,
                                 id_samples = NULL,
                                 id_qcpool = NULL) {

  feature_names <- colnames(data)[-1]

  data <- merge(
    x = data,
    y = meta_data,
    by.x = sampleid_raw_col,
    by.y = sampleid_meta_col,
    all.x = TRUE
  )

  # sort the columns
  other_columns <- colnames(data)[!(colnames(data) %in% feature_names)]
  data <- data[, c(other_columns, feature_names)]

  data <- data[data[, sampleid_raw_col] %in% c(id_samples, id_qcpool), ]

  # matrix and scale
  data_m <- as.matrix(data[, feature_names])
  data_m <- base::scale(x = data_m,
                        center = TRUE,
                        scale = TRUE)
  rownames(data_m) <- data[, sampleid_raw_col]

  # annotations
  row_annotations <- data[, c(sampleid_raw_col, sampletype_col, batch_col)]
  rownames(row_annotations) <- row_annotations[, sampleid_raw_col]
  row_annotations <- row_annotations[, -1]
  row_annotations[, batch_col] <- factor(row_annotations[, batch_col])

  colors_annotation <- list(
    "SampleType" = RColorBrewer::brewer.pal(name = "Set1",
                                            n = 9)[1:length(unique(data[, sampletype_col]))],
    "Batch" = RColorBrewer::brewer.pal(name = "Set2",
                                       n = 8)[1:length(unique(data[, batch_col]))]
  )
  names(colors_annotation$SampleType) <- unique(data[, sampletype_col])
  names(colors_annotation$Batch) <- as.character(unique(data[, batch_col]))
  names(colors_annotation) <- c(sampletype_col, batch_col)

  res <- list(
    data = data_m,
    row_ann = row_annotations,
    colors_ann = colors_annotation
  )

  return(res)
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


#' @title Do PCA and prepare the data for plotting
#'
#' @description
#' Do PCA and prepare the data for plotting.
#'
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param sampleid_meta_col character(1), name of the sample id column in the meta data.
#' @param id_samples character() vector with the names of the sample samples id's.
#' @param id_qcpool character() vector with the names of the pooled sample id's.
#'
#' @return list with the PCA model, summary of fit, scores and loadings.
#'
#' @author Rico Derks
#'
#' @importFrom pcaMethods pca
#' @importFrom tidyr pivot_longer matches
#'
#' @noRd
prepare_pca_data <- function(data = NULL,
                             meta_data = NULL,
                             sampleid_raw_col = NULL,
                             sampleid_meta_col = NULL,
                             id_samples = NULL,
                             id_qcpool = NULL) {
  feature_names <- colnames(data)[-1]

  data <- merge(
    x = data,
    y = meta_data,
    by.x = sampleid_raw_col,
    by.y = sampleid_meta_col,
    all.x = TRUE
  )

  # sort the columns
  other_columns <- colnames(data)[!(colnames(data) %in% feature_names)]
  data <- data[, c(other_columns, feature_names)]

  data <- data[data[, sampleid_raw_col] %in% c(id_qcpool, id_samples), ]
  data_m <- as.matrix(data[, feature_names])
  keep_features <- apply(data_m, 2, function(x) {
    mean(is.na(x)) < 0.5
  })

  data_m <- data_m[, keep_features]


  m1 <- pcaMethods::pca(object = data_m,
                        nPcs = 4,
                        scale = "uv",
                        center = TRUE,
                        cv = "q2")

  m1_sumfit <- data.frame("PC" = factor(paste0("PC", 1:4),
                                        levels = paste0("PC", 1:4),
                                        labels = paste0("PC", 1:4)),
                          "R2cum" = m1@R2cum,
                          "Q2cum" = m1@cvstat)
  m1_sumfit <- m1_sumfit |>
    tidyr::pivot_longer(cols = !tidyr::matches("PC"),
                        names_to = "variable",
                        values_to = "value")

  m1_scores <- cbind(m1@scores,
                     data[, other_columns])

  m1_loadings <- as.data.frame(m1@loadings)
  m1_loadings$featureNames <- rownames(m1_loadings)

  res <- list(
    "model" = m1,
    "summary_of_fit" = m1_sumfit,
    "scores" = m1_scores,
    "loadings" = m1_loadings
  )

  return(res)
}


#' @title PCA scores plot
#'
#' @description
#' PCA scores plot.
#'
#' @param data list from output of prepare_pca_data().
#' @param sampletype_col character(1), name of the sample type column.
#' @param batch_col character(1), name of the batch column.
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
#' @noRd
pca_scores_plot <- function(data = NULL,
                            sampletype_col = NULL,
                            batch_col = NULL) {
  pc_main <- data$scores |>
    ggplot2::ggplot(ggplot2::aes(x = .data$PC1,
                                 y = .data$PC2)) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey") +
    ggplot2::geom_vline(xintercept = 0,
                        color = "grey") +
    ggplot2::geom_polygon(data = simple_ellipse(x = data$scores$PC1,
                                                y = data$scores$PC2,
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
    ggplot2::labs(x = sprintf("PC1 (%0.1f%%)", data$model@R2[1] * 100),
                  y = sprintf("PC2 (%0.1f%%)", data$model@R2[2] * 100)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")


  pc_x_dens <- data$scores |>
    ggplot2::ggplot(ggplot2::aes(x = .data$PC1,
                                 fill = as.factor(.data[[batch_col]]))) +
    ggplot2::geom_density(alpha = 0.3,
                          linewidth = 0.1) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   axis.line.x = ggplot2::element_line(),
                   axis.title.y = ggplot2::element_text(angle = 90))

  pc_y_dens <- data$scores |>
    ggplot2::ggplot(ggplot2::aes(y = .data$PC2,
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
#'
#' @return ggplot2 object, the loadings plot.
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline .data
#'   geom_point  theme_minimal theme labs
#'
#' @noRd
pca_loadings_plot <- function(data = NULL) {
  p <- data$loadings |>
    ggplot2::ggplot(ggplot2::aes(x = .data$PC1,
                                 y = .data$PC2)) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey") +
    ggplot2::geom_vline(xintercept = 0,
                        color = "grey") +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(x = sprintf("PC1 (%0.1f%%)", data$model@R2[1] * 100),
                  y = sprintf("PC2 (%0.1f%%)", data$model@R2[2] * 100)) +
    ggplot2::theme_minimal()

  return(p)
}


#' @title Prepare data for histogram
#'
#' @description
#' Prepare data for histogram
#'
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param sampleid_meta_col character(1), name of the sample id column in the meta data.
#' @param batch_col character(1), name of the batch column.
#' @param id_qcpool character() vector with the names of the pooled sample id's.
#'
#' @return list with the PCA model, summary of fit, scores and loadings.
#'
#' @author Rico Derks
#'
#' @importFrom tidyr pivot_longer matches
#' @importFrom stats sd
#'
#' @noRd
prepare_hist_data <- function(data = NULL,
                              meta_data = NULL,
                              sampleid_raw_col = NULL,
                              sampleid_meta_col = NULL,
                              batch_col = NULL,
                              id_qcpool = NULL) {
  feature_names <- colnames(data)[-1]

  data <- merge(
    x = data,
    y = meta_data,
    by.x = sampleid_raw_col,
    by.y = sampleid_meta_col,
    all.x = TRUE
  )

  # sort the columns
  other_columns <- colnames(data)[!(colnames(data) %in% feature_names)]
  data <- data[, c(other_columns, feature_names)]

  data <- data[data[, sampleid_raw_col] %in% id_qcpool, ]

  data_long <- data |>
    tidyr::pivot_longer(
      cols = !tidyr::matches(other_columns),
      names_to = "featureNames",
      values_to = "value"
    )

  hist_data_all <- data.frame("rsd" = tapply(data_long, list(data_long$featureNames), function(x) {
    stats::sd(x$value, na.rm = TRUE) / mean(x$value, na.rm = TRUE)
  }))
  hist_data_all$featureName <- rownames(hist_data_all)


  hist_data_batch <- as.data.frame(tapply(data_long, list(data_long$featureNames, data_long[, batch_col, drop = TRUE]), function(x) {
    stats::sd(x$value, na.rm = TRUE) / mean(x$value, na.rm = TRUE)
  }))
  hist_data_batch$featureNames <- rownames(hist_data_batch)

  hist_data_batch <- hist_data_batch |>
    tidyr::pivot_longer(cols = !tidyr::matches("featureNames"),
                        names_to = "Batch",
                        values_to = "rsd")
  hist_data_batch$Batch <- factor(hist_data_batch$Batch)

  res <- list(
    "overall" = hist_data_all,
    "batch" = hist_data_batch
  )
}


#' @title Histogram plot
#'
#' @description
#' Histogram plot.
#'
#' @param data list from output of prepare_pca_data().
#'
#' @return ggplot2 object, the scores plot with density plots around it.
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes geom_vline .data
#'   theme_minimal theme labs facet_wrap
#' @importFrom patchwork wrap_plots
#'
#' @noRd
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
