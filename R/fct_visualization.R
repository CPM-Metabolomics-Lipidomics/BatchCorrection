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
