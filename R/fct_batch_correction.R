#' batch_correction
#'
#' @title Median batch correction
#'
#' @description Median batch correction
#'
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param sampleid_meta_col character(1), name of the sample id column in the meta data.
#' @param id_samples character() vector with the names of the sample samples id's.
#' @param id_qcpool character() vector with the names of the pooled sample id's.
#' @param batch_col character(1), name of the batch column.
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom stats median
#'
#' @author Rico Derks
#'
#' @noRd
median_bc <- function(data = NULL,
                      meta_data = NULL,
                      sampleid_raw_col = NULL,
                      sampleid_meta_col = NULL,
                      id_samples = NULL,
                      id_qcpool = NULL,
                      batch_col = NULL) {
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
  data <- data[data[, sampleid_raw_col] %in% c(id_qcpool, id_samples), c(other_columns, feature_names)]
  print(dim(data))

  cor_data <- data
  cor_data[, batch_col] <- factor(cor_data[, batch_col])
  batches <- as.character(unique(cor_data[, batch_col]))

  overall_median <- apply(as.matrix(cor_data[cor_data[, sampleid_raw_col] %in% id_qcpool, feature_names]), 2, stats::median, na.rm = TRUE)

  for(b in 1:length(batches)) {
    idx <- cor_data[, batch_col] == batches[b]

    if(!all(idx == FALSE)) {
      tmp <- as.matrix(cor_data[idx & cor_data[, sampleid_raw_col] %in% id_qcpool, feature_names])

      cor_factor <- overall_median / apply(tmp, 2, stats::median, na.rm = TRUE)

      cor_data[idx, feature_names] <- t(t(cor_data[idx, feature_names]) * cor_factor)
    }
  }

  return(cor_data[, c(sampleid_raw_col, feature_names)])
}
