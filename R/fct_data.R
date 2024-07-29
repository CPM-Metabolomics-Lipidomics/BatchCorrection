#' data
#'
#' @title Read the data file
#'
#' @description
#' Read the data file. It can be in different formats
#'
#' @param file_path character(1) containing the file path to the imported data
#'     file.
#' @param sep character(1) for several file formats a separator can be specified.
#' @param first_column_as_index logical(1) should the first column be used as
#'     index.
#'
#' @return data.frame
#'
#' @importFrom stringr str_sub
#' @importFrom readxl read_xlsx
#' @importFrom utils read.table
#'
#' @author Damien Olivier
#' @author Rico Derks
#'
read_data <- function(file_path = NULL,
                      sep = NULL,
                      first_column_as_index = FALSE) {
  if(is.null(file_path)) {
    stop("No file path specified!")
  } else {
    if(!file.exists(file_path)) {
      stop("File path does not exist!")
    }
  }

  # if (is.na(sep)) {
  #   if (stringr::str_sub(file_path, -4, -1) == ".tsv") {
  #     sep <- '\t'
  #   }
  # }

  if (first_column_as_index) {
    index <- 1
  } else {
    index <- NULL
  }

  if (stringr::str_sub(file_path, -5, -1) == ".xlsx") {
    data_table <- as.data.frame(readxl::read_xlsx(path = file_path))
  } else {
    if (is.null(sep)) {
      sep <- find_delim(file_path = file_path)
      data_table <- utils::read.table(file_path,
                                      header = TRUE,
                                      sep = sep,
                                      check.names = FALSE)
    } else {
      data_table <- utils::read.table(file_path,
                                      header = TRUE,
                                      sep = sep,
                                      check.names = FALSE)
    }
  }

  # if (!is.null(index)) {
  #   duplicates <- duplicated(data_table[, index])
  #   if (sum(duplicates) > 0) {
  #     print(paste0('Removed ', sum(duplicates), ' duplicated features'))
  #     data_table <- data_table[!duplicates, ]
  #     rownames(data_table) <- data_table[, 1]
  #     data_table[, 1] <- NULL
  #   }
  # }

  original_count <- ncol(data_table)
  if (original_count > 1) {
    data_table <- data_table[, !duplicated(colnames(data_table))]
    final_count <- ncol(data_table)
    if(original_count != final_count) {
      print(paste0("Removed ", original_count - final_count, " duplicated columns"))
    }
  }

  return(data_table)
}


#' @title Find the delimiter of a file
#'
#' @description
#' Find the delimiter of a file by reading the first 10 lines.
#'
#' @param file_path character(1) path of the file.
#'
#' @return character(1), the delimiter found.
#'
#' @author Damien Olivier
#' @author Rico Derks
#'
find_delim = function(file_path = NULL) {
  if(is.null(file_path)) {
    stop("No file path specified!")
  } else {
    if(!file.exists(file_path)) {
      stop("File path does not exist!")
    }
  }

  probe <- paste(readLines(con = file_path,
                           n = 10),
                 collapse = "")
  sep <- c(
    "\t" = lengths(regmatches(x = probe,
                              m = gregexpr(pattern = "\t",
                                           text = probe))),
    "," = lengths(regmatches(x = probe,
                             m = gregexpr(pattern = ",",
                                          text = probe))),
    ";" = lengths(regmatches(x = probe,
                             m = gregexpr(pattern = ";",
                                          text = probe)))
  )

  return(names(which.max(sep)))
}


#' @title Distribution plot
#'
#' @description
#' Distribution plot of the sample types
#'
#' @param data data.frame with all the data.
#' @param title character(1), title of the plot.
#'
#' @return ggplot2 object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes .data geom_bar geom_text labs theme_minimal
#'
#' @noRd
distribution_plot <- function(data = NULL,
                              title = NULL) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$value,
                                 y = .data$count)) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "lightblue") +
    ggplot2::geom_text(ggplot2::aes(label = .data$count),
                       vjust = -0.5,
                       hjust = 0.5,
                       size = 4) +
    ggplot2::labs(x = NULL,
                  y = NULL,
                  title = title) +
    ggplot2::theme_minimal()

  return(p)
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
#' @export
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
  pool1_batch <- as.vector(tapply(pool_meta, list(pool_meta[, batch_col]), function(x) {
    x[x[, order_col] == min(x[, order_col]), sampleid_meta_col]
  }))

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
  ref_data_all <- data_long[data_long[, sampleid_raw_col] == pool1, ]
  ref_data_all <- ref_data_all[, c("featureNames", "value")]
  colnames(ref_data_all)[2] <- "refValue"

  # reference data over per batch
  ref_data_batch <- data_long[data_long[, sampleid_raw_col, drop = TRUE] %in% pool1_batch, ]
  ref_data_batch <- ref_data_batch[, c("featureNames", "value", batch_col)]
  colnames(ref_data_batch)[2] <- "refValueBatch"

  # merge all
  data_long <- base::merge(
    x = data_long,
    y = ref_data_all,
    by = "featureNames",
    all.x = TRUE
  )

  data_long <- base::merge(
    x = data_long,
    y = ref_data_batch,
    by = c("featureNames" = "featureNames",
           batch_col = batch_col),
    all.x = TRUE
  )

  data_long$log2fc <- log2(data_long$value / data_long$refValue)
  data_long$log2fc_batch <- log2(data_long$value / data_long$refValueBatch)

  # make sure order of pooled samples is correct
  pool_order <- pool_meta[order(pool_meta[, order_col]), sampleid_meta_col]
  data_long[, sampleid_raw_col] <- factor(x = data_long[, sampleid_raw_col],
                                          levels = pool_order,
                                          labels = pool_order)

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
#' @export
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
#' @export
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

  names(m1@R2) <- paste0("PC", 1:4)

  res <- list(
    "model" = m1,
    "summary_of_fit" = m1_sumfit,
    "scores" = m1_scores,
    "loadings" = m1_loadings
  )

  return(res)
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
#' @export
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


#' @title Prepare data for a relative log expression plot
#'
#' @description
#' Prepare data for a relative log expression plot.
#'
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param sampleid_meta_col character(1), name of the sample id column in the meta data.
#' @param order_col character(1), name of the acquisition order column in the meta data.
#' @param batch_col character(1), name of the batch column.
#' @param id_samples character() vector with the names of the sample samples id's.
#'
#' @return data.frame
#'
#' @author Rico Derks
#'
#' @importFrom tidyr pivot_longer matches
#' @importFrom stats median
#'
#' @export
prepare_rle_data <- function(data = NULL,
                             meta_data = NULL,
                             sampleid_raw_col = NULL,
                             sampleid_meta_col = NULL,
                             order_col = NULL,
                             batch_col = NULL,
                             id_samples = NULL) {
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

  data <- data[data[, sampleid_raw_col] %in% id_samples, ]

  # calculate the deviation to the median
  feature_medians <- apply(data[, feature_names], 2, stats::median, na.rm = TRUE)
  data[, feature_names] <- t(t(data[, feature_names]) - feature_medians)

  data_long <- data |>
    tidyr::pivot_longer(
      cols = !tidyr::matches(other_columns),
      names_to = "featureNames",
      values_to = "value"
    )

  # trying to sort the samples in more or less a measurement order when the order_col
  # is empty in the meta data
  if(all(is.na(data[, order_col]))) {
    sample_order <- data[order(data[, batch_col], data[, sampleid_raw_col]), sampleid_raw_col]
  } else {
    sample_order <- data[order(data[, order_col]), sampleid_raw_col]
  }

  data_long[, sampleid_raw_col] <- factor(x = data_long[, sampleid_raw_col, drop = TRUE],
                                          levels = sample_order,
                                          labels = sample_order)

  return(data_long)
}


#' @title Prepare data for a missing values plot
#'
#' @description
#' Prepare data for a missing values plot.
#'
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param sampleid_meta_col character(1), name of the sample id column in the meta data.
#' @param sample_ids character() vector with the names of the samples id's which should be selected for the plot.
#'
#' @return data.frame
#'
#' @author Rico Derks
#'
#' @importFrom tidyr pivot_longer matches
#'
#' @export
prepare_missing_data <- function(data = NULL,
                                 meta_data = NULL,
                                 sampleid_raw_col = NULL,
                                 sampleid_meta_col = NULL,
                                 sample_ids = NULL) {

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
  data <- data[data[, sampleid_raw_col] %in% sample_ids, ]

  data_long <- data |>
    tidyr::pivot_longer(
      cols = !tidyr::contains(other_columns),
      names_to = "featureNames",
      values_to = "value"
    )

  data_long$missing <- is.na(data_long$value)
  data_long$total <- length(sample_ids)

  data_long <- as.data.frame(tapply(data_long, list(data_long$featureNames, data_long$missing, data_long$total), function(x) {
    nrow(x)
  }))
  data_long$featureNames <- rownames(data_long)
  colnames(data_long)[1:2] <- c("FALSE", "TRUE")

  data_long <- data_long |>
    tidyr::pivot_longer(
      cols = tidyr::contains(c("FALSE", "TRUE")),
      names_to = "missing",
      values_to = "num_missing"
    )
  data_long$total <- length(sample_ids)
  data_long$proportion <- data_long$num_missing / data_long$total

  return(data_long)
}
