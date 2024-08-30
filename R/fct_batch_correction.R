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
#' @param id_blanks character() vector with the names of the blank id's.
#' @param batch_col character(1), name of the batch column.
#'
#' @return data.frame with median batch corrected data.
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
                      id_blanks = NULL,
                      batch_col = NULL) {
  feature_names <- colnames(data)[-1]

  data <- merge(
    x = data,
    y = meta_data,
    by.x = sampleid_raw_col,
    by.y = sampleid_meta_col,
    all.x = TRUE
  )

  if(is.null(id_blanks)) {
    samples_selected <- c(id_qcpool, id_samples)
  } else {
    samples_selected <- c(id_blanks, id_qcpool, id_samples)
  }

  # sort the columns
  other_columns <- colnames(data)[!(colnames(data) %in% feature_names)]
  data <- data[data[, sampleid_raw_col] %in% samples_selected, c(other_columns, feature_names)]

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


#' @title Perform QC-RLSC for batch correction of chromatographic signal
#'
#' @description Perform QC-RLSC for batch correction of chromatographic signal.
#'
#' @param tab table N*K (row * column) with N samples and K variables.
#' @param colv vector N of numbers: 1 for QC samples and 2 for other samples.
#' @param or vector of measuring order (see details).
#' @param span the parameter alpha hich controls the degree of smoothing.
#' @param verbose print which variable has been corrected to monitor the process (default = FALSE).
#'
#' @return corrected table N*K
#'
#' @details Make sure that everything is sorted in measurement order!!!
#'
#' @importFrom stats loess approx
#'
#' @author E. Nevedomskaya
#' @author Rico Derks
#'
#' @references Dunn et al. Nature Protocols 6, 1060-1083 (2011)
#'
#' @noRd
qc_rlsc <- function(tab, colv, or, span = 0.75, verbose = FALSE) {
  # create table of the same size as initial
  tab_corr <- tab

  # For each variable (columns) in the initial table
  for (i in 1:ncol(tab)) {
    # fit loess curve to the QCs
    ll <- stats::loess(tab[which(colv == 1), i] ~ or[which(colv == 1)], span = span)

    # approximate the curve for all the samples
    aa <- stats::approx(x = or[which(colv == 1)],
                        y = ll$fitted,
                        xout = or)

    # correct the variable according to the curve for all the samples
    tab_corr[, i] <- tab[, i] / aa$y

    # print which variable has been corrected in order to monitor the progress
    if(verbose == TRUE) {
      print(i)
    }

  }

  return(tab_corr)
}


#' @title Perform LOESS batch correction
#'
#' @description
#' Perform LOESS batch correction.
#'
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param sampleid_meta_col character(1), name of the sample id column in the meta data.
#' @param id_samples character() vector with the names of the sample samples id's.
#' @param id_qcpool character() vector with the names of the pooled sample id's.
#' @param id_blanks character() vector with the names of the blank id's.
#' @param batch_col character(1), name of the batch column.
#' @param order_col character(1), name of the acquisition order column.
#' @param span numeric(1), the parameter alpha which controls the degree of smoothing.
#' @param method character(1), perform de batch correction per batch or over all batches at once.
#'
#' @return data.frame with LOESS batch corrected data.
#'
#' @author Rico Derks
#'
#' @noRd
loess_bc <- function(data = NULL,
                     meta_data = NULL,
                     sampleid_raw_col = NULL,
                     sampleid_meta_col = NULL,
                     id_samples = NULL,
                     id_qcpool = NULL,
                     id_blanks = NULL,
                     batch_col = NULL,
                     order_col = NULL,
                     span = 0.75,
                     method = c("batch", "over_all")) {

  method <- match.arg(arg = method,
                      choices = c("batch", "over_all"))

  feature_names <- colnames(data)[-1]

  data <- merge(
    x = data,
    y = meta_data,
    by.x = sampleid_raw_col,
    by.y = sampleid_meta_col,
    all.x = TRUE
  )

  if(is.null(id_blanks)) {
    samples_selected <- c(id_qcpool, id_samples)
  } else {
    samples_selected <- c(id_blanks, id_qcpool, id_samples)
  }

  # sort the columns
  other_columns <- colnames(data)[!(colnames(data) %in% feature_names)]
  data <- data[data[, sampleid_raw_col] %in% samples_selected, c(other_columns, feature_names)]

  cor_data <- data
  cor_data[, batch_col] <- factor(cor_data[, batch_col])
  batches <- as.character(unique(cor_data[, batch_col]))

  cor_data <- cor_data[order(cor_data[, order_col]), ]

  if(all(!is.na(cor_data[cor_data[, sampleid_raw_col] %in% id_qcpool, feature_names]))){
    switch(
      method,
      "batch" = {
        print("Per batch")
        for(b in 1:length(batches)) {
          cor_data[cor_data[, batch_col] == batches[b], feature_names] <-
            qc_rlsc(
              tab = cor_data[cor_data[, batch_col] == batches[b], feature_names],
              colv = ifelse(cor_data[cor_data[, batch_col] == batches[b], sampleid_raw_col] %in% id_qcpool,
                            1,
                            2),
              or = cor_data[cor_data[, batch_col] == batches[b], order_col],
              span = span)
        }
      },
      "over_all" = {
        print("Over all batches at once")
        cor_data[, feature_names] <-
          qc_rlsc(
            tab = cor_data[, feature_names],
            colv = ifelse(cor_data[, sampleid_raw_col] %in% id_qcpool,
                          1,
                          2),
            or = cor_data[, order_col],
            span = span)
      }
    )

    return(
      list(
        bc_data = cor_data[, c(sampleid_raw_col, feature_names)],
        status = "ok",
        message = ""
      )
    )
  } else {
    return(
      list(
        bc_data = NULL,
        status = "error",
        message = "Error: there are missing values in the pooled samples. Please select a different batch correction method or remove the missing values!"
      )
    )
  }
}


#' @title Perform ComBat batch correction
#'
#' @description
#' Perform ComBat batch correction from the SVA package.
#'
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param sampleid_raw_col character(1), name of the sample id column in the raw data.
#' @param sampleid_meta_col character(1), name of the sample id column in the meta data.
#' @param id_samples character() vector with the names of the sample samples id's.
#' @param id_qcpool character() vector with the names of the pooled sample id's.
#' @param id_blanks character() vector with the names of the blank id's.
#' @param batch_col character(1), name of the batch column.
#'
#' @return data.frame with combat batch corrected data.
#'
#' @author Rico Derks
#'
#' @importFrom sva ComBat
#' @importFrom stats model.matrix
#'
#' @noRd
combat_bc <- function(data = NULL,
                      meta_data = NULL,
                      sampleid_raw_col = NULL,
                      sampleid_meta_col = NULL,
                      id_samples = NULL,
                      id_qcpool = NULL,
                      id_blanks = NULL,
                      batch_col = NULL) {
  feature_names <- colnames(data)[-1]

  data <- merge(
    x = data,
    y = meta_data,
    by.x = sampleid_raw_col,
    by.y = sampleid_meta_col,
    all.x = TRUE
  )

  if(is.null(id_blanks)) {
    samples_selected <- c(id_qcpool, id_samples)
  } else {
    samples_selected <- c(id_blanks, id_qcpool, id_samples)
  }

  # sort the columns
  other_columns <- colnames(data)[!(colnames(data) %in% feature_names)]
  data <- data[data[, sampleid_raw_col] %in% samples_selected, c(other_columns, feature_names)]

  data[, batch_col] <- factor(data[, batch_col])
  batches <- data[, batch_col]

  anno <- data[, other_columns]
  modcombat <- model.matrix(~1, data = anno)

  cor_data <- ComBat(dat = t(as.matrix(data[, feature_names])),
                     batch = batches,
                     mod = modcombat,
                     par.prior = TRUE,
                     prior.plots = FALSE)

  cor_data <- as.data.frame(t(cor_data))
  cor_data <- cbind(
    data[, sampleid_raw_col],
    cor_data
  )
  colnames(cor_data)[1] <- sampleid_raw_col

  return(cor_data)
}
