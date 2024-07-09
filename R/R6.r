#' @importFrom R6 R6Class
#'
batchCorrection = R6::R6Class(
  classname = "batchCorrection",
  public = list(
    initialize = function(name) {
      self$name <- name
    },
    #-------------------------------------------------------- general stuff ----
    name = NA,
    meta_file = NA,
    data_file = NA,

    #-------------------------------------------------------------- indices ----
    indices = list(
      meta_id_col = NULL,
      meta_type_col = NULL,
      meta_acqorder_col = NULL,
      meta_batch_col = NULL,

      raw_id_col = NULL,

      id_blanks = NULL,
      id_qcpool = NULL,
      id_samples = NULL
    ),

    #--------------------------------------------------------------- tables ----
    tables = list(
      raw_data = NULL,
      meta_data = NULL
    ),

    #---------------------------------------------------------------- plots ----
    plots = list(
      trend_plot = NULL
    ),

    #------------------------------------------------------- util functions ----
    prepare_trend_data = function(raw_data = self$tables$raw_data,
                                  raw_id_col = self$indices$raw_id_col,
                                  meta_data = self$tables$meta_data,
                                  meta_id_col = self$indices$meta_id_col,
                                  id_qcpool = self$indices$id_qcpool) {
      print("Prepare data for trend plot")

      data_wide <- raw_data[raw_data[, raw_id_col] %in% id_qcpool, ]
      print(data_wide)

      return(data_wide)
    }


    #------------------------------------------------------- plot functions ----
    # trend_plot = function() {
    #   print("Make dummy plot")
    #   pl <- plotly::plot_ly(x = 1:10,
    #                   y = 1:10,
    #                   type = "scatter",
    #                   mode = "markers")
    #
    #   return(pl)
    # }

  )
)
