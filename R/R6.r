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
    )
  )
)
