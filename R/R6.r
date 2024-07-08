#' @importFrom R6 R6Class
#'
batchCorrection = R6::R6Class(
  classname = "batchCorrection",
  public = list(
    initialize = function(name) {
      self$name <- name
    },

    name = NA,
    meta_file = NA,
    data_file = NA,

    #--------------------------------------------------------------- tables ----
    tables = list(
      raw_data = NULL,
      meta_data = NULL
    )
  )
)
