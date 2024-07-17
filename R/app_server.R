#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#'
#' @noRd
#'
app_server <- function(input, output, session) {

  # for communication between modules
  r <- shiny::reactiveValues(
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
      meta_data = NULL,
      clean_data = NULL,
      bc_data = NULL
    ),

    #----------------------------------------------------------------- data ----
    # before correction
    data = list(
      trend = NULL,
      histogram = NULL,
      pca = NULL,
      heatmap = NULL,
      rle = NULL,
      missing = NULL
    ),
    # after correction
    data_bc = list(
      trend = NULL,
      histogram = NULL,
      pca = NULL,
      heatmap = NULL,
      rle = NULL
    )
  ) # end reactive values

  mod_data_server(id = "data",
                  r = r)

  mod_visualization_server(id = "viz",
                           r = r)

  mod_batch_correction_server(id = "bc",
                              r = r)

  mod_help_server(id = "help")

  mod_about_server(id = "about")
}
