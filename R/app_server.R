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
  # Your application server logic

  r6 <- shiny::reactiveValues()

  mod_data_server("data", r6 = r6)
}
