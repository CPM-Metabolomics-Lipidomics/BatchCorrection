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

  shiny::observe({
    shiny::req(session,
               r6)

    r6 <- batchCorrection$new(name = "data")
    print("R6 object created")
  })

  mod_data_server(id = "data",
                  r6 = r6)

  mod_visualization_server(id = "viz",
                           r6 = r6)
}
