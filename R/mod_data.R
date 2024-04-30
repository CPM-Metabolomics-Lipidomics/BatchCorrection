#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT dataTableOutput
#'
mod_data_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::p("Here the data page will be shown."),
      shiny::fileInput(
        inputId = ns("data_file"),
        label = "Data file:",
        multiple = FALSE,
        accept = c(".csv", ".tsv", ".txt", ".xlsx")),
      DT::dataTableOutput(ns("data_preview_table"))
    )
  )
}

#' data Server Functions
#'
#' @importFrom DT datatable renderDataTable
#'
#' @noRd
#'
mod_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_table <- shiny::reactive({
      req(input$data_file)

      file_path <- input$data_file$datapath
      data_table <- read_data(file_path = file_path)

      return(data_table)
    })

    output$data_preview_table = DT::renderDataTable({
      shiny::req(data_table)

      data_table <- data_table()

      DT::datatable(data = data_table,
                    options = list(paging = TRUE))
    })
  })
}

## To be copied in the UI
# mod_data_ui("data_1")

## To be copied in the server
# mod_data_server("data_1")
