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
    shiny::tabsetPanel(
      id = "tabset_data",
      shiny::tabPanel(
        title = "Meta data",
        shiny::fileInput(
          inputId = ns("metadata_file"),
          label = "Data file:",
          multiple = FALSE,
          accept = c(".csv", ".tsv", ".txt", ".xlsx")),
        DT::dataTableOutput(ns("metadata_preview_table"))
      ),
      shiny::tabPanel(
        title = "Data",
        shiny::fileInput(
          inputId = ns("rawdata_file"),
          label = "Data file:",
          multiple = FALSE,
          accept = c(".csv", ".tsv", ".txt", ".xlsx")),
        DT::dataTableOutput(ns("rawdata_preview_table"))
      )
    )
  ) # end tagList
}

#' data Server Functions
#'
#' @importFrom DT datatable renderDataTable
#'
#' @noRd
#'
mod_data_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observe({
      shiny::req(session,
          r6)

      r6 <- batchCorrection$new(name = "data")
      print("R6 object created")
    })

    shiny::observeEvent(input$rawdata_file, {
      req(input$rawdata_file)

      file_path <- input$rawdata_file$datapath
      data_table <- read_data(file_path = file_path)

      r6$tables$raw_data <- data_table
      print("Raw data read into R6")
    })

    shiny::observeEvent(input$metadata_file, {
      req(input$metadata_file)

      file_path <- input$metadata_file$datapath
      data_table <- read_data(file_path = file_path)

      r6$tables$meta_data <- data_table
      print("Meta data read into R6")
    })

    output$rawdata_preview_table = DT::renderDataTable({
      shiny::req(r6$tables$raw_data)

      data_table <- r6$tables$raw_data

      DT::datatable(data = data_table,
                    options = list(paging = TRUE))
    })

    output$metadata_preview_table = DT::renderDataTable({
      shiny::req(r6$tables$meta_data)

      data_table <- r6$tables$meta_data

      DT::datatable(data = data_table,
                    options = list(paging = TRUE))
    })
  })
}

## To be copied in the UI
# mod_data_ui("data_1")

## To be copied in the server
# mod_data_server("data_1")
