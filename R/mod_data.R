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
      #---------------------------------------------------------- meta data ----
      shiny::tabPanel(
        title = "Meta data",
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(
              width = 8,
              shiny::fluidRow(
                shiny::inputPanel(
                  shiny::fileInput(
                    inputId = ns("metadata_file"),
                    label = "Data file:",
                    multiple = FALSE,
                    accept = c(".csv", ".tsv", ".txt", ".xlsx")
                  )
                )
              ),
              shiny::fluidRow(
                DT::dataTableOutput(
                  outputId = ns("metadata_preview_table")
                )
              )
            ),
            shiny::column(
              width = 4,
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  shiny::h3("Column selection")
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::selectInput(
                    inputId = ns("meta_select_sampleid"),
                    label = "Sample ID",
                    choices = c("sampleId")
                  ),
                  shiny::selectInput(
                    inputId = ns("meta_select_batch"),
                    label = "Batch",
                    choices = "batch"
                  )
                ),
                shiny::column(
                  width = 6,
                  shiny::selectInput(
                    inputId = ns("meta_select_sampletype"),
                    label = "Sample type",
                    choices = "sampleType"
                  )
                ),
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
                  shiny::h3("Text patterns")
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::textInput(
                    inputId = ns("meta_blank_pattern"),
                    label = "Blank",
                    value = "blank",
                    width = "100%"
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::textInput(
                    inputId = ns("meta_qc_pattern"),
                    label = "QCpool",
                    value = "qcpool",
                    width = "100%"
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::textInput(
                    inputId = ns("meta_sample_pattern"),
                    label = "Sample",
                    value = "sample",
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      ),
      #----------------------------------------------------------- raw data ----
      shiny::tabPanel(
        title = "Data",
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(
              width = 8,
              shiny::fluidRow(
                shiny::fileInput(
                  inputId = ns("rawdata_file"),
                  label = "Data file:",
                  multiple = FALSE,
                  accept = c(".csv", ".tsv", ".txt", ".xlsx"))
              ),
              shiny::fluidRow(
                DT::dataTableOutput(ns("rawdata_preview_table"))
              )
            ),
            shiny::column(
              width = 4,
              shiny::p("Some settings for raw data")
            )
          )
        ) # end fluidPage
      ) # end tabPanel
    ) # end tabsetPanel
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
