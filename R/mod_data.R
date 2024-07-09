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
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = "Meta data",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              shiny::h4("Column selection"),
              shiny::selectInput(
                inputId = ns("meta_select_sampleid"),
                label = "Sample ID",
                choices = c("sampleId")
              ),
              shiny::selectInput(
                inputId = ns("meta_select_sampletype"),
                label = "Sample type",
                choices = "sampleType"
              ),
              shiny::selectInput(
                inputId = ns("meta_select_batch"),
                label = "Batch",
                choices = "batch"
              ),
              shiny::h4("Text patterns"),
              shiny::textInput(
                inputId = ns("meta_blank_pattern"),
                label = "Blank",
                value = "blank",
                width = "100%"
              ),
              shiny::textInput(
                inputId = ns("meta_qc_pattern"),
                label = "QCpool",
                value = "qcpool",
                width = "100%"
              ),
              shiny::textInput(
                inputId = ns("meta_sample_pattern"),
                label = "Sample",
                value = "sample",
                width = "100%"
              )
            ),
            shiny::fileInput(
              inputId = ns("metadata_file"),
              label = "Data file:",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".txt", ".xlsx")
            ),
            DT::dataTableOutput(
              outputId = ns("metadata_preview_table")
            )
          )
        )
      ), # end navpanel meta data
      bslib::nav_panel(
        title = "Raw data",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              title = "sidebar",
              open = FALSE
            ),
            shiny::fileInput(
              inputId = ns("rawdata_file"),
              label = "Data file:",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".txt", ".xlsx")
            ),
            DT::dataTableOutput(
              outputId = ns("rawdata_preview_table")
            )
          )
        )
      ) # end navpanel raw data
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

      # update column names
      column_names <- sort(colnames(r6$tables$meta_data))
      shiny::updateSelectInput(
        inputId = "meta_select_sampleid",
        choices = column_names,
        selected = ifelse(any(grepl(x = column_names,
                                    pattern = ".*sampleid.*",
                                    ignore.case = TRUE)),
                          grep(x = column_names,
                               pattern = ".*sampleid.*",
                               ignore.case = TRUE,
                               value = TRUE)[1],
                          column_names[1])
      )
      shiny::updateSelectInput(
        inputId = "meta_select_sampletype",
        choices = column_names,
        selected = ifelse(any(grepl(x = column_names,
                                    pattern = ".*sampletype.*",
                                    ignore.case = TRUE)),
                          grep(x = column_names,
                               pattern = ".*sampletype.*",
                               ignore.case = TRUE,
                               value = TRUE)[1],
                          column_names[1])
      )
      shiny::updateSelectInput(
        inputId = "meta_select_batch",
        choices = column_names,
        selected = ifelse(any(grepl(x = column_names,
                                    pattern = ".*batch.*",
                                    ignore.case = TRUE)),
                          grep(x = column_names,
                               pattern = ".*batch.*",
                               ignore.case = TRUE,
                               value = TRUE)[1],
                          column_names[1])
      )

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
