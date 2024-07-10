#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r r object with all information
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets progressBar updateProgressBar
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
                inputId = ns("metadata_select_sampleid"),
                label = "Sample ID",
                choices = NULL
              ),
              shiny::selectInput(
                inputId = ns("metadata_select_sampletype"),
                label = "Sample type",
                choices = NULL
              ),
              shiny::selectInput(
                inputId = ns("metadata_select_acqorder"),
                label = "Acquisition order",
                choices = NULL
              ),
              shiny::selectInput(
                inputId = ns("metadata_select_batch"),
                label = "Batch",
                choices = NULL
              ),
              shiny::h4("Text patterns"),
              shiny::textInput(
                inputId = ns("metadata_blank_pattern"),
                label = "Blank",
                value = "^blank",
                width = "100%"
              ),
              shiny::textInput(
                inputId = ns("metadata_qc_pattern"),
                label = "QCpool",
                value = "^pool",
                width = "100%"
              ),
              shiny::textInput(
                inputId = ns("metadata_sample_pattern"),
                label = "Sample",
                value = "^sample",
                width = "100%"
              )
            ),
            shiny::fileInput(
              inputId = ns("metadata_file"),
              label = "Data file:",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".txt", ".xlsx")
            ),
            bslib::layout_column_wrap(
              width = 1 / 2,
              shiny::div(
                DT::dataTableOutput(
                  outputId = ns("metadata_preview_table")
                ),
                style = "font-size:75%;"
              ),
              shiny::plotOutput(
                outputId = ns("metadata_sampletype_plot")
              )
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
            bslib::card_body(
              bslib::layout_column_wrap(
                width = 1 / 3,
                shiny::fileInput(
                  inputId = ns("rawdata_file"),
                  label = "Data file:",
                  multiple = FALSE,
                  accept = c(".csv", ".tsv", ".txt", ".xlsx")
                ),
                shinyWidgets::progressBar(
                  id = ns("row_count_bar"),
                  title = "Row count",
                  value = 0,
                  total = 100,
                  unit_mark = "%"
                ),
                shinyWidgets::progressBar(
                  id = ns("col_count_bar"),
                  title = "Column count",
                  value = 0,
                  total = 100,
                  unit_mark = "%"
                )
              )
            ),
            bslib::card_body(
              shiny::div(
                DT::dataTableOutput(
                  outputId = ns("rawdata_preview_table")
                ),
                style = "font-size:75%;"
              )
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
#' @importFrom patchwork wrap_plots
#'
#' @noRd
#'
mod_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$rawdata_file, {
      req(input$rawdata_file)

      file_path <- input$rawdata_file$datapath
      data_table <- read_data(file_path = file_path)

      r$tables$raw_data <- data_table
      print("Raw data read into r")

      r$indices$raw_id_col <- colnames(r$tables$raw_data)[1]

      shinyWidgets::updateProgressBar(
        session = session,
        id = "col_count_bar",
        title = "Column count",
        value = ncol(data_table),
        total = ncol(r$tables$raw_data)
      )

      shinyWidgets::updateProgressBar(
        session = session,
        id = "row_count_bar",
        title = "Row count",
        value = nrow(data_table),
        total = nrow(r$tables$raw_data)
      )
    })


    shiny::observeEvent(input$metadata_file, {
      req(input$metadata_file)

      file_path <- input$metadata_file$datapath
      data_table <- read_data(file_path = file_path)

      r$tables$meta_data <- data_table
      print("Meta data read into r")

      # update column names
      column_names <- colnames(r$tables$meta_data)
      shiny::updateSelectInput(
        inputId = "metadata_select_sampleid",
        choices = sort(column_names),
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
        inputId = "metadata_select_sampletype",
        choices = sort(column_names),
        selected = ifelse(any(grepl(x = column_names,
                                    pattern = ".*type.*",
                                    ignore.case = TRUE)),
                          grep(x = column_names,
                               pattern = ".*type.*",
                               ignore.case = TRUE,
                               value = TRUE)[1],
                          column_names[1])
      )
      shiny::updateSelectInput(
        inputId = "metadata_select_acqorder",
        choices = sort(column_names),
        selected = ifelse(any(grepl(x = column_names,
                                    pattern = ".*order.*",
                                    ignore.case = TRUE)),
                          grep(x = column_names,
                               pattern = ".*order.*",
                               ignore.case = TRUE,
                               value = TRUE)[1],
                          column_names[1])
      )
      shiny::updateSelectInput(
        inputId = "metadata_select_batch",
        choices = sort(column_names),
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


    shiny::observeEvent(
      c(input$metadata_select_sampleid,
        input$metadata_select_sampletype,
        input$metadata_select_batch,
        input$metadata_blank_pattern,
        input$metadata_qc_pattern,
        input$metadata_sample_pattern), {
          shiny::req(r$tables$meta_data,
                     input$metadata_select_sampleid,
                     input$metadata_select_sampletype,
                     input$metadata_select_batch,
                     input$metadata_blank_pattern,
                     input$metadata_qc_pattern,
                     input$metadata_sample_pattern)

          r$indices$meta_id_col <- input$metadata_select_sampleid
          r$indices$meta_type_col <- input$metadata_select_sampletype
          r$indices$meta_acqorder_col <- input$metadata_select_acqorder
          r$indices$meta_batch_col <- input$metadata_select_batch

          data_table <- r$tables$meta_data

          r$indices$id_blanks <- data_table[grepl(x = data_table[, input$metadata_select_sampletype],
                                                  pattern = input$metadata_blank_pattern[1],
                                                  ignore.case = TRUE), input$metadata_select_sampleid]
          r$indices$id_qcpool <- data_table[grepl(x = data_table[, input$metadata_select_sampletype],
                                                  pattern = input$metadata_qc_pattern[1],
                                                  ignore.case = TRUE), input$metadata_select_sampleid]
          r$indices$id_samples <- data_table[grepl(x = data_table[, input$metadata_select_sampletype],
                                                   pattern = input$metadata_sample_pattern[1],
                                                   ignore.case = TRUE), input$metadata_select_sampleid]
        })


    output$rawdata_preview_table = DT::renderDataTable({
      shiny::req(r$tables$raw_data)

      data_table <- r$tables$raw_data

      DT::datatable(data = data_table,
                    options = list(paging = TRUE))
    })


    output$metadata_preview_table = DT::renderDataTable({
      shiny::req(r$tables$meta_data)

      data_table <- r$tables$meta_data

      DT::datatable(data = data_table,
                    options = list(paging = TRUE))
    })


    output$metadata_sampletype_plot <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 input$metadata_select_sampletype,
                 input$metadata_blank_pattern,
                 input$metadata_qc_pattern,
                 input$metadata_sample_pattern)

      # original data
      data_table <- r$tables$meta_data
      freq_table1 <- data.frame(table(base::factor(data_table[, input$metadata_select_sampletype])))
      names(freq_table1) <- c("value", "count")

      # sample selection based on patterns
      blank_table <- r$tables$meta_data[grepl(x = r$tables$meta_data[, input$metadata_select_sampletype],
                                              pattern = input$metadata_blank_pattern,
                                              ignore.case = TRUE), ]
      qcpool_table <- r$tables$meta_data[grepl(x = r$tables$meta_data[, input$metadata_select_sampletype],
                                               pattern = input$metadata_qc_pattern,
                                               ignore.case = TRUE), ]
      sample_table <- r$tables$meta_data[grepl(x = r$tables$meta_data[, input$metadata_select_sampletype],
                                               pattern = input$metadata_sample_pattern,
                                               ignore.case = TRUE), ]

      data_table <- rbind(
        blank_table,
        qcpool_table,
        sample_table
      )

      freq_table2 <- data.frame(table(base::factor(data_table[, input$metadata_select_sampletype])))
      names(freq_table2) <- c("value", "count")

      p1 <- distribution_plot(data = freq_table1,
                              title = "Type distribution, original")
      p2 <- distribution_plot(data = freq_table2,
                              title = "Type distribution, patterns applied ")

      patchwork::wrap_plots(p1, p2,
                            ncol = 1)
    })
  })
}
