#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r r object with all information
#'
#' @noRd
#'
#' @importFrom shiny NS tagList withProgress incProgress
#' @importFrom shinyWidgets progressBar updateProgressBar
#' @importFrom DT dataTableOutput
#' @importFrom bsicons bs_icon
#' @importFrom bslib navset_card_tab nav_panel card card_body page_sidebar sidebar
#' @importFrom shinyjs useShinyjs enable disabled
#'
mod_data_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = "Meta data",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              shiny::div(
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
                  label = bslib::tooltip(
                    trigger = list(
                      "Blanks",
                      bsicons::bs_icon(name = "info-circle")
                    ),
                    "Regular expression to recognize blank samples."
                  ),
                  value = "^blank",
                  width = "100%"
                ),
                shiny::textInput(
                  inputId = ns("metadata_qc_pattern"),
                  label = bslib::tooltip(
                    trigger = list(
                      "Pooled samples",
                      bsicons::bs_icon(name = "info-circle")
                    ),
                    "Regular expression to recognize pooled samples."
                  ),
                  value = "^pool",
                  width = "100%"
                ),
                shiny::textInput(
                  inputId = ns("metadata_sample_pattern"),
                  label = bslib::tooltip(
                    trigger = list(
                      "Samples",
                      bsicons::bs_icon(name = "info-circle")
                    ),
                    "Regular expression to recognize samples."
                  ),
                  value = "^sample",
                  width = "100%"
                ),
                style = "font-size:85%"
              )

            ),
            shiny::fileInput(
              inputId = ns("metadata_file"),
              label = "Data file:",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".txt", ".xlsx")
            ),
            bslib::card_body(
              shiny::div(
                DT::dataTableOutput(
                  outputId = ns("metadata_preview_table")
                ),
                style = "font-size:75%;"
              ),
              height = "40%"
            ),
            bslib::card_body(
              height = "60%",
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
              title = "Raw data",
              open = FALSE,
              shiny::numericInput(
                inputId = ns("raw_missing"),
                label = bslib::tooltip(
                  trigger = list(
                    "Max. missing values [%]",
                    bsicons::bs_icon(name = "info-circle")
                  ),
                  "Maximum relative amount [%] of missing values per feature.
                  If a feature has more missing values it will be removed"
                ),
                value = 50,
                min = 0,
                max = 100,
                step = 1
              )
            ),
            bslib::card_body(
              height = "20%",
              bslib::layout_column_wrap(
                width = 1 / 2,
                shinyjs::disabled(
                  shiny::fileInput(
                    inputId = ns("rawdata_file"),
                    label = "Data file:",
                    multiple = FALSE,
                    accept = c(".csv", ".tsv", ".txt", ".xlsx")
                  )
                ),
                shiny::selectInput(
                  inputId = ns("raw_select_table"),
                  label = "Select table",
                  choices = c("Raw table" = "raw_data",
                              "Filtered table" = "clean_data"),
                  selected = "clean_data"
                )
              )
            ),
            bslib::card_body(
              height = "65%",
              shiny::div(
                DT::dataTableOutput(
                  outputId = ns("rawdata_preview_table")
                ),
                style = "font-size:75%;"
              )
            ),
            bslib::card_body(
              bslib::layout_column_wrap(
                width = 1 / 2,
                height = "15%",
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
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$rawdata_file, {
      req(input$rawdata_file)

      r$data_file <- input$rawdata_file$name
      file_path <- input$rawdata_file$datapath
      data_table <- read_data(file_path = file_path)

      r$tables$raw_data <- data_table
      print("Raw data read")

      r$indices$raw_id_col <- colnames(r$tables$raw_data)[1]

      r$bc_applied <- "none"
      r$tables$bc_data <- NULL
    })


    shiny::observeEvent(input$metadata_file, {
      req(input$metadata_file)

      r$meta_file <- input$metadata_file$name
      file_path <- input$metadata_file$datapath
      data_table <- read_data(file_path = file_path)

      r$tables$meta_data <- data_table
      print("Meta data read")

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

      r$bc_applied <- "none"
      r$tables$bc_data <- NULL

      shinyjs::enable(id = "rawdata_file")
    })


    shiny::observeEvent(
      c(input$metadata_select_sampleid,
        input$metadata_select_sampletype,
        input$metadata_select_batch,
        input$metadata_blank_pattern,
        input$metadata_qc_pattern,
        input$metadata_sample_pattern,
        input$metadata_file,
        input$rawdata_file,
        input$raw_missing), {
          shiny::req(r$tables$meta_data,
                     r$tables$raw_data,
                     input$metadata_select_sampleid,
                     input$metadata_select_sampletype,
                     input$metadata_select_batch,
                     input$metadata_blank_pattern,
                     input$metadata_qc_pattern,
                     input$metadata_sample_pattern)

          r$bc_applied <- "none"
          r$tables$bc_data <- NULL

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

          raw_missing <- input$raw_missing / 100
          keep_features <- apply(r$tables$raw_data[r$tables$raw_data[, r$indices$raw_id_col] %in% c(r$indices$id_qcpool, r$indices$id_samples), ], 2, function(x) {
            mean(is.na(x)) <= raw_missing
          })

          r$tables$clean_data <-
            r$tables$raw_data[r$tables$raw_data[, r$indices$raw_id_col] %in% c(r$indices$id_blanks, r$indices$id_qcpool, r$indices$id_samples), keep_features]
          r$settings_data$raw_data$missing <- input$raw_missing


          shinyWidgets::updateProgressBar(
            session = session,
            id = "col_count_bar",
            title = "Column count",
            value = ncol(r$tables$clean_data) - 1,
            total = ncol(r$tables$raw_data) - 1
          )

          shinyWidgets::updateProgressBar(
            session = session,
            id = "row_count_bar",
            title = "Row count",
            value = nrow(r$tables$clean_data),
            total = nrow(r$tables$raw_data)
          )

          shiny::withProgress(
            message = "Calculating .....",
            value = 0,
            {
              print("Calculating...")
              print("  * Missing values plot")
              r$data$missing <- prepare_missing_data(data = r$tables$clean_data,
                                                     meta_data = r$tables$meta_data,
                                                     sampleid_raw_col = r$indices$raw_id_col,
                                                     sampleid_meta_col = r$indices$meta_id_col,
                                                     sample_ids = c(r$indices$id_qcpool, r$indices$id_samples))
              shiny::incProgress(1/6)

              print("  * trend plot")
              r$data$trend <- prepare_trend_data(data = r$tables$clean_data,
                                                 meta_data = r$tables$meta_data,
                                                 sampleid_raw_col = r$indices$raw_id_col,
                                                 sampleid_meta_col = r$indices$meta_id_col,
                                                 order_col = r$indices$meta_acqorder_col,
                                                 batch_col = r$indices$meta_batch_col,
                                                 id_qcpool = r$indices$id_qcpool)
              shiny::incProgress(1/6)

              print("  * histogram")
              r$data$histogram <- prepare_hist_data(data = r$tables$clean_data,
                                                    meta_data = r$tables$meta_data,
                                                    sampleid_raw_col = r$indices$raw_id_col,
                                                    sampleid_meta_col = r$indices$meta_id_col,
                                                    batch_col = r$indices$meta_batch_col,
                                                    id_qcpool = r$indices$id_qcpool)
              shiny::incProgress(1/6)

              print("  * heatmap")
              r$data$heatmap <- prepare_heatmap_data(data = r$tables$clean_data,
                                                     meta_data = r$tables$meta_data,
                                                     sampleid_raw_col = r$indices$raw_id_col,
                                                     sampleid_meta_col = r$indices$meta_id_col,
                                                     sampletype_col = r$indices$meta_type_col,
                                                     batch_col = r$indices$meta_batch_col,
                                                     id_qcpool = r$indices$id_qcpool,
                                                     id_samples = r$indices$id_samples)
              shiny::incProgress(1/6)

              print("  * PCA")
              r$data$pca <- prepare_pca_data(data = r$tables$clean_data,
                                             meta_data = r$tables$meta_data,
                                             sampleid_raw_col = r$indices$raw_id_col,
                                             sampleid_meta_col = r$indices$meta_id_col,
                                             id_samples = r$indices$id_samples,
                                             id_qcpool = r$indices$id_qcpool)
              shiny::incProgress(1/6)

              print("  * RLE")
              r$data$rle <- prepare_rle_data(data = r$tables$clean_data,
                                             meta_data = r$tables$meta_data,
                                             sampleid_raw_col = r$indices$raw_id_col,
                                             sampleid_meta_col = r$indices$meta_id_col,
                                             order_col = r$indices$meta_acqorder_col,
                                             batch_col = r$indices$meta_batch_col,
                                             id_samples = r$indices$id_samples)
              shiny::incProgress(1/6)

              print("...done!")
            }
          )

        })


    output$rawdata_preview_table = DT::renderDataTable({
      shiny::req(r$tables$raw_data,
                 input$raw_select_table)

      data_table <- r$tables[[input$raw_select_table]]

      DT::datatable(data = data_table,
                    rownames = FALSE,
                    options = list(dom = "t",
                                   pageLength = -1))
    })


    output$metadata_preview_table = DT::renderDataTable({
      shiny::req(r$tables$meta_data)

      data_table <- r$tables$meta_data

      DT::datatable(data = data_table,
                    rownames = FALSE,
                    options = list(dom = "t",
                                   pageLength = -1))
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
                            ncol = 2)
    })
  })
}
