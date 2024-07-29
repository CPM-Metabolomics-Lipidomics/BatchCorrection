#' visualization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r r object with all information
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @importFrom rmarkdown render
#' @import gt
mod_visualization_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = "Missing values",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              open = FALSE,
              shiny::selectInput(
                inputId = ns("viz_missing_view_select"),
                label = "Show",
                choices = c("Pooled samples / samples" = "all",
                            "Pooled samples" = "pooled"),
                selected = "all"
              )
            ),
            shiny::plotOutput(
              outputId = ns("viz_missing")
            )
          )
        )
      ),
      bslib::nav_panel(
        title = "Histogram",
        bslib::card(
          shiny::plotOutput(
            outputId = ns("viz_histogram")
          )
        )
      ),
      bslib::nav_panel(
        title = "Trend plot",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              open = FALSE,
              shiny::selectInput(
                inputId = ns("viz_trend_plot_view_select"),
                label = "Overview",
                choices = c("Overall" = "log2fc",
                            "Per batch" = "log2fc_batch")
              )
            ),
            shiny::plotOutput(
              outputId = ns("viz_trend_plot")
            )
          )
        )
      ),
      bslib::nav_panel(
        title = "Heatmap",
        bslib::card(
          shiny::plotOutput(
            outputId = ns("viz_heatmap")
          )
        )
      ),
      bslib::nav_panel(
        title = "PCA",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              open = FALSE,
              shiny::selectInput(
                inputId = ns("viz_pca_x"),
                label = "x-axis",
                choices = paste0("PC", 1:4),
                selected = "PC1"
              ),
              shiny::selectInput(
                inputId = ns("viz_pca_y"),
                label = "y-axis",
                choices = paste0("PC", 1:4),
                selected = "PC2"
              )
            ),
            shiny::plotOutput(
              outputId = ns("viz_pca_plot")
            )
          )
        )
      ),
      bslib::nav_panel(
        title = "Relative log expression",
        bslib::card(
          shiny::plotOutput(
            outputId = ns("viz_rle_plot")
          )
        )
      ),
      bslib::nav_spacer(),
      bslib::nav_item(
        shiny::uiOutput(
          outputId = ns("viz_download_ui")
        )
      )
    ) # end navset_card_tab
  )
}

#' visualization Server Functions
#'
#' @importFrom plotly renderPlotly
#'
#' @noRd
mod_visualization_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$viz_missing <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$clean_data,
                 r$data$trend,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_acqorder_col,
                 r$indices$meta_batch_col,
                 r$indices$id_qcpool,
                 input$viz_missing_view_select)
      print("Show missing values plot")
      title <- switch(input$viz_missing_view_select,
                      "all" = "Samples / pooled samples",
                      "pooled" = "Pooled samples")
      missing_plot(data = r$data$missing,
                   title = title)
    })


    output$viz_trend_plot <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$clean_data,
                 r$data$trend,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_acqorder_col,
                 r$indices$meta_batch_col,
                 r$indices$id_qcpool,
                 input$viz_trend_plot_view_select)

      if(!(input$viz_trend_plot_view_select %in% c("log2fc", "log2fc_batch"))) {
        yaxis <- "log2fc"
      } else {
        yaxis <- input$viz_trend_plot_view_select
      }

      print("Show trend plot")
      trend_plot(data = r$data$trend,
                 sampleid_raw_col = r$indices$raw_id_col,
                 batch_col = r$indices$meta_batch_col,
                 yaxis = yaxis)
    })


    output$viz_heatmap <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$clean_data,
                 r$data$heatmap,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_batch_col,
                 r$indices$meta_type_col,
                 r$indices$id_qcpool,
                 r$indices$id_samples)

      print("Show heatmap")

      ComplexHeatmap::Heatmap(
        matrix = r$data$heatmap$data,
        heatmap_legend_param = list(title = "Z-score"),
        right_annotation = ComplexHeatmap::rowAnnotation(
          df = r$data$heatmap$row_ann,
          col = r$data$heatmap$colors_ann
        ),
        cluster_rows = TRUE,
        cluster_columns = FALSE
      )
    })


    output$viz_pca_plot <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$clean_data,
                 r$data$pca,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_batch_col,
                 r$indices$meta_type_col,
                 r$indices$id_qcpool,
                 r$indices$id_samples,
                 input$viz_pca_x,
                 input$viz_pca_y)

      print("Show pca plots")

      p1 <- pca_scores_plot(data = r$data$pca,
                            sampletype_col = r$indices$meta_type_col,
                            batch_col = r$indices$meta_batch_col,
                            xaxis = input$viz_pca_x,
                            yaxis = input$viz_pca_y)
      p2 <- pca_loadings_plot(data = r$data$pca,
                              xaxis = input$viz_pca_x,
                              yaxis = input$viz_pca_y)

      patchwork::wrap_plots(p1, p2,
                            ncol = 2)

    })


    output$viz_histogram <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$clean_data,
                 r$data$histogram,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_batch_col,
                 r$indices$id_qcpool)

      print("Show histograms")

      histogram_plot(data = r$data$histogram)
    })


    output$viz_rle_plot <- shiny::renderPlot({
      shiny::req(r$data$rle,
                 r$indices$raw_id_col,
                 r$indices$meta_batch_col)

      print("Show RLE plot")

      rle_plot(data = r$data$rle,
               sampleid_raw_col = r$indices$raw_id_col,
               batch_col = r$indices$meta_batch_col)
    })


    observeEvent(input$viz_missing_view_select, {
      shiny::req(
        r$tables$clean_data,
        r$tables$meta_data,
        r$indices$raw_id_col,
        r$indices$meta_id_col,
        r$indices$id_qcpool,
        r$indices$id_samples
      )

      print("Calculating...")
      print("  * Missing values plot")
      if(input$viz_missing_view_select == "all") {
        r$data$missing <- prepare_missing_data(data = r$tables$clean_data,
                                               meta_data = r$tables$meta_data,
                                               sampleid_raw_col = r$indices$raw_id_col,
                                               sampleid_meta_col = r$indices$meta_id_col,
                                               sample_ids = c(r$indices$id_qcpool, r$indices$id_samples))
      } else {
        r$data$missing <- prepare_missing_data(data = r$tables$clean_data,
                                               meta_data = r$tables$meta_data,
                                               sampleid_raw_col = r$indices$raw_id_col,
                                               sampleid_meta_col = r$indices$meta_id_col,
                                               sample_ids = r$indices$id_qcpool)
      }
    })


    #------------------------------------------------------------- download ----
    output$viz_download_ui <- shiny::renderUI({
      shiny::req(r$data$missing,
                 r$data$histogram,
                 r$data$trend,
                 r$data$heatmap,
                 r$data$pca,
                 r$data$rle)

        shiny::tagList(
          bslib::popover(
            bsicons::bs_icon(name = "cloud-download-fill",
                             size = "2em"),
            shiny::downloadButton(
              outputId = ns("viz_download_report"),
              label = "Download overview report"
            )
          )
        )
    })


    output$viz_download_report <- shiny::downloadHandler(
      filename = function() {
        paste(Sys.Date(), "_data_overview.html", sep = "")
      },
      content = function(file) {
        temp_report <- file.path(tempdir(), "data_overview.Rmd")
        report_file <- system.file("reports", "data_overview.Rmd",
                                   package = "BatchCorrection")
        file.copy(from = report_file,
                  to = temp_report,
                  overwrite = TRUE)

        e <- new.env()

        e$params <- list(
          data_file = r$data_file,
          meta_file = r$meta_file,
          clean_data = r$tables$clean_data,
          meta_data = r$tables$meta_data,
          trend_data = r$data$trend,
          histogram_data = r$data$histogram,
          pca_data = r$data$pca,
          heatmap_data = r$data$heatmap,
          rle_data = r$data$rle,
          sampleid_raw_col = r$indices$raw_id_col,
          sampleid_meta_col = r$indices$meta_id_col,
          meta_type_col = r$indices$meta_type_col,
          meta_acqorder_col = r$indices$meta_acqorder_col,
          meta_batch_col = r$indices$meta_batch_col,
          sample_ids = r$indices$id_samples,
          qcpool_ids = r$indices$id_qcpool,
          blank_ids = r$indices$id_blanks,
          settings_data = r$settings_data
        )

        shiny::withProgress(
          message = "Rendering report.....",
          value = 0,
          {
            shiny::incProgress(1/10)
            Sys.sleep(1)
            shiny::incProgress(5/10)
            rmarkdown::render(input = temp_report,
                              output_file = file,
                              envir = e)
          }
        )
      }
    )

  })
}
