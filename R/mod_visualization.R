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
mod_visualization_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
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
          bslib::card(
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
  })
}
