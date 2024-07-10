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
          shiny::p("Show histogram of RSD"),
          plotly::plotlyOutput(
            outputId = ns("viz_histogram")
          )
        )
      ),
      bslib::nav_panel(
        title = "Trend plot",
        bslib::card(
          plotly::plotlyOutput(
            outputId = ns("viz_trend_plot")
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
          shiny::plotOutput(
            outputId = ns("viz_pca_plot")
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

    output$viz_trend_plot <- plotly::renderPlotly({
      shiny::req(r$tables$meta_data,
                 r$tables$raw_data,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_acqorder_col,
                 r$indices$meta_batch_col,
                 r$indices$id_qcpool)

      print("Create trend plot")
      trend_data <- prepare_trend_data(data = r$tables$raw_data,
                                       meta_data = r$tables$meta_data,
                                       sampleid_raw_col = r$indices$raw_id_col,
                                       sampleid_meta_col = r$indices$meta_id_col,
                                       order_col = r$indices$meta_acqorder_col,
                                       batch_col = r$indices$meta_batch_col,
                                       id_qcpool = r$indices$id_qcpool)

      trend_plot(data = trend_data,
                 sampleid_raw_col = r$indices$raw_id_col,
                 batch_col = r$indices$meta_batch_col)
    })


    output$viz_heatmap <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$raw_data,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_batch_col,
                 r$indices$meta_type_col,
                 r$indices$id_qcpool,
                 r$indices$id_samples)

      print("Create heatmap")

      res <- prepare_heatmap_data(data = r$tables$raw_data,
                                  meta_data = r$tables$meta_data,
                                  sampleid_raw_col = r$indices$raw_id_col,
                                  sampleid_meta_col = r$indices$meta_id_col,
                                  sampletype_col = r$indices$meta_type_col,
                                  batch_col = r$indices$meta_batch_col,
                                  id_qcpool = r$indices$id_qcpool,
                                  id_samples = r$indices$id_samples)

      ComplexHeatmap::Heatmap(
        matrix = res$data,
        heatmap_legend_param = list(title = "Z-score"),
        right_annotation = ComplexHeatmap::rowAnnotation(
          df = res$row_ann,
          col = res$colors_ann
        ),
        cluster_rows = TRUE,
        cluster_columns = FALSE
      )
    })


    output$viz_pca_plot <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$raw_data,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_batch_col,
                 r$indices$meta_type_col,
                 r$indices$id_qcpool,
                 r$indices$id_samples)

      print("Create pca plot")

      pca_data <- prepare_pca_data(data = r$tables$raw_data,
                                   meta_data = r$tables$meta_data,
                                   sampleid_raw_col = r$indices$raw_id_col,
                                   sampleid_meta_col = r$indices$meta_id_col,
                                   id_samples = r$indices$id_samples,
                                   id_qcpool = r$indices$id_qcpool)

      p1 <- pca_scores_plot(data = pca_data,
                            sampletype_col = r$indices$meta_type_col,
                            batch_col = r$indices$meta_batch_col)

      p2 <- pca_loadings_plot(data = pca_data)

      patchwork::wrap_plots(p1, p2,
                            ncol = 2)

    })

  })
}
