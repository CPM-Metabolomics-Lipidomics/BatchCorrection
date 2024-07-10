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
      id = ns('skeleton_1'),
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
          shiny::p("Show heatmap plot")
        )
      ),
      bslib::nav_panel(
        title = "PCA",
        bslib::card(
          shiny::p("Show pca plots")
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

  })
}
