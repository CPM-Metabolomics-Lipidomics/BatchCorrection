#' visualization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r6 r6 object with all information
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
          shiny::p("Show trend plot"),
          shiny::textOutput(outputId = ns("viz_text_output")),
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
mod_visualization_server <- function(id, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$viz_trend_plot <- plotly::renderPlotly({
      shiny::req(r6$tables$meta_data,
                 r6$tables$raw_data)

      print("Show trend plot")

      trend_plot()
    })

  })
}
