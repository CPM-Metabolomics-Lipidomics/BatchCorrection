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
mod_visualization_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = "Trend plot"
      ),
      bslib::nav_panel(
        title = "Heatmap"
      ),
      bslib::nav_panel(
        title = "PCA"
      )
    )
  )
}

#' visualization Server Functions
#'
#' @noRd
mod_visualization_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
