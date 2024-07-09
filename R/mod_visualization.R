#' visualization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visualization_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' visualization Server Functions
#'
#' @noRd 
mod_visualization_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_visualization_ui("visualization_1")
    
## To be copied in the server
# mod_visualization_server("visualization_1")
