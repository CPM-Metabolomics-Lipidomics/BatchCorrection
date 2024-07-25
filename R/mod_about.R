#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      shiny::h3("Issues"),
      shiny::p("If you have any ideas to extend this shiny app please send me an email. If you have any issue please send me an email or go to the ",
        shiny::a("issue tracker.", href = "http://github.com/CPM-Metabolomics-Lipidomics/BatchCorrection/issues", target = "_blank"),
        "Cheers, Rico"),
      shiny::h3("Session info"),
      shiny::verbatimTextOutput(ns("about_session"))
    )
  )
}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$about_session <- renderPrint({
      sessioninfo::session_info()
    })
  })
}
