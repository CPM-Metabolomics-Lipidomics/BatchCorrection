#' help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib navset_card_tab nav_panel
mod_help_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = "Data",
        shiny::p("In the ", shiny::strong("Data"), "sheet you can upload your meta data and raw data."),
        shiny::h3("Meta data"),
        shiny::p("In the meta data part you have to define several columns."),
        shiny::HTML("<ul>
                      <li><b>Sample ID:</b> column with sample ID's</li>
                      <li><b>Sample type:</b> column with sample type, i.e. blank, pooled sample, sample, etc.</li>
                      <li><b>Acquisition order:</b> column with the acquisition order. This is over all batches.</li>
                      <li><b>Batch:</b> column with batch information.</li>
                    </ul>"),
        shiny::p("The app tries to recognize several columns, but please check them."),
        shiny::p("The ", shiny::strong("text patterns"), "need to be defined by to recognize from the ", shiny::strong("sample type column"), "which samples is which.",
                 "Regular expression are use for the recognition. After loading the meta data the two graphs at the bottom will show how the different sample types are recognized.",
                 "The left graph shows the different sample types and the count. The right graph will show the different sample types and the count, after applying the filtering,
                 based on the text patterns. It is important to have:"),
        shiny::HTML("<ul>
                      <li>Blanks</li>
                      <li>Pooled samples</li>
                      <li>Samples</li>
                    </ul>")
      ),
      bslib::nav_panel(
        title = "Data overview"
      ),
      bslib::nav_panel(
        title = "Batch correction"
      )
    )
  )
}

#' help Server Functions
#'
#' @noRd
mod_help_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
