#' batch_correction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r r object with all information
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_batch_correction_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        title = "Batch correction",
        bslib::tooltip(
          shiny::selectInput(
            inputId = ns("bc_select_method"),
            label = "Select method",
            choices = c("Median correction" = "median",
                        "LOESS correction" = "loess",
                        "ComBat (SVA)" = "combat"),
            selected = "median"
          ),
          "Select a batch corretion method."
        )
      ),
      bslib::navset_card_tab(
        bslib::nav_panel(
          title = "Histogram",
          bslib::card(
            shiny::plotOutput(
              outputId = ns("bc_histogram")
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
                  inputId = ns("bc_trend_plot_view_select"),
                  label = "Overview",
                  choices = c("Overall" = "log2fc",
                              "Per batch" = "log2fc_batch")
                )
              ),
              shiny::plotOutput(
                outputId = ns("bc_trend_plot")
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Heatmap",
          bslib::card(
            shiny::plotOutput(
              outputId = ns("bc_heatmap")
            )
          )
        ),
        bslib::nav_panel(
          title = "PCA",
          bslib::card(
            shiny::plotOutput(
              outputId = ns("bc_pca_plot")
            )
          )
        ),
        bslib::nav_panel(
          title = "Relative log expression",
          bslib::card(
            shiny::plotOutput(
              outputId = ns("bc_rle_plot")
            )
          )
        )
      ) # end navset_card_tab
    )
  )
}

#' batch_correction Server Functions
#'
#' @noRd
mod_batch_correction_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_batch_correction_ui("batch_correction_1")

## To be copied in the server
# mod_batch_correction_server("batch_correction_1")
