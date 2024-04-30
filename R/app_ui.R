#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#'
#' @noRd
#'
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny::navbarPage(
      title = paste0("CPM - BatchCorrection | v", utils::packageVersion("BatchCorrection")),
      id = "navbar_home",
      shiny::tabPanel(
        title = "Data",
          mod_data_ui("data")
      ), # end Data
      shiny::tabPanel(
        title = "Visualization",
        shiny::p("Here the different visualizations will be shown.")
      )
    ) # end navbar
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#'
#' @noRd
#'
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BatchCorrection"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
