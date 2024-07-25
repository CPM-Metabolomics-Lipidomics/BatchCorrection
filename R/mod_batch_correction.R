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
#' @importFrom bsicons bs_icon
#' @importFrom utils write.csv
mod_batch_correction_ui <- function(id){
  ns <- NS(id)

  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        title = shiny::uiOutput(
          outputId = ns("bc_download_ui")
        ),
        shiny::selectInput(
          inputId = ns("bc_select_method"),
          label = "Select method",
          choices = c("Median correction" = "median",
                      "LOESS correction" = "loess",
                      "ComBat (SVA)" = "combat"),
          selected = "median"
        ),
        shiny::uiOutput(
          outputId = ns("bc_options_ui")
        ),
        shiny::actionButton(
          inputId = ns("bc_apply_method"),
          label = "Apply correction"
        ),
        shiny::uiOutput(
          outputId = ns("bc_status_text")
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
            bslib::page_sidebar(
              sidebar = bslib::sidebar(
                open = FALSE,
                shiny::selectInput(
                  inputId = ns("bc_pca_x"),
                  label = "x-axis",
                  choices = paste0("PC", 1:4),
                  selected = "PC1"
                ),
                shiny::selectInput(
                  inputId = ns("bc_pca_y"),
                  label = "y-axis",
                  choices = paste0("PC", 1:4),
                  selected = "PC2"
                )
              ),
              shiny::plotOutput(
                outputId = ns("bc_pca_plot")
              )
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

    shiny::observeEvent(input$bc_select_method, {
      r$tables$bc_data <- NULL
      output$bc_status_text <- shiny::renderUI({
        NULL
      })

      if(input$bc_select_method == "loess") {
        output$bc_options_ui <- shiny::renderUI({
          shiny::tagList(
            shiny::selectInput(
              inputId = ns("bc_loess_batch"),
              label = "Batch / over all",
              choices = c("Per batch" = "batch",
                          "Over all batches" = "over_all"),
              selected = "batch"
            ),
            shiny::numericInput(
              inputId = ns("bc_loess_span"),
              label = "Span",
              value = 0.75,
              min = 0,
              max = 1,
              step = 0.01
            )
          )
        })
      } else {
        output$bc_options_ui <- shiny::renderUI({
          NULL
        })
      }
    })


    shiny::observeEvent(input$bc_apply_method, {
      shiny::req(r$tables$clean_data,
                 r$tables$meta_data,
                 input$bc_select_method,
                 r$bc_applied)

      output$bc_status_text <- shiny::renderUI({
        NULL
      })

      switch(
        input$bc_select_method,
        "median" = {
          print("Median batch correction")
          r$tables$bc_data <- median_bc(data = r$tables$clean_data,
                                        meta_data = r$tables$meta_data,
                                        sampleid_raw_col = r$indices$raw_id_col,
                                        sampleid_meta_col = r$indices$meta_id_col,
                                        id_samples = r$indices$id_samples,
                                        id_qcpool = r$indices$id_qcpool,
                                        batch_col = r$indices$meta_batch_col)
        },
        "loess" = {
          print("LOESS batch correction")
          r$tables$bc_data <- loess_bc(data = r$tables$clean_data,
                                       meta_data = r$tables$meta_data,
                                       sampleid_raw_col = r$indices$raw_id_col,
                                       sampleid_meta_col = r$indices$meta_id_col,
                                       id_samples = r$indices$id_samples,
                                       id_qcpool = r$indices$id_qcpool,
                                       batch_col = r$indices$meta_batch_col,
                                       order_col = r$indices$meta_acqorder_col,
                                       span = input$bc_loess_span,
                                       method = input$bc_loess_batch)
        },
        "combat" = {
          r$tables$bc_data <- combat_bc(data = r$tables$clean_data,
                                        meta_data = r$tables$meta_data,
                                        sampleid_raw_col = r$indices$raw_id_col,
                                        sampleid_meta_col = r$indices$meta_id_col,
                                        id_samples = r$indices$id_samples,
                                        id_qcpool = r$indices$id_qcpool,
                                        batch_col = r$indices$meta_batch_col)
        }
      )

      if(!is.null(r$tables$bc_data)) {
        r$bc_applied <- input$bc_select_method

        # Changes status update while just selecting from median -> loess
        output$bc_status_text <- shiny::renderUI({
          bc_method <- switch(
            input$bc_select_method,
            "median" = "Median batch correction applied.",
            "loess" = "LOESS batch correction applied.",
            "combat" = "ComBat batch correction applied."
          )
          shiny::p(bc_method)
        })

        print("Calculating...")
        print("  * trend plot")
        r$data_bc$trend <- prepare_trend_data(data = r$tables$bc_data,
                                              meta_data = r$tables$meta_data,
                                              sampleid_raw_col = r$indices$raw_id_col,
                                              sampleid_meta_col = r$indices$meta_id_col,
                                              order_col = r$indices$meta_acqorder_col,
                                              batch_col = r$indices$meta_batch_col,
                                              id_qcpool = r$indices$id_qcpool)

        print("  * histogram")
        r$data_bc$histogram <- prepare_hist_data(data = r$tables$bc_data,
                                                 meta_data = r$tables$meta_data,
                                                 sampleid_raw_col = r$indices$raw_id_col,
                                                 sampleid_meta_col = r$indices$meta_id_col,
                                                 batch_col = r$indices$meta_batch_col,
                                                 id_qcpool = r$indices$id_qcpool)

        print("  * heatmap")
        r$data_bc$heatmap <- prepare_heatmap_data(data = r$tables$bc_data,
                                                  meta_data = r$tables$meta_data,
                                                  sampleid_raw_col = r$indices$raw_id_col,
                                                  sampleid_meta_col = r$indices$meta_id_col,
                                                  sampletype_col = r$indices$meta_type_col,
                                                  batch_col = r$indices$meta_batch_col,
                                                  id_qcpool = r$indices$id_qcpool,
                                                  id_samples = r$indices$id_samples)

        print("  * PCA")
        r$data_bc$pca <- prepare_pca_data(data = r$tables$bc_data,
                                          meta_data = r$tables$meta_data,
                                          sampleid_raw_col = r$indices$raw_id_col,
                                          sampleid_meta_col = r$indices$meta_id_col,
                                          id_samples = r$indices$id_samples,
                                          id_qcpool = r$indices$id_qcpool)

        print("  * RLE")
        r$data_bc$rle <- prepare_rle_data(data = r$tables$bc_data,
                                          meta_data = r$tables$meta_data,
                                          sampleid_raw_col = r$indices$raw_id_col,
                                          sampleid_meta_col = r$indices$meta_id_col,
                                          order_col = r$indices$meta_acqorder_col,
                                          id_samples = r$indices$id_samples)

        print("...done!")
      } else {
        output$bc_status_text <- shiny::renderUI({
          shiny::p(shiny::span("Error: there are missing values in the pooled samples.",
                               style = "color:red;"),
                   "Please select a different batch correction method or remove the missing values!")

          r$bc_applied <- "none"
        })
      }
    })


    # Changes status update while just selecting from median -> loess
    output$bc_status_text <- shiny::renderUI({
      shiny::req(input$bc_select_method,
                 !is.null(r$tables$bc_data),
                 r$bc_applied)

      print("Rico")
      print(r$bc_applied)

      if(!is.null(r$tables$bc_data)) {
        bc_method <- switch(
          input$bc_select_method,
          "median" = "Median batch correction applied.",
          "loess" = "LOESS batch correction applied.",
          "combat" = "ComBat batch correction applied."
        )
        shiny::p(bc_method)
      } else {
        NULL
      }
    })
    #---------------------------------------------------------------- plots ----
    output$bc_trend_plot <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$bc_data,
                 r$data_bc$trend,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_acqorder_col,
                 r$indices$meta_batch_col,
                 r$indices$id_qcpool,
                 input$bc_trend_plot_view_select)

      if(!(input$bc_trend_plot_view_select %in% c("log2fc", "log2fc_batch"))) {
        yaxis <- "log2fc"
      } else {
        yaxis <- input$bc_trend_plot_view_select
      }

      print("Show trend plot")
      trend_plot(data = r$data_bc$trend,
                 sampleid_raw_col = r$indices$raw_id_col,
                 batch_col = r$indices$meta_batch_col,
                 yaxis = yaxis)
    })


    output$bc_heatmap <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$bc_data,
                 r$data_bc$heatmap,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_batch_col,
                 r$indices$meta_type_col,
                 r$indices$id_qcpool,
                 r$indices$id_samples)

      print("Show heatmap")

      ComplexHeatmap::Heatmap(
        matrix = r$data_bc$heatmap$data,
        heatmap_legend_param = list(title = "Z-score"),
        right_annotation = ComplexHeatmap::rowAnnotation(
          df = r$data_bc$heatmap$row_ann,
          col = r$data_bc$heatmap$colors_ann
        ),
        cluster_rows = TRUE,
        cluster_columns = FALSE
      )
    })


    output$bc_pca_plot <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$bc_data,
                 r$data_bc$pca,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_batch_col,
                 r$indices$meta_type_col,
                 r$indices$id_qcpool,
                 r$indices$id_samples,
                 input$bc_pca_x,
                 input$bc_pca_y)

      print("Show pca plots")

      p1 <- pca_scores_plot(data = r$data_bc$pca,
                            sampletype_col = r$indices$meta_type_col,
                            batch_col = r$indices$meta_batch_col,
                            xaxis = input$bc_pca_x,
                            yaxis = input$bc_pca_y)
      p2 <- pca_loadings_plot(data = r$data_bc$pca,
                              xaxis = input$bc_pca_x,
                              yaxis = input$bc_pca_y)

      patchwork::wrap_plots(p1, p2,
                            ncol = 2)

    })


    output$bc_histogram <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 r$tables$bc_data,
                 r$data_bc$histogram,
                 r$indices$raw_id_col,
                 r$indices$meta_id_col,
                 r$indices$meta_batch_col,
                 r$indices$id_qcpool)

      print("Show histograms")

      histogram_plot(data = r$data_bc$histogram)
    })


    output$bc_rle_plot <- shiny::renderPlot({
      shiny::req(r$data_bc$rle,
                 r$tables$bc_data,
                 r$indices$raw_id_col,
                 r$indices$meta_batch_col)

      print("Show RLE plot")

      rle_plot(data = r$data_bc$rle,
               sampleid_raw_col = r$indices$raw_id_col,
               batch_col = r$indices$meta_batch_col)
    })

    #------------------------------------------------------------- download ----
    output$bc_download_ui <- shiny::renderUI({
      shiny::tagList(
        bslib::popover(
          bsicons::bs_icon(name = "cloud-download-fill"),
          if(!is.null(r$tables$bc_data)) {
            shiny::downloadButton(
              outputId = ns("bc_download"),
              label = "Download results"
            )
          } else {
            shiny::p("No download yet!")
          }
        )
      )
    })


    output$bc_download <- shiny::downloadHandler(
      filename = function() {
        paste0("batch_correction_results_", input$bc_select_method, ".csv")
      },
      content = function(file) {
        if(!is.null(r$tables$bc_data)) {
          write.csv(r$tables$bc_data,
                    file = file,
                    row.names = FALSE)
        }
      }
    )

  })
}
