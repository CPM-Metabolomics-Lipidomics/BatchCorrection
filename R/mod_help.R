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
        shiny::p("The meta data can be read in different formats, i.e. tsv (tab separated), csv (comma separated) or xlsx (Excel).
                 In the meta data part you have to define several columns."),
        shiny::HTML("<ul>
                      <li><b>Sample ID:</b> column with sample ID's</li>
                      <li><b>Sample type:</b> column with sample type, i.e. blank, pooled sample, sample, etc.</li>
                      <li><b>Acquisition order:</b> column with the acquisition order. This is over all batches.</li>
                      <li><b>Batch:</b> column with batch information.</li>
                    </ul>"),
        shiny::p("The app tries to recognize several column names, but please check them."),
        shiny::p("The ", shiny::strong("text patterns"), "need to be defined in order to recognize from the ", shiny::strong("sample type column"), "which samples belong to which sample type.",
                 "Regular expression are use for the recognition. After loading the meta data the two graphs at the bottom will show how the different sample types are recognized.",
                 "The left graph shows the different sample types and the count. The right graph will show the different sample types and the count, after applying the filtering,
                 based on the text patterns. It is important to have:"),
        shiny::HTML("<ul>
                      <li>Blanks</li>
                      <li>Pooled samples</li>
                      <li>Samples</li>
                    </ul>"),
        shiny::p("The pooled samples are important to be recognized, because they are used for several visualizations (e.g. histograms, trend plots) and batch correction methods."),
        shiny::h3("Raw data"),
        shiny::p("The raw data can be read in different formats, i.e. tsv (tab separated), csv (comma separated) or xlsx (Excel).
                 The first column of the raw data needs to be the column with the sample ID's."),
      ),
      bslib::nav_panel(
        title = "Data overview",
        shiny::h3("Missing values"),
        shiny::p("The missing values plot shows an overview of how many missing values are present in the features.
                 The amount of missing values can be viewed for the pooled samples or the samples and pooled samples together (default setting).
                 The batch correction methods are sensitive for missing values. For some batch correction methods (e.g. LOESS) it is important to have no
                 missing values in the pooled samples."),
        shiny::h3("Histogram"),
        shiny::p("The histograms are calculated based on the pooled samples. Here an overview of the relative standard deviation (RSD) of all features in the
                 pooled samples is shown. The left graph shows the RSD over all the batches together. The right graphs shows the RSD per batch."),
        shiny::h3("Trend plot"),
        shiny::p("De trend plots are calculated based on the pooled samples. The trend plot is calculated to show how the pooled samples behaved during the all
                 the batches. There are two options:"),
        shiny::HTML("<ul>
                      <li>show the trend over all batches (default)</li>
                      <li>show the trend per batch</li>
                    </ul>"),
        shiny::p("The trend, per feature, is calculated as the fold change in signal against the reference sample. The reference sample is the first pooled sample over all
                 batches or the first pooled sample in each batch. The trend plots can be used to evaluate if there is a trend in signal within a batch or over all batches."),
        shiny::h3("Heatmap"),
        shiny::p("The heatmap will help to evaluate if there is a batch effect. On the right side the samples are colored according to sample type and batch."),
        shiny::h3("PCA"),
        shiny::h3("Relative log expression")
      ),
      bslib::nav_panel(
        title = "Batch correction",
        shiny::h3("Median"),
        shiny::h3("LOESS"),
        shiny::h3("ComBat (SVA)")
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
