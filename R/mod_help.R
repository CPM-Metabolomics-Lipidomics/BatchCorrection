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
        shiny::p("In the ", shiny::strong("Data"), "sheet you can upload your meta data (sample annotation data) and raw data (measurement data)."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("Meta data"),
        shiny::p("This tab is designed for the upload and curation of sample meta data and is formatted with samples as rows and annotations as columns.
                 The meta data can be read in different formats, i.e. tsv (tab separated), csv (comma separated) or xlsx (Excel).
                 The meta data needs 4 essential columns. The app tries to recognize these columns, but they can be adjusted here. The 4 essential columns are:"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("Sample ID:"), " column with unique sample ID's"),
          shiny::tags$li(shiny::strong("Sample type:"), " column with sample type, i.e. blank, pooled sample, sample, etc."),
          shiny::tags$li(shiny::strong("Acquisition order:"), " column with the acquisition order. This is over all batches."),
          shiny::tags$li(shiny::strong("Batch:"), " column with batch information.")
        ),
        shiny::p("The ", shiny::strong("text patterns"), "need to be defined in order to recognize the different sample types from the ", shiny::strong("sample type column"), ".",
                 "Regular expression are use for the recognition. After loading the meta data the two graphs at the bottom will show how the different sample types are recognized.",
                 "The left graph shows the different sample types and the count. The right graph will show the different sample types and the count, after applying the filtering,
                 based on the text patterns. It is important to have:"),
        shiny::tags$ul(
          shiny::tags$li("Blanks"),
          shiny::tags$li("Pooled samples"),
          shiny::tags$li("Samples")
        ),
        shiny::p("The pooled samples are important to be recognized, because they are used for several visualizations (e.g. histograms, trend plots) and batch correction methods."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("Raw data"),
        shiny::p("This tab is designed for the upload and curation of measurement data. The data must be provided with samples as rows and features as columns. The values can only be numerical or missing.
                  The first column of the raw data needs to be the column with unique sample ID's. The raw data can be read in different formats, i.e. tsv (tab separated), csv (comma separated) or xlsx (Excel)."),
      ),
      bslib::nav_panel(
        title = "Data overview",
        shiny::h3("Missing values"),
        shiny::p("The missing values plot shows an overview of how many missing values are present in the features.
                 The amount of missing values can be viewed for the pooled samples or the samples and pooled samples together (default setting).
                 The batch correction methods are sensitive for missing values. For some batch correction methods (e.g. LOESS) it is important to have no
                 missing values in the pooled samples."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("Histogram"),
        shiny::p("The histograms are calculated based on the pooled samples. Here an overview of the relative standard deviation (RSD) of all features in the
                 pooled samples is shown. The left graph shows the RSD over all the batches together. The right graphs shows the RSD per batch."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("Trend plot"),
        shiny::p("De trend plots are calculated based on the pooled samples. The trend plot is calculated to show how the pooled samples behaved during the all
                 the batches. There are two options:"),
        shiny::tags$ul(
          shiny::tags$li("show the trend over all batches (default)"),
          shiny::tags$li("show the trend per batch")
        ),
        shiny::p("The trend, per feature, is calculated as the fold change in signal against the reference sample. The reference sample is the first pooled sample over all
                 batches or the first pooled sample in each batch. The trend plots can be used to evaluate if there is a trend in signal within a batch or over all batches."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("Heatmap"),
        shiny::p("The heatmap will help to evaluate if there is a batch effect. On the right side the samples are colored according to sample type and batch. Hierarchical clustering is applied to the samples.
                 z-scores are calculated for the heatmap."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("PCA"),
        shiny::p("Principal Component Analysis (PCA) is a widely-used technique for
                 dimensionality reduction and data visualization. Two plots are made available through PCA:
                 the scores plot and the loadings plot. The scores plot displays the data points in the
                 reduced-dimensional space created by PCA. Each point on the scores plot represents a
                 sample, but instead of being plotted using the original variables, it's plotted using the
                 principal components. The scores plot lets you visualize multivariate data in a 2D, making
                 patterns, clusters, or outliers more evident. The loadings plot shows how each feature
                 contributes to the principal components. Here, principal component 1 - 4 can be used to evaluate if there
                 is a batch effect. The density plots around the scores plot assist to determine if there is a batch effect."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("Relative log expression"),
        shiny::p("Relative log expression (RLE) plots are a powerful tool for visualising unwanted variation in high dimensional data.
                 The RLE plot is a useful diagnostic plot to visualize the differences between the distributions of the signal of the featrures across samples.
                 It shows the boxplots of the feature signal deviations of each sample to those of a reference sample (defined as the median across the samples).
                 Ideally, the distributions should be centered around the zero line and as tight as possible. Clear deviations indicate the need for normalization and/or the presence of outlying samples. ")
      ),
      bslib::nav_panel(
        title = "Batch correction",
        shiny::p("The app assumes that quality control methods are applied to the data, i.e. removal of features with high RSD, high background etc. The app can filter out features
                 with missing values, but it is best to do this outside the app. Especially, features which have missing values in the pooled samples should be removed or carefully imputed."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("Median"),
        shiny::p("With the median batch correction method a correction factor for each feature is calculate by dividing the overall median by the median per batch for all
                 features in the pooled samples. This correction factor is then used to correct all samples."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("LOESS"),
        shiny::p("The LOESS batch correction method [1] depends on pooled samples being measured in regular intervals. It can correct signal intensity
                 drifts over time. In short, a low-order nonlinear locally estimated smooth-ing function (LOESS) is fitted to the QC
                 data with respect to the order of injection. A correction curve for the whole analytical run is then interpolated, to which the
                 total data set for that feature is normalized. It is important that there are sufficient pooled samples per batch, preferably 6 or more. The
                 LOESS batch correction can not handle missing values in the pooled sample."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("ComBat (SVA)"),
        shiny::p("The sva package [2] contains functions for removing batch effects and other un-
                 wanted variation in high-throughput experiments. Specifically, the sva pack-
                 age contains functions for identifying and building surrogate variables for high-
                 dimensional data sets. Surrogate variables are covariates constructed directly
                 from high-dimensional data (like gene expression/RNA sequencing/methylation/brain
                 imaging data) that can be used in subsequent analyses to adjust for unknown,
                 unmodeled, or latent sources of noise.
                 The sva package can be used to remove artifacts in two ways: (1) identifying
                 and estimating surrogate variables for unknown sources of variation in high-
                 throughput experiments and (2) directly removing known batch effects using
                 ComBat [3]."),
        shiny::hr(width = "75%", style = "margin-left:12.5%;"),
        shiny::h3("References"),
        shiny::tags$ol(
          shiny::tags$li("Dunn et al. Nature Protocols 6, 1060-1083 (2011)"),
          shiny::tags$li("Leek JT, Johnson WE, Parker HS, Jaffe AE, and Storey JD. (2012) The
                         sva package for removing batch effects and other unwanted variation in
                         high-throughput experiments. Bioinformatics DOI:10.1093/bioinformatics/bts034"),
          shiny::tags$li("Johnson WE, Li C, Rabinovic A (2007) Adjusting batch effects in mi-
                         croarray expression data using empirical Bayes methods. Biostatistics, 8
                         (1), 118-127")
        )
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
