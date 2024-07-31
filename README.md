
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BatchCorrection

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `BatchCorrection` is to check if there are possible batch
effects in the data and serveral batch correction methods are available
to apply if needed.

Batch correction methods available:

- **median correction:** With the median batch correction method a
  correction factor for each feature is calculate by dividing the
  overall median by the median per batch for all features in the pooled
  samples. This correction factor is then used to correct all samples.
- **LOESS:** The LOESS batch correction method \[1\] depends on pooled
  samples being measured in regular intervals. It can correct signal
  intensity drifts over time. In short, a low-order nonlinear locally
  estimated smooth-ing function (LOESS) is fitted to the QC data with
  respect to the order of injection. A correction curve for the whole
  analytical run is then interpolated, to which the total data set for
  that feature is normalized. It is important that there are sufficient
  pooled samples per batch, preferably 6 or more. The LOESS batch
  correction can not handle missing values in the pooled sample.
- **ComBat (SVA):** The sva package \[2\] contains functions for
  removing batch effects and other un- wanted variation in
  high-throughput experiments. Specifically, the sva pack- age contains
  functions for identifying and building surrogate variables for high-
  dimensional data sets. Surrogate variables are covariates constructed
  directly from high-dimensional data (like gene expression/RNA
  sequencing/methylation/brain imaging data) that can be used in
  subsequent analyses to adjust for unknown, unmodeled, or latent
  sources of noise. The sva package can be used to remove artifacts in
  two ways: (1) identifying and estimating surrogate variables for
  unknown sources of variation in high- throughput experiments and (2)
  directly removing known batch effects using ComBat \[3\].

## Installation

You can install the development version of BatchCorrection like so:

``` r
remotes::install_github("CPM-Metabolomics-Lipidomics/BatchCorrection")
```

## Run

You can run the app with:

``` r
BatchCorrection::run_app()
```

## Reference

1.  Dunn et al. Nature Protocols 6, 1060-1083 (2011)
2.  Leek JT, Johnson WE, Parker HS, Jaffe AE, and Storey JD. (2012) The
    sva package for removing batch effects and other unwanted variation
    in high-throughput experiments. Bioinformatics
    <DOI:10.1093/bioinformatics/bts034>
3.  Johnson WE, Li C, Rabinovic A (2007) Adjusting batch effects in
    microarray expression data using empirical Bayes methods.
    Biostatistics, 8(1), 118-127
