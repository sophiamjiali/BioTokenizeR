# ==============================================================================
# Purpose:            Tests the plot_token_frequency_distribution() function
# Author:             Sophia Li
# Date:               2025-11-28
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Built for integration with the usethis package
# ==============================================================================

library(BioTokenizeR)

# Define constants reused across tests: partial string matching
STATS_ERROR <- "`statistics` must be"

# =====| Input Validation |=====================================================

test_that("plot_token_frequency_distribution: correctly validates inputs", {
  
  # Generate DNA, RNA, and AA sequences to test visualization
  data <- generate_data(
    n          = 3, 
    length     = 500, 
    vocab_size = 3,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = TRUE,
    summarize  = TRUE
  )
  
  ## 1. Rejects non-bioBPE_summary
  testthat::expect_error(plot_token_frequency_distribution(statistics = c(1, 2, 3),
                                                           output_dir = NULL), 
                         STATS_ERROR)
  testthat::expect_error(plot_token_frequency_distribution(statistics = "test",
                                                           output_dir = NULL), 
                         STATS_ERROR)
  testthat::expect_error(plot_token_frequency_distribution(statistics = 123,
                                                           output_dir = NULL), 
                         STATS_ERROR)
  
  ## 2. Correctly returns the plot
  testthat::expect_s3_class(plot_token_frequency_distribution(statistics = data$dna_summary,
                                                              output_dir = NULL), 
                            "ggplot")
  testthat::expect_s3_class(plot_token_frequency_distribution(statistics = data$rna_summary,
                                                              output_dir = NULL), 
                            "ggplot")
  testthat::expect_s3_class(plot_token_frequency_distribution(statistics = data$aa_summary,
                                                              output_dir = NULL), 
                            "ggplot")
})

# =====| Generating Visualizations |============================================

test_that("plot_cumulative_coverage: correctly returns plots", {
  
  # Generate DNA, RNA, and AA sequences to test visualization
  data <- generate_data(
    n          = 3, 
    length     = 500, 
    vocab_size = 3,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = TRUE,
    summarize  = TRUE
  )
  
  # Correctly generate the plots
  dna_plot <- plot_token_frequency_distribution(statistics = data$dna_summary,
                                                output_dir = NULL)
  rna_plot <- plot_token_frequency_distribution(statistics = data$rna_summary,
                                                output_dir = NULL)
  aa_plot  <- plot_token_frequency_distribution(statistics = data$aa_summary,
                                                output_dir = NULL)
  
  ## 1. Correctly returns the plots
  testthat::expect_s3_class(dna_plot, "ggplot")
  testthat::expect_s3_class(rna_plot, "ggplot")
  testthat::expect_s3_class(aa_plot, "ggplot")
})

# [END]