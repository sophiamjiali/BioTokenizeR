# ==============================================================================
# Purpose:            Tests the visualize_tokens() function and plotting
# Author:             Sophia Li
# Date:               2025-11-28
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Built for integration with the usethis package
# ==============================================================================

library(BioTokenizeR)

# Define constants reused across tests: partial string matching
STATS_ERROR <- "`statistics` must be"
TOP_N_ERROR <- "`top_n` must be"

PLOT_NAMES  <- c("frequency_distribution", "top_tokens", "cumulative_coverage")

# =====| Input Validation |=====================================================

test_that("visualize_tokens: correctly validates inputs", {
  
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
  testthat::expect_error(visualize_tokens(statistics = c(1, 2, 3),
                                          top_n      = 30,
                                          output_dir = NULL), 
                         STATS_ERROR)
  testthat::expect_error(visualize_tokens(statistics = "test",
                                          top_n      = 30,
                                          output_dir = NULL), 
                         STATS_ERROR)
  testthat::expect_error(visualize_tokens(statistics = 123,
                                          top_n      = 30,
                                          output_dir = NULL), 
                         STATS_ERROR)
  
  ## 2. Rejects Top N that is zero
  testthat::expect_error(visualize_tokens(statistics = data$dna_summary,
                                          top_n      = 0,
                                          output_dir = NULL),
                         TOP_N_ERROR)
  testthat::expect_error(visualize_tokens(statistics = data$rna_summary,
                                          top_n      = 0,
                                          output_dir = NULL),
                         TOP_N_ERROR)
  testthat::expect_error(visualize_tokens(statistics = data$aa_summary,
                                          top_n      = 0,
                                          output_dir = NULL),
                         TOP_N_ERROR)

  
  ## 3. Rejects Top N that is less than zero
  testthat::expect_error(visualize_tokens(statistics = data$dna_summary,
                                          top_n      = -3,
                                          output_dir = NULL),
                         TOP_N_ERROR)
  testthat::expect_error(visualize_tokens(statistics = data$rna_summary,
                                          top_n      = -3,
                                          output_dir = NULL),
                         TOP_N_ERROR)
  testthat::expect_error(visualize_tokens(statistics = data$aa_summary,
                                          top_n      = -3,
                                          output_dir = NULL),
                         TOP_N_ERROR)
})

# =====| Generating Visualizations |============================================

test_that("visualize_tokens: correctly returns plots", {
  
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
  dna_plots <- visualize_tokens(statistics = data$dna_summary,
                                top_n      = 3,
                                output_dir = NULL)
  rna_plots <- visualize_tokens(statistics = data$rna_summary,
                                top_n      = 3,
                                output_dir = NULL)
  aa_plots  <- visualize_tokens(statistics = data$aa_summary,
                                top_n      = 3,
                                output_dir = NULL)
  
  ## 1. Correctly returns a list of plots
  testthat::expect_named(dna_plots, PLOT_NAMES) 
  testthat::expect_named(rna_plots, PLOT_NAMES)
  testthat::expect_named(aa_plots, PLOT_NAMES)
  
  ## 2. correctly returns the frequency distribution plot
  testthat::expect_s3_class(dna_plots$frequency_distribution, "ggplot")
  testthat::expect_s3_class(rna_plots$frequency_distribution, "ggplot")
  testthat::expect_s3_class(aa_plots$frequency_distribution, "ggplot")
  
  ## 3. correctly returns the top tokens plot
  testthat::expect_s3_class(dna_plots$top_tokens, "ggplot")
  testthat::expect_s3_class(rna_plots$top_tokens, "ggplot")
  testthat::expect_s3_class(aa_plots$top_tokens, "ggplot")
  
  ## 4. correctly returns the cumulative coverage plot
  testthat::expect_s3_class(dna_plots$cumulative_coverage, "ggplot")
  testthat::expect_s3_class(rna_plots$cumulative_coverage, "ggplot")
  testthat::expect_s3_class(aa_plots$cumulative_coverage, "ggplot")
})

# [END]