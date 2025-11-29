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

test_that("visualize_tokens: correctly functions", {
  
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
  
  ## 4. Correctly returns a list of plots
  testthat::expect_named(visualize_tokens(statistics = data$dna_summary,
                                          top_n      = 3,
                                          output_dir = NULL), 
                         PLOT_NAMES)
  testthat::expect_named(visualize_tokens(statistics = data$rna_summary,
                                          top_n      = 3,
                                          output_dir = NULL), 
                         PLOT_NAMES)
  testthat::expect_named(visualize_tokens(statistics = data$aa_summary,
                                          top_n      = 3,
                                          output_dir = NULL), 
                         PLOT_NAMES)
})

# =====| Plot Token Frequency Distribution |====================================

test_that("plot_token_frequency_distribution: correctly functions", {
  
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
})

# =====| Plot Top N Tokens |====================================================

test_that("plot_top_tokens: correctly functions", {
  
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
  testthat::expect_error(plot_top_tokens(statistics = c(1, 2, 3),
                                         top_n      = 3,
                                         output_dir = NULL),
                         STATS_ERROR)
  testthat::expect_error(plot_top_tokens(statistics = "test",
                                         top_n      = 3,
                                         output_dir = NULL),
                         STATS_ERROR)
  testthat::expect_error(plot_top_tokens(statistics = 123,
                                         top_n      = 3,
                                         output_dir = NULL),
                         STATS_ERROR)
  
  ## 2. Rejects Top N that is zero
  testthat::expect_error(plot_top_tokens(statistics = data$dna_summary,
                                         top_n      = 0,
                                         output_dir = NULL),
                         TOP_N_ERROR)
  testthat::expect_error(plot_top_tokens(statistics = data$rna_summary,
                                         top_n      = 0,
                                         output_dir = NULL),
                         TOP_N_ERROR)
  testthat::expect_error(plot_top_tokens(statistics = data$aa_summary,
                                         top_n      = 0,
                                         output_dir = NULL),
                         TOP_N_ERROR)
  
  ## 3. Rejects Top N that is less than zero
  testthat::expect_error(plot_top_tokens(statistics = data$dna_summary,
                                         top_n      = -3,
                                         output_dir = NULL),
                         TOP_N_ERROR)
  testthat::expect_error(plot_top_tokens(statistics = data$rna_summary,
                                         top_n      = -3,
                                         output_dir = NULL),
                         TOP_N_ERROR)
  testthat::expect_error(plot_top_tokens(statistics = data$aa_summary,
                                         top_n      = -3,
                                         output_dir = NULL),
                         TOP_N_ERROR)
  
})

# =====| Plot Cumulative Coverage |====================================

test_that("plot_cumulative_coverage: correctly functions", {
  
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
  testthat::expect_error(plot_cumulative_coverage(statistics = c(1, 2, 3),
                                                  output_dir = NULL), 
                         STATS_ERROR)
  testthat::expect_error(plot_cumulative_coverage(statistics = "test",
                                                  output_dir = NULL), 
                         STATS_ERROR)
  testthat::expect_error(plot_cumulative_coverage(statistics = 123,
                                                  output_dir = NULL), 
                         STATS_ERROR)
})

# [END]