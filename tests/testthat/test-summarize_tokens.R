# ==============================================================================
# Purpose:            Tests the summarize_tokens() function
# Author:             Sophia Li
# Date:               2025-11-03
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Built for integration with the usethis package
# ==============================================================================

library(BioTokenizeR)

# Define constants reused across tests: partial string matches
TYPE_ERROR <- "'tokens' must be a non-empty"
ELEMENT_ERROR <- "All elements of 'tokens'"

SUMMARY_COLUMNS <- c("corpus", "token_summary", "token_length_summary")
CORPUS_COLUMNS <- c("num_sequences", "total_tokens", "avg_seq_length", 
                    "median_seq_length", "vocab_size")
TOKEN_SUMMARY_COLUMNS <- c("token", "frequency", "length", "gc_like")

# =====| Input Validation |=====================================================

test_that("summarize_tokens: correctly validates inputs", {
  
  # Generate DNA, RNA, and AA sequences to test each alphabet
  data <- generate_data(
    n          = 1, 
    length     = 100, 
    vocab_size = 5,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = TRUE,
    summarize  = FALSE
  )
  
  ## 1. Rejects empty or non-list inputs
  testthat::expect_error(summarize_tokens(tokens = c(), TYPE_ERROR))
  testthat::expect_error(summarize_tokens(tokens = "ACGT", TYPE_ERROR))
  
  ## 2. Rejects non-character tokens
  testthat::expect_error(summarize_tokens(tokens = c("ACT", 3)), TYPE_ERROR)
  testthat::expect_error(summarize_tokens(tokens = c(1, 2, 3)), TYPE_ERROR)
})

# =====| Sequence Annotation |==================================================

test_that("summarize_tokens: correctly summarizes single sequences", {
  
  # Generate DNA, RNA, and AA sequences to test each alphabet
  data <- generate_data(
    n          = 1, 
    length     = 100, 
    vocab_size = 5,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = TRUE,
    summarize  = TRUE
  )
  
  ## 1. Summary object type returned correctly
  testthat::expect_s3_class(data$dna_summary, "bioBPE_summary")
  testthat::expect_s3_class(data$rna_summary, "bioBPE_summary")
  testthat::expect_s3_class(data$aa_summary, "bioBPE_summary")
  
  ## 2. Object columns returned and named correctly
  testthat::expect_named(data$dna_summary, SUMMARY_COLUMNS)
  testthat::expect_named(data$rna_summary, SUMMARY_COLUMNS)
  testthat::expect_named(data$aa_summary, SUMMARY_COLUMNS)
  
  testthat::expect_named(data$dna_summary$corpus, CORPUS_COLUMNS)
  testthat::expect_named(data$rna_summary$corpus, CORPUS_COLUMNS)
  testthat::expect_named(data$aa_summary$corpus, CORPUS_COLUMNS)
  
  testthat::expect_named(data$dna_summary$token_summary, TOKEN_SUMMARY_COLUMNS)
  testthat::expect_named(data$rna_summary$token_summary, TOKEN_SUMMARY_COLUMNS)
  testthat::expect_named(data$aa_summary$token_summary, TOKEN_SUMMARY_COLUMNS)
})

test_that("summarize_tokens: correctly summarizes multiple sequences", {
  
  # Generate DNA, RNA, and AA sequences to test each alphabet
  data <- generate_data(
    n          = 3, 
    length     = 100, 
    vocab_size = 5,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = TRUE,
    summarize  = TRUE
  )
  
  ## 1. Summary object type returned correctly
  testthat::expect_s3_class(data$dna_summary, "bioBPE_summary")
  testthat::expect_s3_class(data$rna_summary, "bioBPE_summary")
  testthat::expect_s3_class(data$aa_summary, "bioBPE_summary")
  
  ## 2. Object columns returned and named correctly
  testthat::expect_named(data$dna_summary, SUMMARY_COLUMNS)
  testthat::expect_named(data$rna_summary, SUMMARY_COLUMNS)
  testthat::expect_named(data$aa_summary, SUMMARY_COLUMNS)
  
  testthat::expect_named(data$dna_summary$corpus, CORPUS_COLUMNS)
  testthat::expect_named(data$rna_summary$corpus, CORPUS_COLUMNS)
  testthat::expect_named(data$aa_summary$corpus, CORPUS_COLUMNS)
  
  testthat::expect_named(data$dna_summary$token_summary, TOKEN_SUMMARY_COLUMNS)
  testthat::expect_named(data$rna_summary$token_summary, TOKEN_SUMMARY_COLUMNS)
  testthat::expect_named(data$aa_summary$token_summary, TOKEN_SUMMARY_COLUMNS)
})