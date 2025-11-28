# ==============================================================================
# Purpose:            Tests the preprocess_sequences() function
# Author:             Sophia Li
# Date:               2025-11-02
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Built for integration with the usethis package
# ==============================================================================

library(BioTokenizeR)

# Define constants reused across tests: partial string matching
PREPROC_COLUMNS <- c("seqs", "type", "preproc_steps", "annot_steps")
CLASS_ERROR     <- "'seqs' must be a Biostrings::XStringSet"

DNA_PREPROC_STEPS <- c("to_lower", "trim_N", "remove_ambiguous", "drop_empty")
RNA_PREPROC_STEPS <- c("to_lower", "trim_N", "remove_ambiguous", "drop_empty")
AA_PREPROC_STEPS  <- c("to_lower", "remove_non_canonical", "drop_empty")

# =====| Input Validation |=====================================================

test_that("preprocess_sequences: correctly validates inputs", {
  
  # Generate DNA, RNA, and AA sequences to test each alphabet
  data <- generate_data(
    n          = 1, 
    length     = 10000, 
    vocab_size = NULL,
    preprocess = FALSE, 
    annotate   = FALSE, 
    tokenize   = FALSE,
    summarize  = FALSE
  )
  
  ## 1. Rejects non-XStringSet class
  testthat::expect_error(preprocess_sequences(seqs = data$dna_letters), 
                         CLASS_ERROR)
  testthat::expect_error(preprocess_sequences(seqs = data$rna_letters), 
                         CLASS_ERROR)
  testthat::expect_error(preprocess_sequences(seqs = data$aa_letters), 
                         CLASS_ERROR)
})

# =====| Preprocessing Sequences |==============================================

test_that("preprocess_sequences: correctly preprocess single sequences", {
  
  # Generate individual DNA, RNA, and AA sequences and preprocess them
  data <- generate_data(
    n          = 1, 
    length     = 10000, 
    vocab_size = NULL,
    preprocess = TRUE, 
    annotate   = FALSE, 
    tokenize   = FALSE,
    summarize  = FALSE
  )
  
  ## 1. Preprocessed object type returned correctly
  testthat::expect_s3_class(data$dna_preproc, "bioBPE_preprocessed")
  testthat::expect_s3_class(data$rna_preproc, "bioBPE_preprocessed")
  testthat::expect_s3_class(data$aa_preproc, "bioBPE_preprocessed")
  
  ## 2. Sequence number returned correctly
  testthat::expect_equal(length(data$dna_preproc$seqs), 1)
  testthat::expect_equal(length(data$rna_preproc$seqs), 1)
  testthat::expect_equal(length(data$aa_preproc$seqs), 1)
  
  ## 3. Object columns returned and named correctly
  testthat::expect_named(data$dna_preproc, PREPROC_COLUMNS)
  testthat::expect_named(data$rna_preproc, PREPROC_COLUMNS)
  testthat::expect_named(data$aa_preproc, PREPROC_COLUMNS)
  
  ## 4. All Non-canonical Characters Removed
  testthat::expect_true(all(data$dna_letters %in% Biostrings::DNA_ALPHABET))
  testthat::expect_true(all(data$rna_letters %in% Biostrings::RNA_ALPHABET))
  testthat::expect_true(all(data$aa_letters %in% Biostrings::AA_ALPHABET))
  
  ## 5. Annotation steps returned correctly
  testthat::expect_equal(data$dna_preproc$preproc_steps, DNA_PREPROC_STEPS)
  testthat::expect_equal(data$rna_preproc$preproc_steps, RNA_PREPROC_STEPS)
  testthat::expect_equal(data$aa_preproc$preproc_steps, AA_PREPROC_STEPS)
})


test_that("preprocess_sequences: correctly preprocess multiple sequences", {
  
  # Generate multiple DNA, RNA, and AA sequences and preprocess them
  data <- generate_data(
    n          = 3, 
    length     = 10000, 
    vocab_size = NULL,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = FALSE,
    summarize  = FALSE
  )
  
  ## 1. Preprocessed object type returned correctly
  testthat::expect_s3_class(data$dna_preproc, "bioBPE_preprocessed")
  testthat::expect_s3_class(data$rna_preproc, "bioBPE_preprocessed")
  testthat::expect_s3_class(data$aa_preproc, "bioBPE_preprocessed")
  
  ## 2. Sequence number returned correctly
  testthat::expect_equal(length(data$dna_preproc$seqs), 3)
  testthat::expect_equal(length(data$rna_preproc$seqs), 3)
  testthat::expect_equal(length(data$aa_preproc$seqs), 3)
  
  ## 3. Object columns returned and named correctly
  testthat::expect_named(data$dna_preproc, PREPROC_COLUMNS)
  testthat::expect_named(data$rna_preproc, PREPROC_COLUMNS)
  testthat::expect_named(data$aa_preproc, PREPROC_COLUMNS)
  
  ## 4. All Non-canonical Characters Removed
  testthat::expect_true(all(data$dna_letters %in% Biostrings::DNA_ALPHABET))
  testthat::expect_true(all(data$rna_letters %in% Biostrings::RNA_ALPHABET))
  testthat::expect_true(all(data$aa_letters %in% Biostrings::AA_ALPHABET))
  
  ## 5. Annotation steps returned correctly
  testthat::expect_equal(data$dna_preproc$preproc_steps, DNA_PREPROC_STEPS)
  testthat::expect_equal(data$rna_preproc$preproc_steps, RNA_PREPROC_STEPS)
  testthat::expect_equal(data$aa_preproc$preproc_steps, AA_PREPROC_STEPS)
})
