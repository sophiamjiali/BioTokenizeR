# ==============================================================================
# Purpose:            Tests the annotate_sequences() function
# Author:             Sophia Li
# Date:               2025-11-02
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Built for integration with the usethis package
# ==============================================================================

library(BioTokenizeR)

# Define constants reused across tests: partial string matches
CLASS_ERROR  <- "'bioBPE_seqs' must be a"
TYPE_ERROR   <- "'bioBPE_seqs' is an unknown"
LENGTH_ERROR <- "'bioBPE_seqs' must contain at least"

ANNOT_COLUMNS   <- c("seqs", "type", "preproc_steps", "annot_steps")
DNA_ANNOT_STEPS <- c("length", "gc_content")
RNA_ANNOT_STEPS <- c("length", "gc_content")
AA_ANNOT_STEPS  <- c("length", "hydrophobic_fraction", "charged_fraction",
                     "polar_fraction", "composition_entropy")

# =====| Input Validation |=====================================================

test_that("annotate_sequences: correctly validates inputs", {
  
  # Generate DNA, RNA, and AA sequences, and dummy data
  data <- generate_data(
    n          = 1, 
    length     = 10000, 
    vocab_size = NULL,
    preprocess = TRUE, 
    annotate   = FALSE, 
    tokenize   = FALSE,
    summarize  = FALSE
  )
  dummy_data <- generate_dummy_data()

  ## 1. Rejects non-bioBPE_preprocessed class
  testthat::expect_error(annotate_sequences(bioBPE_seqs = data$dna_letters), 
                         CLASS_ERROR)
  testthat::expect_error(annotate_sequences(bioBPE_seqs = data$rna_letters), 
                         CLASS_ERROR)
  testthat::expect_error(annotate_sequences(bioBPE_seqs = data$aa_letters), 
                         CLASS_ERROR)
  
  ## 2. Rejects bioBPE_preprocessed sequences that have zero-length sequences
  testthat::expect_error(annotate_sequences(bioBPE_seqs = dummy_data$dna_empty), 
                         LENGTH_ERROR)
  testthat::expect_error(annotate_sequences(bioBPE_seqs = dummy_data$rna_empty), 
                         LENGTH_ERROR)
  testthat::expect_error(annotate_sequences(bioBPE_seqs = dummy_data$aa_empty), 
                         LENGTH_ERROR)
  
  ## 3. Rejects bioBPE_preprocessed sequences of an unknown sequence type
  testthat::expect_error(
    annotate_sequences(bioBPE_seqs = dummy_data$invalid_type), TYPE_ERROR
  )
  
})

# =====| Sequence Annotation |==================================================

test_that("annotate_sequences: correctly annotates single sequences", {
  
  # Generate DNA, RNA, and AA sequences
  data <- generate_data(
    n          = 1, 
    length     = 10000, 
    vocab_size = NULL,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = FALSE,
    summarize  = FALSE
  )
  
  ## 1. Annotated object type returned correctly
  testthat::expect_s3_class(data$dna_preproc, "bioBPE_preprocessed")
  testthat::expect_s3_class(data$rna_preproc, "bioBPE_preprocessed")
  testthat::expect_s3_class(data$aa_preproc, "bioBPE_preprocessed")
  
  ## 2. Sequence number returned correctly
  testthat::expect_equal(length(data$dna_preproc$seqs), 1)
  testthat::expect_equal(length(data$rna_preproc$seqs), 1)
  testthat::expect_equal(length(data$aa_preproc$seqs), 1)
  
  ## 3. Object columns returned and named correctly
  testthat::expect_named(data$dna_annot, ANNOT_COLUMNS)
  testthat::expect_named(data$rna_annot, ANNOT_COLUMNS)
  testthat::expect_named(data$aa_annot, ANNOT_COLUMNS)
  
  ## 4. Annotation steps returned correctly
  testthat::expect_equal(data$dna_annot$annot_steps, DNA_ANNOT_STEPS)
  testthat::expect_equal(data$rna_annot$annot_steps, RNA_ANNOT_STEPS)
  testthat::expect_equal(data$aa_annot$annot_steps, AA_ANNOT_STEPS)
})


test_that("annotate_sequences: correctly annotates multiple sequences", {
  
  # Generate DNA, RNA, and AA sequences
  data <- generate_data(
    n          = 3, 
    length     = 10000, 
    vocab_size = NULL,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = FALSE,
    summarize  = FALSE
  )
  
  ## 1. Annotated object type returned correctly
  testthat::expect_s3_class(data$dna_preproc, "bioBPE_preprocessed")
  testthat::expect_s3_class(data$rna_preproc, "bioBPE_preprocessed")
  testthat::expect_s3_class(data$aa_preproc, "bioBPE_preprocessed")
  
  ## 2. Sequence number returned correctly
  testthat::expect_equal(length(data$dna_preproc$seqs), 3)
  testthat::expect_equal(length(data$rna_preproc$seqs), 3)
  testthat::expect_equal(length(data$aa_preproc$seqs), 3)
  
  ## 3. Object columns returned and named correctly
  testthat::expect_named(data$dna_annot, ANNOT_COLUMNS)
  testthat::expect_named(data$rna_annot, ANNOT_COLUMNS)
  testthat::expect_named(data$aa_annot, ANNOT_COLUMNS)
  
  ## 4. Annotation steps returned correctly
  testthat::expect_equal(data$dna_annot$annot_steps, DNA_ANNOT_STEPS)
  testthat::expect_equal(data$rna_annot$annot_steps, RNA_ANNOT_STEPS)
  testthat::expect_equal(data$aa_annot$annot_steps, AA_ANNOT_STEPS)
})
