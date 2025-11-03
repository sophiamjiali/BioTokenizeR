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
CLASS_ERROR <- "'seqs' must be a Biostrings::XStringSet"

# =====| Input Validation |=====================================================

test_that("preprocess_sequences: correctly validates inputs", {
  
  # Generate DNA, RNA, and AA sequences to test each alphabet
  dna_seq <- generate_sequences(type = "DNA", n = 1, length = 21)
  rna_seq <- generate_sequences(type = "RNA", n = 1, length = 21)
  aa_seq <- generate_sequences(type = "AA", n = 1, length = 21)
  
  dna_letters <- unlist(strsplit(as.character(dna_seq), split = ""))
  rna_letters <- unlist(strsplit(as.character(rna_seq), split = ""))
  aa_letters <- unlist(strsplit(as.character(aa_seq), split = ""))
  
  ## DNA Input Class Validation: rejects non-XStringSet
  testthat::expect_error(preprocess_sequences(seqs = dna_letters), CLASS_ERROR)
  
  ## RNA Input Class Validation: rejects non-XStringSet
  testthat::expect_error(preprocess_sequences(seqs = rna_letters), CLASS_ERROR)
  
  ## AA Input Class Validation: rejects non-XStringSet
  testthat::expect_error(preprocess_sequences(seqs = aa_letters), CLASS_ERROR)
})

# =====| Preprocessing Sequences |==========================================

test_that("preprocess_sequences: correctly preprocess single sequences", {
  
  # Generate individual DNA, RNA, and AA sequences and preprocess them
  dna_seq <- generate_sequences(type = "DNA", n = 1, length = 21)
  rna_seq <- generate_sequences(type = "RNA", n = 1, length = 21)
  aa_seq <- generate_sequences(type = "AA", n = 1, length = 21)
  
  dna_letters <- unlist(strsplit(as.character(dna_seq), split = ""))
  rna_letters <- unlist(strsplit(as.character(rna_seq), split = ""))
  aa_letters <- unlist(strsplit(as.character(aa_seq), split = ""))
  
  dna_result <- preprocess_sequences(seqs = dna_seq)
  rna_result <- preprocess_sequences(seqs = rna_seq)
  aa_result <- preprocess_sequences(seqs = aa_seq)
  
  ## 1. Basic Output Validation: type, length, columns
  testthat::expect_s3_class(dna_result, "bioBPE_preprocessed")
  testthat::expect_s3_class(rna_result, "bioBPE_preprocessed")
  testthat::expect_s3_class(aa_result, "bioBPE_preprocessed")
  
  testthat::expect_equal(length(dna_result$seqs), 1)
  testthat::expect_equal(length(rna_result$seqs), 1)
  testthat::expect_equal(length(aa_result$seqs), 1)
  
  testthat::expect_named(dna_result, PREPROC_COLUMNS)
  testthat::expect_named(rna_result, PREPROC_COLUMNS)
  testthat::expect_named(aa_result, PREPROC_COLUMNS)
  
  ## 2. All Non-canonical Characters Removed
  testthat::expect_true(all(dna_letters %in% Biostrings::DNA_ALPHABET))
  testthat::expect_true(all(rna_letters %in% Biostrings::RNA_ALPHABET))
  testthat::expect_true(all(aa_letters %in% Biostrings::AA_ALPHABET))
  
})
