# ==============================================================================
# Purpose:            Tests the tokenize_sequences() function
# Author:             Sophia Li
# Date:               2025-11-02
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Will take a bit longer to run due to iterating through
#                     sequences for some tests
# ==============================================================================

library(BioTokenizeR)

# Define constants reused across tests: partial string matches
CLASS_ERROR <- "'bioBPE_seqs' must be a"
VOCAB_ERROR <- "'vocab_size' must be greater"
LENGTH_ERROR <- "'bioBPE_seqs' must contain at least"

TOKENIZATION_COLUMNS <- c("vocab", "tokens")
VOCAB_COLUMNS <- c("vocab", "merges", "bio_scores")


# =====| Input Validation |=====================================================

test_that("tokenize_sequences: correctly validates inputs", {
  
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
  dummy_data <- generate_dummy_data()
  
  ## 1. Rejects non-bioBPE_preprocessed class
  testthat::expect_error(
    tokenize_sequences(bioBPE_seqs = data$dna_letters, vocab_size = 15), 
    CLASS_ERROR
  )
  testthat::expect_error(
    tokenize_sequences(bioBPE_seqs = data$rna_letters, vocab_size = 15), 
    CLASS_ERROR
  )
  testthat::expect_error(
    tokenize_sequences(bioBPE_seqs = data$aa_letters, vocab_size = 15), 
    CLASS_ERROR
  )
  
  ## 2. Rejects bioBPE_preprocessed sequences that have zero-length sequences
  testthat::expect_error(
    tokenize_sequences(bioBPE_seqs = dummy_data$dna_empty, vocab_size = 15), 
    LENGTH_ERROR
  )
  testthat::expect_error(
    tokenize_sequences(bioBPE_seqs = dummy_data$rna_empty, vocab_size = 15), 
    LENGTH_ERROR
  )
  testthat::expect_error(
    tokenize_sequences(bioBPE_seqs = dummy_data$aa_empty, vocab_size = 15), 
    LENGTH_ERROR
  )
  
  ## 3. Rejects vocabulary sizes less than or equal to zero
  testthat::expect_error(
    tokenize_sequences(bioBPE_seqs = data$dna_preproc, vocab_size = 0), 
    VOCAB_ERROR
  )
  testthat::expect_error(
    tokenize_sequences(bioBPE_seqs = data$dna_preproc, vocab_size = -1), 
    VOCAB_ERROR
  )
})

# =====| Sequence Tokenization |================================================

test_that("tokenize_sequences: correctly tokenizes single sequences", {
  
  # Generate DNA, RNA, and AA sequences
  data <- generate_data(
    n          = 1, 
    length     = 10000, 
    vocab_size = 15,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = TRUE,
    summarize  = FALSE
  )
  
  ## 1. Annotated object type returned correctly
  testthat::expect_type(data$dna_tokens, "list")
  testthat::expect_type(data$rna_tokens, "list")
  testthat::expect_type(data$aa_tokens, "list")
  
  ## 2. Object columns returned and named correctly
  testthat::expect_named(data$dna_tokens, TOKENIZATION_COLUMNS)
  testthat::expect_named(data$rna_tokens, TOKENIZATION_COLUMNS)
  testthat::expect_named(data$aa_tokens, TOKENIZATION_COLUMNS)
  
  ## 3. Vocabulary columns returned and named correctly
  testthat::expect_named(data$dna_tokens$vocab, VOCAB_COLUMNS)
  testthat::expect_named(data$rna_tokens$vocab, VOCAB_COLUMNS)
  testthat::expect_named(data$aa_tokens$vocab, VOCAB_COLUMNS)
  
  ## 4. Vocabulary size aligns with selected value
  testthat::expect_equal(length(data$dna_tokens$vocab$vocab), 15)
  testthat::expect_equal(length(data$rna_tokens$vocab$vocab), 15)
  testthat::expect_equal(length(data$aa_tokens$vocab$vocab), 20)
  
  ## 5. Merge size aligns with vocabulary size and starting population
  testthat::expect_equal(length(data$dna_tokens$vocab$merges), 15 - 4)
  testthat::expect_equal(length(data$rna_tokens$vocab$merges), 15 - 4)
  testthat::expect_equal(length(data$aa_tokens$vocab$merges), 0)
  
  ## 6. Tokenized sequences properly returned
  testthat::expect_true(all(length(data$dna_tokens$tokens) > 0))
  testthat::expect_true(all(length(data$rna_tokens$tokens) > 0))
  testthat::expect_true(all(length(data$aa_tokens$tokens) > 0))
  
  ## 7. Ensure all vocabulary are biologically plausible
  dna_letters <- unique(unlist(strsplit(unlist(data$dna_tokens$vocab$vocab), "")))
  rna_letters <- unique(unlist(strsplit(unlist(data$rna_tokens$vocab$vocab), "")))
  aa_letters <- unique(unlist(strsplit(unlist(data$aa_tokens$vocab$vocab), "")))
  testthat::expect_true(all(dna_letters %in% Biostrings::DNA_ALPHABET))
  testthat::expect_true(all(rna_letters %in% Biostrings::RNA_ALPHABET))
  testthat::expect_true(all(aa_letters %in% Biostrings::AA_ALPHABET))
  
  ## 8. Ensure all tokens are biologically plausible
  dna_letters <- unique(unlist(strsplit(unlist(data$dna_tokens$tokens), "")))
  rna_letters <- unique(unlist(strsplit(unlist(data$rna_tokens$tokens), "")))
  aa_letters <- unique(unlist(strsplit(unlist(data$aa_tokens$tokens), "")))
  testthat::expect_true(all(dna_letters %in% Biostrings::DNA_ALPHABET))
  testthat::expect_true(all(rna_letters %in% Biostrings::RNA_ALPHABET))
  testthat::expect_true(all(aa_letters %in% Biostrings::AA_ALPHABET))
})

test_that("tokenize_sequences: correctly tokenizes multiple sequences", {
  
  # Generate DNA, RNA, and AA sequences
  data <- generate_data(
    n          = 10, 
    length     = 10000, 
    vocab_size = 15,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = TRUE,
    summarize  = FALSE
  )
  
  ## 1. Annotated object type returned correctly
  testthat::expect_type(data$dna_tokens, "list")
  testthat::expect_type(data$rna_tokens, "list")
  testthat::expect_type(data$aa_tokens, "list")
  
  ## 2. Object columns returned and named correctly
  testthat::expect_named(data$dna_tokens, TOKENIZATION_COLUMNS)
  testthat::expect_named(data$rna_tokens, TOKENIZATION_COLUMNS)
  testthat::expect_named(data$aa_tokens, TOKENIZATION_COLUMNS)
  
  ## 3. Vocabulary columns returned and named correctly
  testthat::expect_named(data$dna_tokens$vocab, VOCAB_COLUMNS)
  testthat::expect_named(data$rna_tokens$vocab, VOCAB_COLUMNS)
  testthat::expect_named(data$aa_tokens$vocab, VOCAB_COLUMNS)
  
  ## 4. Vocabulary size aligns with selected value
  testthat::expect_equal(length(data$dna_tokens$vocab$vocab), 15)
  testthat::expect_equal(length(data$rna_tokens$vocab$vocab), 15)
  testthat::expect_equal(length(data$aa_tokens$vocab$vocab), 20)
  
  ## 5. Merge size aligns with vocabulary size and starting population
  testthat::expect_equal(length(data$dna_tokens$vocab$merges), 15 - 4)
  testthat::expect_equal(length(data$rna_tokens$vocab$merges), 15 - 4)
  testthat::expect_equal(length(data$aa_tokens$vocab$merges), 0)
  
  ## 6. Tokenized sequences properly returned
  testthat::expect_true(all(length(data$dna_tokens$tokens) > 0))
  testthat::expect_true(all(length(data$rna_tokens$tokens) > 0))
  testthat::expect_true(all(length(data$aa_tokens$tokens) > 0))
  
  ## 7. Ensure all vocabulary are biologically plausible
  dna_letters <- unique(unlist(strsplit(unlist(data$dna_tokens$vocab$vocab), "")))
  rna_letters <- unique(unlist(strsplit(unlist(data$rna_tokens$vocab$vocab), "")))
  aa_letters <- unique(unlist(strsplit(unlist(data$aa_tokens$vocab$vocab), "")))
  testthat::expect_true(all(dna_letters %in% Biostrings::DNA_ALPHABET))
  testthat::expect_true(all(rna_letters %in% Biostrings::RNA_ALPHABET))
  testthat::expect_true(all(aa_letters %in% Biostrings::AA_ALPHABET))
  
  ## 8. Ensure all tokens are biologically plausible
  dna_letters <- unique(unlist(strsplit(unlist(data$dna_tokens$tokens), "")))
  rna_letters <- unique(unlist(strsplit(unlist(data$rna_tokens$tokens), "")))
  aa_letters <- unique(unlist(strsplit(unlist(data$aa_tokens$tokens), "")))
  testthat::expect_true(all(dna_letters %in% Biostrings::DNA_ALPHABET))
  testthat::expect_true(all(rna_letters %in% Biostrings::RNA_ALPHABET))
  testthat::expect_true(all(aa_letters %in% Biostrings::AA_ALPHABET))
})

test_that("tokenize_sequences: correctly tokenizes with high vocabulary size", {
  
  # Generate DNA, RNA, and AA sequences
  data <- generate_data(
    n          = 10, 
    length     = 10000, 
    vocab_size = 30,
    preprocess = TRUE, 
    annotate   = TRUE, 
    tokenize   = TRUE,
    summarize  = FALSE
  )
  
  ## 1. Annotated object type returned correctly
  testthat::expect_type(data$dna_tokens, "list")
  testthat::expect_type(data$rna_tokens, "list")
  testthat::expect_type(data$aa_tokens, "list")
  
  ## 2. Object columns returned and named correctly
  testthat::expect_named(data$dna_tokens, TOKENIZATION_COLUMNS)
  testthat::expect_named(data$rna_tokens, TOKENIZATION_COLUMNS)
  testthat::expect_named(data$aa_tokens, TOKENIZATION_COLUMNS)
  
  ## 3. Vocabulary columns returned and named correctly
  testthat::expect_named(data$dna_tokens$vocab, VOCAB_COLUMNS)
  testthat::expect_named(data$rna_tokens$vocab, VOCAB_COLUMNS)
  testthat::expect_named(data$aa_tokens$vocab, VOCAB_COLUMNS)
  
  ## 4. Vocabulary size aligns with selected value
  testthat::expect_equal(length(data$dna_tokens$vocab$vocab), 30)
  testthat::expect_equal(length(data$rna_tokens$vocab$vocab), 30)
  testthat::expect_equal(length(data$aa_tokens$vocab$vocab), 30)
  
  ## 5. Merge size aligns with vocabulary size and starting population
  testthat::expect_equal(length(data$dna_tokens$vocab$merges), 30 - 4)
  testthat::expect_equal(length(data$rna_tokens$vocab$merges), 30 - 4)
  testthat::expect_equal(length(data$aa_tokens$vocab$merges), 30 - 20)
  
  ## 6. Tokenized sequences properly returned
  testthat::expect_true(all(length(data$dna_tokens$tokens) > 0))
  testthat::expect_true(all(length(data$rna_tokens$tokens) > 0))
  testthat::expect_true(all(length(data$aa_tokens$tokens) > 0))
  
  ## 7. Ensure all vocabulary are biologically plausible
  dna_letters <- unique(unlist(strsplit(unlist(data$dna_tokens$vocab$vocab), "")))
  rna_letters <- unique(unlist(strsplit(unlist(data$rna_tokens$vocab$vocab), "")))
  aa_letters <- unique(unlist(strsplit(unlist(data$aa_tokens$vocab$vocab), "")))
  testthat::expect_true(all(dna_letters %in% Biostrings::DNA_ALPHABET))
  testthat::expect_true(all(rna_letters %in% Biostrings::RNA_ALPHABET))
  testthat::expect_true(all(aa_letters %in% Biostrings::AA_ALPHABET))
  
  ## 8. Ensure all tokens are biologically plausible
  dna_letters <- unique(unlist(strsplit(unlist(data$dna_tokens$tokens), "")))
  rna_letters <- unique(unlist(strsplit(unlist(data$rna_tokens$tokens), "")))
  aa_letters <- unique(unlist(strsplit(unlist(data$aa_tokens$tokens), "")))
  testthat::expect_true(all(dna_letters %in% Biostrings::DNA_ALPHABET))
  testthat::expect_true(all(rna_letters %in% Biostrings::RNA_ALPHABET))
  testthat::expect_true(all(aa_letters %in% Biostrings::AA_ALPHABET))
})
