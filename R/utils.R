# ==============================================================================
# Purpose:            Defines helper functions and utilities used by R/
# Author:             Sophia Li
# Date:               2025-11-26
# Version:            1.0
# Bugs and Issues:    N/A
# ==============================================================================

# =====| Global Constants |=====================================================

# Declare NSE variables to avoid R CMD check notes
utils::globalVariables(c("frequency", "cumulative", "freq", "token"))

# Define canonical nucleotides for each biological sequence type
CANONICAL_DNA <- c("A", "C", "G", "T")
CANONICAL_RNA <- c("A", "C", "G", "U")
CANONICAL_AA  <- c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N",
                   "P", "Q", "R", "S", "T", "V", "W", "Y")

# =====| Data Generation Wrapper |==============================================

#' Generate Sequencing Data
#' 
#' Wraps the generation of DNA, RNA, and AA sequences at different states of the 
#' pipeline. Parameters and pipeline steps are toggled. For use by 
#' documentation, testthat, and various tests.
#' 
#' @param n A numerical value indicating the number of sequences to generate.
#' @param length A numerical value indicating the length of all sequences.
#' @param vocab_size A numerical value indicating the BPE tokenization 
#'    vocabulary size for use by `tokenize_sequences()`
#' @param preprocess A Boolean value toggling the preprocessing step.
#' @param annotate A Boolean value toggling the annotation step.
#' @param tokenize A Boolean value toggling the tokenization step.
#' @param summarize A Boolean value toggling the analysis step.
#' @param verbose A Boolean value toggling verbose output during generation.
#' 
#' @return A list containing:
#'    \describe{
#'        \item{`dna_seq`}{A `Biostrings::DNAStringSet` of generated sequences.}
#'        \item{`rna_seq`}{A `Biostrings::RNAStringSet` of generated sequences.}
#'        \item{`aa_seq`}{A `Biostrings::AAStringSet` of generated sequences.}
#'        \item{`dna_letters`}{A string of all letters from `dna_seq`.}
#'        \item{`rna_letters`}{A string of all letters from `rna_seq`.}
#'        \item{`aa_letters`}{A string of all letters from `ana_seq`.}
#'        \item{`dna_preproc`}{A `bioBPE_preprocessed` object of preprocessed 
#'            DNA sequences.}
#'        \item{`rna_preproc`}{A `bioBPE_preprocessed` object of preprocessed
#'            RNA sequences.}
#'        \item{`aa_preproc`}{A `bioBPE_preprocessed` object of preprocessed
#'            AA sequences.}
#'        \item{`dna_annot`}{A `bioBPE_preprocessed` object of preprocessed DNA 
#'            sequences with biological annotations.}
#'        \item{`rna_annot`}{A `bioBPE_preprocessed` object of preprocessed RNA 
#'            sequences with biological annotations.}
#'        \item{`aa_annot`}{A `bioBPE_preprocessed` object of preprocessed AA 
#'            sequences with biological annotations.}
#'        \item{`dna_tokens`}{A list of tokenized DNA sequences}
#'        \item{`rna_tokens`}{A list of tokenized RNA sequences}
#'        \item{`aa_tokens`}{A list of tokenized AA sequences}
#'        \item{`dna_summary`}{A `bioBPE_summary` object summarizing DNA tokens.}
#'        \item{`rna_summary`}{A `bioBPE_summary` object summarizing RNA tokens.}
#'        \item{`aa_summary`}{A `bioBPE_summary` object summarizing AA tokens.}
#'    }
#' 
#' @examples
#' \dontrun{
#'    # Generate simulated data
#'    data <- generate_data(
#'        n          = 3, 
#'        length     = 1000, 
#'        vocab_size = 25, 
#'        preprocess = FALSE,
#'        annotate   = FALSE,
#'        tokenize   = FALSE,
#'        summarize  = FALSE,
#'        verbose    = FALSE
#'    )
#' }
#' 
#' @keywords internal
generate_data <- function(n          = 1, 
                          length     = 21,
                          vocab_size = 10,
                          preprocess = TRUE,
                          annotate   = TRUE,
                          tokenize   = TRUE,
                          summarize  = TRUE,
                          verbose    = FALSE) {
  
  # Generate individual DNA, RNA, and AA sequences and preprocess them
  dna_seq <- generate_sequences(type = "DNA", n = n, length = length)
  rna_seq <- generate_sequences(type = "RNA", n = n, length = length)
  aa_seq  <- generate_sequences(type = "AA", n = n, length = length)
  
  if (verbose) cat("Generated", n * 3, "sequences of length", length, "\n")
  
  # Fetch the letters of all sequence
  dna_letters <- unlist(strsplit(as.character(dna_seq), split = ""))
  rna_letters <- unlist(strsplit(as.character(rna_seq), split = ""))
  aa_letters <- unlist(strsplit(as.character(aa_seq), split = ""))
  
  # Preprocess the sequences
  if (preprocess == TRUE) {
    dna_preproc <- preprocess_sequences(seqs = dna_seq)
    rna_preproc <- preprocess_sequences(seqs = rna_seq)
    aa_preproc <- preprocess_sequences(seqs = aa_seq)
    if (verbose) cat("Preprocessed", n * 3, "sequences\n")
    
  } else {
    dna_preproc <- NULL
    rna_preproc <- NULL
    aa_preproc <- NULL
  }
  
  # Annotate the sequences
  if (all(c(preprocess, annotate))) {
    dna_annot <- annotate_sequences(bioBPE_seqs = dna_preproc)
    rna_annot <- annotate_sequences(bioBPE_seqs = rna_preproc)
    aa_annot <- annotate_sequences(bioBPE_seqs = aa_preproc)
    if (verbose) cat("Annotated", n * 3, "sequences\n")
    
  } else {
    dna_annot <- NULL
    rna_annot <- NULL
    aa_annot <- NULL
  }
  
  # Tokenize the sequences
  if (all(c(preprocess, annotate, tokenize))) {
    dna_tokens <- tokenize_sequences(bioBPE_seqs = dna_annot, 
                                     vocab_size  = vocab_size)
    if (verbose) cat("Tokenized", n, "DNA sequences\n")
    
    rna_tokens <- tokenize_sequences(bioBPE_seqs = rna_annot,
                                     vocab_size  = vocab_size)
    if (verbose) cat("Tokenized", n, "RNA sequences\n")
    
    aa_tokens <- tokenize_sequences(bioBPE_seqs = aa_annot,
                                    vocab_size  = vocab_size)
    if (verbose) cat("Tokenized", n, "ANA sequences\n")
    
  } else {
    dna_tokens <- NULL
    rna_tokens <- NULL
    aa_tokens <- NULL
  }
  
  # Summarize the tokenized sequences
  if (all(c(preprocess, annotate, tokenize, summarize))) {
    dna_summary <- summarize_tokens(tokens = dna_tokens$tokens)
    rna_summary <- summarize_tokens(tokens = rna_tokens$tokens)
    aa_summary  <- summarize_tokens(tokens = aa_tokens$tokens)
    
    if (verbose) cat("Summarized", n * 3, "sequences\n")
    
  } else {
    dna_summary <- NULL
    rna_summary <- NULL
    aa_summary <- NULL
  }
  
  # Initialize a comprehensive return object
  result <- list(
    
    # Define raw generated sequences and their letters per type
    dna_seq     = dna_seq,     rna_seq = rna_seq,         aa_seq = aa_seq,
    dna_letters = dna_letters, rna_letters = rna_letters, aa_letters = aa_letters,
    
    # Define intermediate results from each step of the pipeline
    dna_preproc = dna_preproc, rna_preproc = rna_preproc, aa_preproc = aa_preproc,
    dna_annot   = dna_annot,   rna_annot = rna_annot,     aa_annot = aa_annot,
    dna_tokens  = dna_tokens,  rna_tokens  = rna_tokens,  aa_tokens   = aa_tokens,
    dna_summary = dna_summary, rna_summary = rna_summary, aa_summary  = aa_summary
  )
  
  return (result)
}


#' Generate Dummy Data
#' 
#' Wraps the generation of the primary types of invalid sequencing data inputted
#' to the workflow for standardized error checking.
#' 
#' @return A list containing:
#'    \describe{
#'        \item{`dna_empty`}{An empty `bioBPE_preprocessed` object.}
#'        \item{`rna_empty`}{An empty `bioBPE_preprocessed` object.}
#'        \item{`aa_empty`}{An empty `bioBPE_preprocessed` object.}
#'        \item{`invalid_type`}{A `bioBPE_preprocessed` object of invalid type.}
#'    }
#' 
#' @examples
#' \dontrun{
#'    # Generate simulated dummy data
#'    dummy_data <- generate_dummy_data()
#' }
#' 
#' @keywords internal
#' @importFrom Biostrings DNAStringSet RNAStringSet AAStringSet
generate_dummy_data <- function() {
  
  # Initialize dummy data with empty sequence sets
  dna_empty <- structure(list(seqs  = Biostrings::DNAStringSet()), 
                              class = "bioBPE_preprocessed")
  rna_empty <- structure(list(seqs  = Biostrings::RNAStringSet()), 
                              class = "bioBPE_preprocessed")
  aa_empty  <- structure(list(seqs  = Biostrings::AAStringSet()), 
                              class = "bioBPE_preprocessed")
  
  # Initialize dummy data with incorrect classes
  invalid_type <- structure(list(
    seqs = generate_sequences(type = "DNA", n = 1, length = 21),
    type = "Lipid"
  ), class = "bioBPE_preprocessed"
  )
  
  # Return the dummy data as a list
  dummy_data <- list(
    dna_empty    = dna_empty,
    rna_empty    = rna_empty,
    aa_empty     = aa_empty,
    invalid_type = invalid_type
  )
  
  return (dummy_data)
}

# =====| Sequence Generation |==================================================

#' Generate Biological Sequences
#' 
#' Wraps the generation of the primary types of invalid sequencing data inputted
#' to the workflow for standardized error checking.
#' 
#' @return A list containing:
#'    \describe{
#'        \item{`dna_empty`}{An empty `bioBPE_preprocessed` object.}
#'        \item{`rna_empty`}{An empty `bioBPE_preprocessed` object.}
#'        \item{`aa_empty`}{An empty `bioBPE_preprocessed` object.}
#'        \item{`invalid_type`}{A `bioBPE_preprocessed` object of invalid type.}
#'    }
#' 
#' @examples
#' \dontrun{
#'    # Generate simulated dummy data
#'    dummy_data <- generate_dummy_data()
#' }
#' 
#' @keywords internal
#' @importFrom Biostrings DNAStringSet RNAStringSet AAStringSet
generate_sequences <- function(type = NULL, n = 1, length = 21) {
  
  # Generate a set of n sequences of the given type
  if (type == "DNA") {
    seqs <- Biostrings::DNAStringSet(replicate(n = n, expr = paste0(
      sample(x = CANONICAL_DNA, size = length, replace = TRUE), 
      collapse = ""
    )))
    
  } else if (type == "RNA") {
    seqs <- Biostrings::RNAStringSet(replicate(n = n, expr = paste0(
      sample(x = CANONICAL_RNA, size = length, replace = TRUE), 
      collapse = ""
    )))
    
  } else if (type == "AA") {
    seqs <- Biostrings::AAStringSet(replicate(n = n, expr = paste0(
      sample(x = CANONICAL_AA, size = length, replace = TRUE), 
      collapse = ""
    )))
    
  } else {
    stop("'type' must be DNA, RNA, or AA.")
  }
  
  return (seqs)
}