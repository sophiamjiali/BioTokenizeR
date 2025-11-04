# ==============================================================================
# Purpose:            Helper functions to generate data for testing
# Author:             Sophia Li
# Date:               2025-11-02
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Built for integration with the usethis package
# ==============================================================================

# =====| Global Constants |=====================================================

# Define canonical nucleotides for each biological sequence type
CANONICAL_DNA <- c("A", "C", "G", "T")
CANONICAL_RNA <- c("A", "C", "G", "U")
CANONICAL_AA  <- c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N",
                   "P", "Q", "R", "S", "T", "V", "W", "Y")

# =====| Data Generation Wrapper |==============================================

generate_data <- function(n          = 1, 
                          length     = 21,
                          vocab_size = 10,
                          preprocess = TRUE,
                          annotate   = TRUE,
                          tokenize   = TRUE,
                          summarize  = TRUE) {
  
  # Generate individual DNA, RNA, and AA sequences and preprocess them
  dna_seq <- generate_sequences(type = "DNA", n = n, length = length)
  rna_seq <- generate_sequences(type = "RNA", n = n, length = length)
  aa_seq <- generate_sequences(type = "AA", n = n, length = length)
  
  # Fetch the letters of all sequence
  dna_letters <- unlist(strsplit(as.character(dna_seq), split = ""))
  rna_letters <- unlist(strsplit(as.character(rna_seq), split = ""))
  aa_letters <- unlist(strsplit(as.character(aa_seq), split = ""))
  
  # Preprocess the sequences
  if (preprocess == TRUE) {
    dna_preproc <- preprocess_sequences(seqs = dna_seq)
    rna_preproc <- preprocess_sequences(seqs = rna_seq)
    aa_preproc <- preprocess_sequences(seqs = aa_seq)
    
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
    
  } else {
    dna_annot <- NULL
    rna_annot <- NULL
    aa_annot <- NULL
  }
  
  # Tokenize the sequences
  if (all(c(preprocess, annotate, tokenize))) {
    dna_tokens <- tokenize_sequences(bioBPE_seqs = dna_annot, 
                                     vocab_size = vocab_size)
    rna_tokens <- tokenize_sequences(bioBPE_seqs = rna_annot,
                                     vocab_size = vocab_size)
    aa_tokens <- tokenize_sequences(bioBPE_seqs = aa_annot,
                                    vocab_size = vocab_size)
  } else {
    dna_tokens <- NULL
    rna_tokens <- NULL
    aa_tokens <- NULL
  }
  
  # Summarize the tokenized sequences
  if (all(c(preprocess, annotate, tokenize, summarize))) {
    dna_summary <- summarize_tokens(tokens = dna_tokens$tokens)
    rna_summary <- summarize_tokens(tokens = rna_tokens$tokens)
    aa_summary <- summarize_tokens(tokens = aa_tokens$tokens)
    
  } else {
    dna_summary <- NULL
    rna_summary <- NULL
    aa_summary <- NULL
  }
  
  # Initialize a comprehensive return object
  result <- list(
    dna_seq = dna_seq,
    rna_seq = rna_seq,
    aa_seq = aa_seq,
    
    dna_letters = dna_letters,
    rna_letters = rna_letters,
    aa_letters = aa_letters,
    
    dna_preproc = dna_preproc,
    rna_preproc = rna_preproc,
    aa_preproc = aa_preproc,
    
    dna_annot = dna_annot,
    rna_annot = rna_annot,
    aa_annot = aa_annot,
    
    dna_tokens = dna_tokens,
    rna_tokens = rna_tokens,
    aa_tokens = aa_tokens,
    
    dna_summary = dna_summary,
    rna_summary = rna_summary,
    aa_summary = aa_summary
  )
  
  return (result)
}


generate_dummy_data <- function() {
  
  # Initialize dummy data with empty sequence sets
  dna_empty <- structure(list(seqs = DNAStringSet()), 
                         class = "bioBPE_preprocessed")
  rna_empty <- structure(list(seqs = RNAStringSet()), 
                         class = "bioBPE_preprocessed")
  aa_empty <- structure(list(seqs = AAStringSet()), 
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

generate_sequences <- function(type = NULL, n = 1, length = 21) {
  
  # Generate a set of n sequences of the given type
  if (type == "DNA") {
    seqs <- DNAStringSet(replicate(n = n, expr = paste0(
          sample(x = CANONICAL_DNA, size = length, replace = TRUE), 
          collapse = ""
    )))
    
  } else if (type == "RNA") {
    seqs <- RNAStringSet(replicate(n = n, expr = paste0(
      sample(x = CANONICAL_RNA, size = length, replace = TRUE), 
      collapse = ""
    )))
    
  } else if (type == "AA") {
    seqs <- AAStringSet(replicate(n = n, expr = paste0(
      sample(x = CANONICAL_AA, size = length, replace = TRUE), 
      collapse = ""
    )))
    
  } else {
    stop("'type' must be DNA, RNA, or AA.")
  }
  
  return (seqs)
}