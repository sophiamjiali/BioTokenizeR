# ==============================================================================
# Purpose:            Preprocesses biological sequences for tokenization
# Author:             Sophia Li
# Date:               2025-10-27
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Explicitly separated preprocessing by type to facilitate 
#                     further extension; DNA/RNA is slightly redundant, but once
#                     further preprocessing is implemented, the functions will 
#                     scale more efficiently and cleanly
# ==============================================================================

# =====| Global Constants |=====================================================

# Define canonical nucleotides for each biological sequence type
CANONICAL_DNA <- c("A", "C", "G", "T")
CANONICAL_RNA <- c("A", "C", "G", "U")
CANONICAL_AA  <- c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N",
                   "P", "Q", "R", "S", "T", "V", "W", "Y")

# =====| Preprocessing Wrapper |================================================

preprocess_seqs <- function(seqs) {
  
  # Verify that the sequences provided are a Biostrings sequence set object
  if (!inherits(seqs, "XStringSet")) {
    stop(paste0("'seqs' must be a Biostrings::XStringSet (e.g., DNAStringSet, ",
                "RNAStringSet, AAStringSet)."))
  }
  
  # Verify that the sequences object is non-empty
  if (length(seqs) == 0) {
    stop("'seqs' cannot be empty.")
  }
  
  # Preprocess the sequences based on type, else raise an error if unsupported
  processed <- switch(class(seqs),
    "DNAStringSet" = .BioTokenizeR_preprocess_DNA(seqs),
    "RNAStringSet" = .BioTokenizeR_preprocess_RNA(seqs),
    "AAStringSet"  = .BioTokenizeR_preprocess_AA(seqs),
    stop(paste0("Unsupported sequence(s) type: must be a ",
                "Biostrings::XStringSet (DNAStringSet, RNAStringSet, ",
                "AAStringSet)."))
  )
  
  # Verify that valid sequences exist after preprocessing (and dropping empty)
  if (length(processed$seqs) == 0) {
    stop("No valid sequence(s) exist after preprocessing.")
  }
  
  # Initialize a preprocessed object compatible with downstream BPE tokenization
  seq_type <- switch(class(seqs),
                     DNAStringSet = "DNA",
                     RNAStringSet = "RNA",
                     AAStringSet  = "AA")
  
  bioBPE_seqs <- structure(list(
      seqs = processed$seqs,
      type = seq_type,
      preproc_steps = processed$steps
      annot_steps = NULL),
    class = "bioBPE_preprocessed"
  )
  
  return (bioBPE_seqs)
}


# =====| Preprocessing Helper Functions |=======================================

.BioTokenizeR_preprocess_DNA <- function(seqs) {
  
  # Define the preprocessing steps applied as metadata
  steps <- c("to_lower", "trim_N", "remove_ambiguous", "drop_empty")
  
  # Convert all sequences to lower-case
  seqs <- Biostrings::DNAStringSet(tolower(as.character(seqs)))
  
  # Trim trailing unknown/ambiguous 'N' nucleotides arising from seq. errors
  seqs <- Biostrings::trimLRPatterns(
    Lpattern = "N",
    Rpattern = "N",
    subject = seqs
  )
  
  # Remove ambiguous nucleotides: characters not in the canonical set
  canonical <- paste0("[^", paste(CANONICAL_DNA, collapse=""), "]")
  seqs <- DNAStringSet(gsub(canonical, "", as.character(seqs)))
  
  # Drop any empty sequences that may exist
  seqs <- seqs[width(seqs) > 0]
  
  return (list(seqs = seqs, steps = steps))
}

.BioTokenizeR_preprocess_RNA <- function(seqs) {
  
  # Define the preprocessing steps applied as metadata
  steps <- c("to_lower", "trim_N", "remove_ambiguous", "drop_empty")
  
  # Convert all sequences to lower-case
  seqs <- Biostrings::RNAStringSet(tolower(as.character(seqs)))
  
  # Trim trailing unknown/ambiguous 'N' nucleotides arising from seq. errors
  seqs <- Biostrings::trimLRPatterns(
    Lpattern = "N",
    Rpattern = "N",
    subject = seqs
  )
  
  # Remove ambiguous nucleotides: characters not in the canonical set
  canonical <- paste0("[^", paste(CANONICAL_RNA, collapse=""), "]")
  seqs <- RNAStringSet(gsub(canonical, "", as.character(seqs)))
  
  # Drop any empty sequences that may exist
  seqs <- seqs[width(seqs) > 0]
  
  return (list(seqs = seqs, steps = steps))
}

.BioTokenizeR_preprocess_AA <- function(seqs) {
  
  # Define the preprocessing steps applied as metadata
  steps <- c("to_lower", "remove_non_canonical", "drop_empty")
  
  # Convert all sequences to lower-case
  seqs <- Biostrings::AAStringSet(tolower(as.character(seqs)))
  
  # Remove ambiguous amino acids: characters not in the canonical set
  canonical <- paste0("[^", paste(CANONICAL_AA, collapse=""), "]")
  seqs <- AAStringSet(gsub(canonical, "", as.character(seqs)))
  
  # Drop any empty sequences that may exist
  seqs <- seqs[width(seqs) > 0]
  
  return (list(seqs = seqs, steps = steps))
}
