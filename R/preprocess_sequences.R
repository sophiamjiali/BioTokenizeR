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

#' Preprocess Biological Sequences for BPE Tokenization
#'
#' Performs input validation and standardized preprocessing of biological
#' sequences (DNA, RNA, or AA) to ensure compatibility with downstream byte-pair
#' encoding (BPE) tokenization and analysis workflows.
#' 
#' This function verifies that the input is a valid `Biostrings::XStringSet` 
#' object, applies preprocessing appropriate to the sequence type, and returns 
#' a structured object of class `"bioBPE_preprocessed"` containing the processed
#' sequences and metadata describing the preprocessing steps applied.
#'
#' @param seqs A `Biostrings::XStringSet` object containing DNA, RNA, or AA
#'    sequences (e.g. `DNAStringSet`, `RNAStringSet`, or `AAStringSet`).
#'
#' @return An object of class `"bioBPE_preprocessed"`, being a list containing:
#'    \describe{
#'        \item{`seqs`}{A cleaned `Biostrings::XStringSet` object of 
#'            preprocessed sequences.}
#'        \item{`type`}{A string indicating the biological sequence type: 
#'            `"DNA"`, `"RNA"`, or `"AA"`.}
#'        \item{`preproc_steps`}{A record of preprocessing steps applied.}
#'        \item{`annot_steps`}{Annotation steps applied (initialized as `NULL`).}
#'    }
#' 
#' @details The preprocessing step may include sequence cleaning, normalization,
#'    or removal of invalid characters, depending on sequence type. The exact 
#'    operations are delegated to internal helper functions:
#'    \itemize{
#'        \item `.BioTokenizeR_preprocess_DNA()` for DNA sequences
#'        \item `.BioTokenizeR_preprocess_RNA()` for RNA sequences
#'        \item `.BioTokenizeR_preprocess_AA()` for amino acid sequences
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
#' 
#'    # Preprocess the sequences
#'    dna_preproc <- preprocess_sequences(seqs = data$dna_seq)
#'    rna_preproc <- preprocess_sequences(seqs = data$rna_seq)
#'    aa_preproc <- preprocess_sequences(seqs = data$aa_seq)
#' }
#' 
#' @family preprocessing
#' @keywords preprocessing internal
#'     
#' @import Biostrings
#' @export
preprocess_sequences <- function(seqs) {
  
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
      preproc_steps = processed$steps,
      annot_steps = NULL),
    class = "bioBPE_preprocessed"
  )
  
  return (bioBPE_seqs)
}


# =====| Preprocessing Helper Functions |=======================================

#' Preprocess DNA Sequences
#'
#' Performs standardized preprocessing of DNA sequences to prepare them for
#' downstream byte-pair encoding (BPE) tokenization. Steps include case
#' normalization, trimming ambiguous nucleotides, and removing invalid 
#' characters.
#'
#' @param seqs A `Biostrings::DNAStringSet` object containing raw DNA sequences.
#'
#' @return A list with components:
#'    \describe{
#'        \item{`seqs`}{A cleaned `Biostrings::DNAStringSet` object of 
#'            preprocessed sequences.}
#'        \item{`steps`}{A character vector describing preprocessing steps applied.}
#'    }
#'
#' @family preprocessing
#' @keywords preprocessing internal
#' 
#' @importFrom Biostrings DNAStringSet trimLRPatterns
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Preprocess RNA Sequences for BPE Tokenization
#'
#' Performs standardized preprocessing of RNA sequences to prepare them for
#' downstream byte-pair encoding (BPE) tokenization. Steps include case
#' normalization, trimming ambiguous nucleotides, and removing invalid 
#' characters.
#'
#' @param seqs A `Biostrings::RNAStringSet` object containing raw RNA sequences.
#'
#' @return A list with components:
#'    \describe{
#'        \item{`seqs`}{A cleaned `Biostrings::RNAStringSet` object of 
#'            preprocessed sequences.}
#'        \item{`steps`}{A character vector describing preprocessing steps applied.}
#'    }
#'
#' @family preprocessing
#' @keywords preprocessing internal
#' 
#' @importFrom Biostrings RNAStringSet trimLRPatterns
.BioTokenizeR_preprocess_RNA <- function(seqs) {
  
  # Define the preprocessing steps applied as metadata
  steps <- c("to_lower", "trim_N", "remove_ambiguous", "drop_empty")
  
  # Convert all sequences to lower-case
  seqs <- Biostrings::RNAStringSet(tolower(as.character(seqs)))
  
  # Trim trailing unknown/ambiguous 'N' nucleotides arising from seq. errors
  seqs <- Biostrings::trimLRPatterns(
    Lpattern = "N",
    Rpattern = "N",
    subject  = seqs
  )
  
  # Remove ambiguous nucleotides: characters not in the canonical set
  canonical <- paste0("[^", paste(CANONICAL_RNA, collapse=""), "]")
  seqs <- Biostrings::RNAStringSet(gsub(canonical, "", as.character(seqs)))
  
  # Drop any empty sequences that may exist
  seqs <- seqs[width(seqs) > 0]
  
  return (list(seqs = seqs, steps = steps))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Preprocess AA Sequences for BPE Tokenization
#'
#' Performs standardized preprocessing of AA sequences to prepare them for
#' downstream byte-pair encoding (BPE) tokenization. Steps include case
#' normalization and removing invalid characters.
#'
#' @param seqs A `Biostrings::AAStringSet` object containing raw AA sequences.
#'
#' @return A list with components:
#'    \describe{
#'        \item{`seqs`}{A cleaned `Biostrings::AAStringSet` object of 
#'            preprocessed sequences.}
#'        \item{`steps`}{A character vector describing preprocessing steps applied.}
#'    }
#'
#' @family preprocessing
#' @keywords preprocessing internal
#' 
#' @importFrom Biostrings AAStringSet
.BioTokenizeR_preprocess_AA <- function(seqs) {
  
  # Define the preprocessing steps applied as metadata
  steps <- c("to_lower", "remove_non_canonical", "drop_empty")
  
  # Convert all sequences to lower-case
  seqs <- Biostrings::AAStringSet(tolower(as.character(seqs)))
  
  # Remove ambiguous amino acids: characters not in the canonical set
  canonical <- paste0("[^", paste(CANONICAL_AA, collapse=""), "]")
  seqs <- Biostrings::AAStringSet(gsub(canonical, "", as.character(seqs)))
  
  # Drop any empty sequences that may exist
  seqs <- seqs[width(seqs) > 0]
  
  return (list(seqs = seqs, steps = steps))
}
