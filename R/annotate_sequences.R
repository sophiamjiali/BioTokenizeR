# ==============================================================================
# Purpose:            Annotates preprocessed biological sequences for BPE token.
# Author:             Sophia Li
# Date:               2025-10-27
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Explicitly separated annotation by type to facilitate 
#                     further extension; slightly redundant, but modularity will
#                     scale more efficiently and cleanly
# ==============================================================================

# =====| Global Constants |=====================================================

# Define canonical amino acid properties
HYDROPHOBIC <- c("A", "V", "I", "L", "M", "F", "Y", "W")
CHARGED_POS <- c("K", "R", "H")
CHARGED_NEG <- c("D", "E")
POLAR <- c("S", "T", "N", "Q")

# =====| Annotation Wrapper |===================================================

#' Annotate Biological Sequences for BPE Workflows
#'
#' Adds biological annotations to preprocessed sequences, preparing them for
#' downstream byte-pair encoding (BPE) tokenization and analysis.
#' Annotation steps depend on the sequence type (DNA, RNA, or amino acids).
#'
#' @param bioBPE_seqs A `bioBPE_preprocessed` object containing preprocessed 
#'    sequences (DNA, RNA, or AA).
#'
#' @return The same `bioBPE_preprocessed` object with the `seqs` updated with
#'    annotations and `annot_steps` recording the steps applied.
#'    
#' @details The annotation step may include annotating sequence length, GC 
#'    content, hydrophobic fraction, charged fraction, polar fraction, or 
#'    composition entropy, depending on sequence type. The exact operations are 
#'    delegated to internal helper functions:
#'    \itemize{
#'        \item `.BioTokenizeR_annotate_DNA()` for DNA sequences
#'        \item `.BioTokenizeR_annotate_RNA()` for RNA sequences
#'        \item `.BioTokenizeR_annotate_AA()` for amino acid sequences
#'    }
#'    
#' @examples
#' \dontrun{
#'    # Generate simulated data
#'    data <- generate_data(
#'        n          = 3, 
#'        length     = 1000, 
#'        vocab_size = 25, 
#'        preprocess = TRUE,
#'        annotate   = FALSE,
#'        tokenize   = FALSE,
#'        summarize  = FALSE,
#'        verbose    = FALSE
#'    )
#'   
#'    # Annotate the sequences
#'    dna_annot <- annotate_sequences(bioBPE_seqs = data$dna_preproc)
#'    rna_annot <- annotate_sequences(bioBPE_seqs = data$rna_preproc)
#'    aa_annot <- annotate_sequences(bioBPE_seqs = data$aa_preproc)
#' }
#' 
#' @family annotation
#' @keywords annotation internal
#' 
#' @export
annotate_sequences <- function(bioBPE_seqs) {
  
  # Verify that the sequences object is of class bioBPE_preprocessed
  if (!inherits(bioBPE_seqs, "bioBPE_preprocessed")) {
    stop("'bioBPE_seqs' must be a bioBPE_preprocessed.")
  }
  
  # Verify that not all sequences are of length zero
  if (all(width(bioBPE_seqs$seqs) == 0)) {
    stop("'bioBPE_seqs' must contain at least one sequence that is not empty.")
  }
  
  # Annotate the sequences based on type
  annotation <- switch(bioBPE_seqs$type,
    "DNA" = .BioTokenizeR_annotate_DNA(bioBPE_seqs$seqs),
    "RNA" = .BioTokenizeR_annotate_RNA(bioBPE_seqs$seqs),
    "AA" = .BioTokenizeR_annotate_AA(bioBPE_seqs$seqs),
    stop("'bioBPE_seqs' is an unknown sequence type.")
  )
  
  # Re-initialize the bioBPE_preprocessed objects with annotations and steps
  bioBPE_seqs$seqs <- annotation$seqs
  bioBPE_seqs$annot_steps <- annotation$steps
  
  return (bioBPE_seqs)
}


# =====| Annotation Functions |=================================================

#' Annotate DNA Sequences for BPE Workflows
#'
#' Computes basic annotations for DNA sequences, including sequence length and
#' GC content, for downstream byte-pair encoding (BPE) analysis.
#'
#' @param seqs A `Biostrings::DNAStringSet` object containing preprocessed DNA
#'    sequences.
#'
#' @return A list with components:
#'    \describe{
#'        \item{`seqs`}{The input DNA sequences updated with metadata columns
#'            (`length` and `gc_content`).}
#'        \item{`steps`}{A character vector of annotation steps applied.}
#'    }
#'
#' @family annotation
#' @keywords annotation internal
#' 
#' @importFrom Biostrings letterFrequency
.BioTokenizeR_annotate_DNA <- function(seqs) {
  
  # Define the annotation steps applied as metadata
  steps <- c("length", "gc_content")
  
  # Annotate the Biostrings::XStringSet object for length and GC content
  mcols(seqs)$length <- width(seqs)
  mcols(seqs)$gc_content <- rowSums(Biostrings::letterFrequency(
    seqs, c('G', 'C'), as.prob = TRUE
  ))
  
  return (list(seqs = seqs, steps = steps))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Annotate RNA Sequences for BPE Workflows
#'
#' Computes basic annotations for DNA sequences, including sequence length and
#' GC content, for downstream byte-pair encoding (BPE) analysis.
#'
#' @param seqs A `Biostrings::RNAStringSet` object containing preprocessed DNA
#'    sequences.
#'
#' @return A list with components:
#'    \describe{
#'        \item{`seqs`}{The input RNA sequences updated with metadata columns
#'            (`length` and `gc_content`).}
#'        \item{`steps`}{A character vector of annotation steps applied.}
#'    }
#'
#' @family annotation
#' @keywords annotation internal
#' 
#' @importFrom Biostrings letterFrequency
.BioTokenizeR_annotate_RNA <- function(seqs) {
  
  # Define the annotation steps applied as metadata
  steps <- c("length", "gc_content")
  
  # Annotate the Biostrings::XStringSet object
  mcols(seqs)$length <- width(seqs)
  mcols(seqs)$gc_content <- rowSums(Biostrings::letterFrequency(
    seqs, c('G', 'C'), as.prob = TRUE
  ))
  
  return (list(seqs = seqs, steps = steps))
}

#' Annotate AA Sequences for BPE Workflows
#'
#' Computes basic annotations for AA sequences, including sequence length, 
#' hydrophobic fraction, charged fraction, polar fraction, and composition
#' entropy, for downstream byte-pair encoding (BPE) analysis.
#'
#' @param seqs A `Biostrings::AAStringSet` object containing preprocessed DNA
#'    sequences.
#'
#' @return A list with components:
#'    \describe{
#'        \item{`seqs`}{The input AA sequences updated with metadata columns
#'            (`length`, `hydrophobic_fraction`, `charged_fraction`,
#'            `polar_fraction`, and `composition_entropy`).}
#'        \item{`steps`}{A character vector of annotation steps applied.}
#'    }
#'
#' @family annotation
#' @keywords annotation internal
#' 
#' @importFrom Biostrings letterFrequency
.BioTokenizeR_annotate_AA <- function(seqs) {
  
  # Define the annotation steps applied as metadata
  steps <- c("length", "hydrophobic_fraction", "charged_fraction",
                   "polar_fraction", "composition_entropy")
  
  # Annotate the Biostrings::XStringSet object
  mcols(seqs)$length <- width(seqs)
  
  # Compute amino acid frequencies for hydrophobic, charged, and polar fractions
  aa_counts <- Biostrings::letterFrequency(seqs, Biostrings::AA_STANDARD, 
                                           as.prob = TRUE)
  mcols(seqs)$hydrophobic_fraction <- rowSums(
    aa_counts[, HYDROPHOBIC, drop = FALSE]
  )
  mcols(seqs)$charged_fraction <- rowSums(
    aa_counts[, c(CHARGED_POS, CHARGED_NEG), drop = FALSE]
  )
  mcols(seqs)$polar_fraction <- rowSums(
    aa_counts[, POLAR, drop = FALSE]
  )

  # Compute Composition entropy based on amino acid frequencies
  mcols(seqs)$composition_entropy <- apply(aa_counts, 1, function(p) {
    -sum(ifelse(p > 0, p * log2(p), 0))
  })
  
  return (list(seqs = seqs, steps = steps))
}
