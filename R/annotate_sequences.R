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

annotate_sequences <- function(bioBPE_seqs) {
  
  # Verify that the sequences object is of class bioBPE_preprocessed
  if (!inherits(bioBPE_seqs, "bioBPE_preprocessed")) {
    stop("'bioBPE_seqs' must be a bioBPE_preprocessed.")
  }
  
  # Verify that the sequences object is non-empty
  if (length(bioBPE_seqs) == 0) {
    stop("'bioBPE_seqs' cannot be empty.")
  }
  
  # Annotate the sequences based on type
  annotated, annot_steps <- switch(type,
    "DNA" = .BioTokenizeR_annotate_DNA(bioBPE_seqs$seqs),
    "RNA" = .BioTokenizeR_annotate_RNA(bioBPE_seqs$seqs),
    "AA" = .BioTokenizeR_annotate_AA(bioBPE_seqs$seqs),
    stop("'bioBPE_seqs' is an unknown sequence type.")
  )
  
  # Re-initialize the bioBPE_preprocessed objects with annotations and steps
  bioBPE_seqs$seqs <- annotated
  bioBPE_seqs$annot_steps <- annot_steps
  
  return (bioBPE_seqs)
}


# =====| Annotation Functions |=================================================

.BioTokenizeR_annotate_DNA <- function(seqs) {
  
  # Define the annotation steps applied as metadata
  annot_steps <- c("length", "gc_content")
  
  # Annotate the Biostrings::XStringSet object for length and GC content
  mcols(seqs)$length <- width(seqs)
  mcols(seqs)$gc_content <- rowSums(Biostrings::letterFrequency(
    seqs, c('G', 'C'), as.prob = TRUE
  ))
  
  return (seqs, annot_steps)
}


.BioTokenizeR_annotate_RNA <- function(seqs) {
  
  # Define the annotation steps applied as metadata
  annot_steps <- c("length", "gc_content")
  
  # Annotate the Biostrings::XStringSet object
  mcols(seqs)$length <- width(seqs)
  mcols(seqs)$gc_content <- rowSums(Biostrings::letterFrequency(
    seqs, c('G', 'C'), as.prob = TRUE
  ))
  
  return (seqs, annot_steps)
}


.BioTokenizeR_annotate_AA <- function(seqs) {
  
  # Define the annotation steps applied as metadata
  annot_steps <- c("length", "hydrophobic_fraction", "charged_fraction",
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
  
  return (seqs, annot_steps)
}


