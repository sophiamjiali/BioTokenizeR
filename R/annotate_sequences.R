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

# Define the canonical amino acids that are hydrophobic
HYDROPHOBIC_AA <- c("A", "V", "I", "L", "M", "F", "Y", "W")

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


# =====| Annotation Helper Functions |==========================================

.BioTokenizeR_annotate_DNA <- function(seqs) {
  
  # Define the annotation steps applied as metadata
  annot_steps <- c("length", "gc_content")
  
  # Annotate the Biostrings::XStringSet object
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
  annot_steps <- c("length", "hydrophobic_fraction", "composition_entropy")
  
  # Annotate the Biostrings::XStringSet object
  mcols(seqs)$length <- width(seqs)
  
  # Compute the hydrophobic fraction  
  mcols(seqs)$hydrophobic_fraction <- rowSums(Biostrings::letterFrequency(
    seqs, HYDROPHOBIC_AA, as.prob = TRUE
  ))
  
  # Compute Composition entropy based on amino acide frequencies
  aa_frequencies <- Biostrings::letterFrequency(
    seqs, Biostrings::AA_STANDARD, as.prob = TRUE
  )
  mcols(seqs)$composition_entropy <- apply(aa_freq, 1, function(p) {
    -sum(ifelse(p > 0, p * log2(p), 0))
  })
  
  return (seqs, annot_steps)
}