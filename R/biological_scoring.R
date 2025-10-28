# ==============================================================================
# Purpose:            Computes a biologically-motivated score for sequences
# Author:             Sophia Li
# Date:               2025-10-27
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Separated scoring and normalization functions into their 
#                     own helpers to ensure modularity and efficiency if 
#                     implementation later changes
# ==============================================================================

# =====| Global Constants |=====================================================

# Define weights of each annotation for computing bio-score
BIO_SCORE_WEIGHTS # ..

# =====| Biological Score |=====================================================

.BioTokenizeR_compute_bio_score <- function(bioBPE_seqs) {
  
  # Compute bio-score based on what annotations are available
  scoring_funcs <- list(
    length               = .BioTokenizeR_score_length,
    gc_content           = .BioTokenizeR_score_gc,
    hydrophobic_fraction = .BioTokenizeR_score_hydrophobic,
    charged_fraction     = .BioTokenizeR_score_charged,
    polar_fraction       = .BioTokenizeR_score_polar,
    composition_entropy  = .BioTokenizeR_score_entropy
  )
  
  # Compute a matrix of bio-scores for each sequence
  scores <- lapply(bioBPE$annot_steps, function(a) ann_scoring[[a]](ann))
  scores <- do.call(cbind, scores)
  
  # Apply annotation-weighting upon the computed scores
  bio_score <- as.numeric(scores %*% BIO_SCORE_WEIGHTS)
  
  return (bio_score)
}

# =====| Annotation Scoring Helpers |===========================================

.BioTokenizeR_normalize <- function(x) {
  return (scales::rescale(x, to = c(0, 1)))
}

.BioTokenizeR_score_length <- function(ann) {
  return (.BioTokenizeR_normalize(log(ann$length + 1)))
}

.BioTokenizeR_score_gc <- function(ann) {
  return (.BioTokenizeR_normalize(ann$gc_content)
}

.BioTokenizeR_score_length <- function(ann) {
  return (.BioTokenizeR_normalize(log(ann$length + 1)))
}

.BioTokenizeR_score_hydrophobic <- function(ann) {
  return (.BioTokenizeR_normalize(ann$hydrophobic_fraction))
}

.BioTokenizeR_score_charged <- function(ann) {
  return (.BioTokenizeR_normalize(ann$charged_fraction))
}

.BioTokenizeR_score_polar <- function(ann) {
  return (.BioTokenizeR_normalize(ann$polar_fraction))
}

.BioTokenizeR_score_entropy <- function(ann) {
  return (.BioTokenizeR_normalize(ann$composition_entropy))
}