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
BIO_SCORE_WEIGHTS = list(
  length               = 1,
  gc_content           = 1,
  hydrophobic_fraction = 1,
  charged_fraction     = 1,
  polar_fraction       = 1,
  composition_entropy  = 1
)

# =====| Biological Score |=====================================================

#' Compute Biological Score for BPE Tokenization
#'
#' Computes a biological score from the provided annotations as input into 
#' biology-aware BPE tokenization.
#'
#' @param bioBPE_seqs A `bioBPE_preprocessed` object containing preprocessed 
#'    DNA, RNA, or AA sequences.
#'
#' @return A numeric vector of biology scores per sequence sorted in the same
#'    order as `bioBPE_seqs`.
#'
#' @family preprocessing
#' @keywords preprocessing internal
#' 
#' @importFrom S4Vectors mcols
#' @importFrom scales rescale
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
  scores <- lapply(bioBPE_seqs$annot_steps, 
    function(a) scoring_funcs[[a]](S4Vectors::mcols(bioBPE_seqs$seqs))
  )
  scores <- as.matrix(do.call(cbind, scores))
  colnames(scores) <- bioBPE_seqs$annot_steps
  
  # Apply annotation-weighting upon the computed scores
  weights <- unlist(BIO_SCORE_WEIGHTS)[colnames(scores)]
  bio_score <- as.numeric(scores %*% weights) / sum(weights)
  
  return (bio_score)
}

# =====| Annotation Scoring Helpers |===========================================

.BioTokenizeR_normalize <- function(x) {
  return (scales::rescale(x, to = c(0, 1)))
}

.BioTokenizeR_score_length <- function(ann) {
  score <- as.numeric(.BioTokenizeR_normalize(log(ann$length + 1)))
  return (score)
}

.BioTokenizeR_score_gc <- function(ann) {
  score <- as.numeric(.BioTokenizeR_normalize(ann$gc_content))
  return (score)
}

.BioTokenizeR_score_hydrophobic <- function(ann) {
  score <- as.numeric(.BioTokenizeR_normalize(ann$hydrophobic_fraction))
  return (score)
}

.BioTokenizeR_score_charged <- function(ann) {
  score <- as.numeric(.BioTokenizeR_normalize(ann$charged_fraction))
  return (score)
}

.BioTokenizeR_score_polar <- function(ann) {
  score <- as.numeric(.BioTokenizeR_normalize(ann$polar_fraction))
  return (score)
}

.BioTokenizeR_score_entropy <- function(ann) {
  score <- as.numeric(.BioTokenizeR_normalize(ann$composition_entropy))
  return (score)
}