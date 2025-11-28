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

# Define default weights of each annotation for computing bio-score
DEFAULT_BIO_WEIGHTS = list(
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
#' biology-aware BPE tokenization. This biological score is used to incorporate
#' awareness of biological annotations into the NLP-style tokenization and 
#' vocabulary learning phase of the pipeline, utilizing user-provided (or
#' default) weightings of the implemented annotations to fine-tune performance.
#'
#' @param bioBPE_seqs A `bioBPE_preprocessed` object containing preprocessed 
#'    DNA, RNA, or AA sequences.
#' @param bio_weights A named list of numerical values containing the following:
#'    \describe{
#'        \item{`length`}{A numerical value indicating the weight of the length 
#'            annotation when computing each sequence's biological score.},
#'        \item{`gc_content`}{A numerical value indicating the weight of the 
#'            GC-content annotation when computing each sequence's biological score.},
#'        \item{`hydrophobic_fraction`}{A numerical value indicating the weight 
#'            of the hydrophobic fraction annotation when computing each sequence's 
#'            biological score.},
#'        \item{`charged_fraction`}{A numerical value indicating the weight of
#'            the charged fraction annotation when computing each sequence's 
#'            biological score.},
#'        \item{`polar_fraction`}{A numerical value indicating the weight of the
#'            polar fraction annotation when computing each sequence's biological 
#'            score.},
#'        \item{`composition_entropy`}{A numerical value indicating the weight 
#'            of the composition entropy annotation when computing each sequence's
#'            biological score.}
#'    }
#'
#' @return A numeric vector of biology scores per sequence sorted in the same
#'    order as `bioBPE_seqs`.
#'    
#' @references {
#'    Dotan E, Jaschek G, Pupko T, Belinkov Y (2024). Effect of tokenization on
#'    transformers for biological sequences. Bioinformatics, 40(4): btae196.
#'    doi:10.1093/bioinformatics/btae196. PMCID: PMC11055402.
#' 
#'    Pagès H, Lawrence M, Aboyoun P (2025). S4Vectors:
#'    Foundation of vector-like and list-like containers in Bioconductor.
#'    doi:10.18129/B9.bioc.S4Vectors https://doi.org/10.18129/B9.bioc.S4Vectors,
#'    R package version 0.48.0, https://bioconductor.org/packages/S4Vectors.
#'    
#'    Wickham H, Pedersen T, Seidel D (2025). _scales: Scale Functions for 
#'    Visualization_. doi:10.32614/CRAN.package.scales 
#'    <https://doi.org/10.32614/CRAN.package.scales>, R package version 1.4.0, 
#'    <https://CRAN.R-project.org/package=scales>.
#' }
#'
#' @family preprocessing
#' @keywords preprocessing internal
#' 
#' @importFrom S4Vectors mcols
#' @importFrom scales rescale
.BioTokenizeR_compute_bio_score <- function(bioBPE_seqs, bio_weights = NULL) {
  
  # Parse if biological annotation weights were provided, else use defaults
  if (is.null(bio_weights)) { bio_weights = DEFAULT_BIO_WEIGHTS }
  
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
  weights <- unlist(bio_weights)[colnames(scores)]
  bio_score <- as.numeric(scores %*% weights) / sum(weights)
  
  return (bio_score)
}

# =====| Annotation Scoring Helpers |===========================================

#' Normalize Biological Annotation
#'
#' This function normalizes a biological annotation of a DNA, RNA, or AA 
#' sequence through min-max scaling to (0, 1). Scaling is necessary to ensure
#' comparability between sequences within a set.
#' 
#' @param x A numerical value to min-max scale.
#' 
#' @return A numerical value scaled to the range of (0, 1).
#' 
#' @references {
#'    Wickham H, Pedersen T, Seidel D (2025). _scales: Scale Functions for 
#'    Visualization_. doi:10.32614/CRAN.package.scales 
#'    <https://doi.org/10.32614/CRAN.package.scales>, R package version 1.4.0, 
#'    <https://CRAN.R-project.org/package=scales>.
#' }
#' 
#' @family preprocessing
#' @keywords preprocessing internal
#' 
#' @importFrom scales rescale
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

# [END]