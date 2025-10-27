# ==============================================================================
# Purpose:            Tokenizes sequences using biology-aware BPE tokenization
# Author:             Sophia Li
# Date:               2025-10-27
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Treats all sequences as a single corpus to learn a shared
#                     vocabulary, then tokenizes sequences individually
# ==============================================================================


# BPE Algorithm Functions ------------------------------------------------------

tokenize_sequences <- function() {
  
}

bpe_algorithm <- function() {
  
}

compute_pair_frequencies <- function() {
  
}

select_best_merge <- function() {
  
}


# Biological Scoring -----------------------------------------------------------

score_biological <- function() {
  
}

score_codon_boundary <- function() {
  
}

score_cpg_islands <- function() {
  
}

score_hydrophobic_runs <- function() {
  
}


Okay, let's now assume that we have the preprocessed sequences in the bioBPE_preprocessed object. Let's now begin structuring our tokenize_sequences.R file. My thoughts were to define the main BPE algorihtm functions such as compute_pair_frequencies, but also define a new biological scoring based on annotations1