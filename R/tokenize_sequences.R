# ==============================================================================
# Purpose:            Tokenizes sequences using biology-aware BPE tokenization
# Author:             Sophia Li
# Date:               2025-10-27
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Treats all sequences as a single corpus to learn a shared
#                     vocabulary, then tokenizes sequences individually
# ==============================================================================

# =====| Global Constants |=====================================================




# =====| Tokenize Sequences |===================================================

tokenize_sequences <- function(bioBPE_seqs, vocab_size = 1000) {
  
  # Prepare the sequences and annotations
  seqs <- as.character(bioBPE_seqs$seqs)
  ann <- mcols(bioBPE_seqs$seqs)
  
  # Learn the BPE vocabulary using all sequences
  merges <- .BioTokenizeR_learn_bpe_vocabulary(seqs = seqs, ann = ann, 
                                               vocab_size = vocab_size)
  
  # Perform the learned merges to generate tokens from the sequences
  tokens <- lapply(seqs, function(s) .BioTokenizeR_apply_bpe(s, merges))
  
  # Initialize a token obkect compatible with downstream analysis
  # ...
}

.BioTokenizeR_apply_bpe <- function(seq, merges) {
  
}

# =====| Learn BPE Vocabulary |=================================================

.BioTokenizeR_learn_bpe_vocabulary <- function(seqs, ann, vocab_size = 1000) {
  
  # Compute the biological score of each sequence based on their annotations
  
  # Iteratively compute merges until the desired vocabulary size is reached
  merges <- list()
  for (i in seq(along = vocab_size)) {
    
    # Compute pair frequencies
    pair_freqs <- .BioTokenizeR_compute_pair_frequencies(seqs)
    
    # 
    
  }
  
}

.BioTokenizeR_compute_pair_frequencies <- function(seqs) {
  
}

.BioTokenizeR_merge_best_pair <- function() {
  
}


# Biological Scoring -----------------------------------------------------------
.BioTokenizeR_weigh_bio_frequencies <- function() {
  
}

.BioTokenizeR_score_biological <- function() {
  
}

.BioTokenizeR_score_codon_boundary <- function() {
  
}

.BioTokenizeR_score_cpg_islands <- function() {
  
}

.BioTokenizeR_score_hydrophobic_runs <- function() {
  
}

