# ==============================================================================
# Purpose:            Tokenizes sequences using biology-aware BPE tokenization
# Author:             Sophia Li
# Date:               2025-10-27
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Treats all sequences as a single corpus to learn a shared
#                     vocabulary, then tokenizes sequences individually
# ==============================================================================

# =====| Tokenize Sequences |===================================================

tokenize_sequences <- function(bioBPE_seqs, vocab_size = 1000) {
  
  # Learn the BPE vocabulary using all sequences
  vocab <- .BioTokenizeR_learn_bpe_vocabulary(bioBPE_seqs = bioBPE_seqs, 
                                               vocab_size = vocab_size)
  
  # Perform the learned merges to generate tokens from the sequences
  tokens <- lapply(seqs, function(s) .BioTokenizeR_apply_bpe(s, vocab$merges))
  
  # Initialize a token obkect compatible with downstream analysis
  # ...
}

.BioTokenizeR_apply_bpe <- function(seq, merges) {
  
}

# =====| Learn BPE Vocabulary |=================================================

.BioTokenizeR_learn_bpe_vocabulary <- function(bioBPE_seqs, vocab_size = 1000) {
  
  # Fetch the sequences from the full annotated preprocessing object
  seqs <- bioBPE_seqs$seqs
  
  # Compute the biological score of each sequence based on their annotations
  bio_score <- .BioTokenizeR_compute_bio_score(bioBPE_seqs)
  
  # Initialize the vocabulary as individual characters (tokens)
  vocab <- unique(unlist(strsplit(as.character(unlist(seqs)), "")))
  
  # Iteratively learn merges until the desired vocabulary size is reached
  merges <- list()
  repeat {
    
    # Compute biologically-weighted pair frequencies based on bio-score
    pair_freqs <- .BioTokenizeR_compute_bio_pair_frequencies(seqs, bio_scores)
    
    # Stop if no pairings exist
    if (length(pair_freqs) == 0) break
    
    # Find the most frequent pair
    best_pair <- names(which.max(pair_freqs))
    best_freq <- max(pair_freqs)
    
    # Stop if no frequent pairs were found, or if vocabulary size was reached
    if (length(vocab) >= vocab_size || best_freq == 0) break
    
    # Merge all instances of the best pair
    seqs <- .BioTokenizeR_merge_best_pair(seqs, best_pair)
    merges[[length(merges) + 1]] <- best_pair
    
    # Update the vocabulary with the best merged pairing
    vocab <- unique(c(vocab, best_pair))
  }
  
  # Return the vocabulary as a list
  bpe_vocabulary <- list(
    vocab = vocab,
    merges = merges,
    bio_scores = bio_scores,
    type = type
  )
  
  return (bpe_vocabulary)
}

.BioTokenizeR_compute_bio_pair_frequencies <- function(seqs) {
  
}


.BioTokenizeR_merge_best_pair <- function() {
  
}
