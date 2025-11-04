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

tokenize_sequences <- function(bioBPE_seqs, vocab_size = 15) {
  
  # Verify that the sequences object is of class bioBPE_preprocessed
  if (!inherits(bioBPE_seqs, "bioBPE_preprocessed")) {
    stop("'bioBPE_seqs' must be a bioBPE_preprocessed.")
  }
  
  # Verify that not all sequences are of length zero
  if (all(width(bioBPE_seqs$seqs) == 0)) {
    stop("'bioBPE_seqs' must contain at least one sequence that is not empty.")
  }
  
  # Verify that the vocabulary size is greater than zero
  if (vocab_size <= 0) {
    stop("'vocab_size' must be greater or equal to one.")
  }
  
  # Learn the BPE vocabulary using all sequences
  vocab <- .BioTokenizeR_learn_bpe_vocabulary(bioBPE_seqs = bioBPE_seqs, 
                                               vocab_size = vocab_size)
  
  # Perform the learned merges to generate tokens from the sequences
  tokens <- .BioTokenizeR_apply_bpe(
    seqs = as.character(bioBPE_seqs$seqs), 
    vocab = vocab
  )
  
  return (list(
    vocab = vocab, 
    tokens = tokens
  ))
  
}

.BioTokenizeR_apply_bpe <- function(seqs, vocab) {
  
  # Verify that the input was provided appropriately
  if (!is.character(seqs)) stop("'seqs' must be a character vector.")
  if (!is.list(vocab)) {
    stop(paste0("'vocab' must be a list returned by ",
                ".BioTokenizeR_learn_bpe_vocabulary."))
  }
  
  # Convert sequences to space-separated tokens
  seqs <- vapply(seqs, function(s) {
    paste(strsplit(s, "")[[1]], collapse = " ")
  }, character(1))
  
  # Apply the merges to each sequence in the learned order
  for (merge in vocab$merges) {
    
    # Merge the pair into a single token
    merged_token <- gsub(" ", "", merge)
    
    # Replace each occurrence of the merge in all sequences
    seqs <- vapply(seqs, function(s) {
      gsub(merge, merged_token, s, fixed = TRUE)
    }, character(1))
  }
  
  # Convert space-separated strings to lists of vectors
  seqs <- unname(seqs)
  token_lists <- strsplit(seqs, " ", fixed = TRUE)
  
  return (token_lists)
}

# =====| Learn BPE Vocabulary |=================================================

.BioTokenizeR_learn_bpe_vocabulary <- function(bioBPE_seqs, vocab_size = 15) {
  
  # Tokenize the sequences into single-character tokens: string with spaces
  seqs <- as.character(bioBPE_seqs$seqs)
  tok_seqs <- vapply(seqs, function(s) 
    paste(strsplit(s, "")[[1]], collapse = " "), 
    FUN.VALUE = character(1)
  )
  
  # Compute the biological score of each sequence based on their annotations
  bio_scores <- .BioTokenizeR_compute_bio_score(bioBPE_seqs = bioBPE_seqs)
  
  # Initialize the vocabulary as individual characters (tokens)
  vocab <- unique(unlist(strsplit(seqs, "")))
  
  # If the base vocabulary is larger than the vocabulary size, exit early
  if (length(vocab) >= vocab_size) {
    return (list(
      vocab = vocab,
      merges = NULL,
      bio_scores = bio_scores
    ))
  }
  
  # Iteratively learn merges until the desired vocabulary size is reached
  merges <- list()
  repeat {
    
    # Compute biologically-weighted pair frequencies based on bio-score
    pair_freqs <- .BioTokenizeR_compute_bio_pair_frequencies(
      tok_seqs = tok_seqs, 
      bio_scores = bio_scores
    )
    
    # Stop if no pairings exist
    if (length(pair_freqs) == 0) break
    
    # Find the most frequent pair
    best_pair <- names(which.max(pair_freqs))
    best_freq <- max(pair_freqs)
    
    # Stop if no frequent pairs were found, or if vocabulary size was reached
    if (length(vocab) >= vocab_size || best_freq == 0) break
    
    # Merge all instances of the best pair
    tok_seqs <- .BioTokenizeR_merge_best_pair(
      tok_seqs = tok_seqs,
      best_pair = best_pair
    )
    merges[[length(merges) + 1]] <- best_pair
    
    # Update the vocabulary with the best merged pairing, removing the space
    new_token <- gsub(" ", "", best_pair)
    vocab <- unique(c(vocab, new_token))
  }
  
  # Return the vocabulary as a list
  bpe_vocabulary <- list(
    vocab = vocab,
    merges = merges,
    bio_scores = bio_scores
  )
  
  return (bpe_vocabulary)
}


.BioTokenizeR_compute_bio_pair_frequencies <- function(tok_seqs, bio_scores) {
  
  # Verify that the number of bio-scores match the number of sequences
  if (length(tok_seqs) != length(bio_scores)) {
    stop("Length of 'tok_seqs' and 'bio_scores' must match.")
  }
  
  # Iterate over all sequences, computing adjacent pair scores
  pair_freqs <- list()
  for (i in seq_along(tok_seqs)) {
    
    # Fetch the tokenized sequence, a string separated by spaces
    tokens <- tok_seqs[[i]]
    if (nchar(tokens) == 0) next
    
    # Identify all adjacent token pairs, skipping if no pairs were found
    matches <- gregexpr("(\\S+) (\\S+)", tokens, perl = TRUE)[[1]]
    if (matches[1] == -1) next
    
    # Extract the matching substrings
    pairs <- regmatches(tokens, list(matches))[[1]]
    counts <- table(pairs)
    
    # Weigh the pairs by bio_score
    weighted_counts <- as.numeric(counts) * bio_scores[i]
    names(weighted_counts) <- names(counts)
    
    # Accumulate the weighted pairs
    for (pair in names(weighted_counts)) {
      pair_freqs[[pair]] <- (pair_freqs[[pair]] %||% 0) + weighted_counts[[pair]]
    }
  }
  
  # Return the bio-score weighted pair frequencies as a sorted numeric vector
  pair_freqs <- unlist(pair_freqs, use.names = TRUE)
  pair_freqs <- sort(pair_freqs, decreasing = TRUE)
  
  return (pair_freqs)
}


.BioTokenizeR_merge_best_pair <- function(tok_seqs, best_pair) {
  
  # Verify that the input was provided appropriately
  if (!is.character(tok_seqs)) stop("'tok_seqs' must be a character vector.")
  if (!is.character(best_pair) || length(best_pair) != 1) {
    stop("'best_pair' must be a single string.")
  }
  
  # Replace the best pair with the merged token in all sequences
  merged_token <- gsub(" ", "", best_pair)
  
  merged_seqs <- vapply(tok_seqs, function(s) {
      gsub(paste0("\\b", best_pair, "\\b"), merged_token, s, perl = TRUE)
  }, FUN.VALUE = character(1))
  
  return (merged_seqs)
}
