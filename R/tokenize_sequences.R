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
  
  # Initialize integer sequences using the base token mapping
  base_tokens <- vocab$vocab[1:(length(vocab$vocab) - length(vocab$merges))]
  token_to_id <- setNames(seq_along(base_tokens), base_tokens)
  id_to_token <- setNames(base_tokens, seq_along(base_tokens))
  
  id_seqs <- lapply(seqs, function(s) token_to_id[strsplit(s, "")[[1]]])
  
  # Convert sequences to space-separated tokens
  seqs <- vapply(seqs, function(s) {
    paste(strsplit(s, "")[[1]], collapse = " ")
  }, character(1))
  
  # Apply the merges to each sequence in the learned order
  for (merge in vocab$merges) {
    
    # Parse the ID-based merge pair
    a <- merge[1]; b <- merge[2]
    new_id <- length(id_to_token) + 1
    
    # Merge all sequences
    id_seqs <- .BioTokenizeR_merge_best_pair(id_seqs, a, b, new_id)
    
    # Update the ID to token mapping
    id_to_token[[as.character(new_id)]] <- paste0(id_to_token[[as.character(a)]],
                                                  id_to_token[[as.character(b)]])
  }
  
  # Convert integer sequences back to token vectors
  token_lists <- lapply(id_seqs, function(ids) id_to_token[as.character(ids)])
  
  return (token_lists)
}

# =====| Learn BPE Vocabulary |=================================================

.BioTokenizeR_learn_bpe_vocabulary <- function(bioBPE_seqs, vocab_size = 15) {
  
  # Initialize sequences as integer token vectors, initializing a mapping
  seqs <- as.character(bioBPE_seqs$seqs)
  vocab <- unique(unlist(strsplit(seqs, "")))

  token_to_id <- setNames(seq_along(vocab), vocab)
  id_to_token <- setNames(vocab, seq_along(vocab))
  id_seqs <- lapply(seqs, function(s) token_to_id[strsplit(s, "")[[1]]])
  
  # Compute the biological score of each sequence based on their annotations
  bio_scores <- .BioTokenizeR_compute_bio_score(bioBPE_seqs = bioBPE_seqs)
  
  # If the base vocabulary is larger than the vocabulary size, exit early
  if (length(vocab) >= vocab_size) {
    return (list(vocab = vocab, merges = NULL, bio_scores = bio_scores))
  }
  
  # Iteratively learn merges until the desired vocabulary size is reached
  merges <- list()
  current_vocab_id <- length(vocab)
  
  repeat {
    
    # Compute biologically-weighted pair frequencies based on bio-score
    pair_freqs <- .BioTokenizeR_compute_bio_pair_frequencies(
      id_seqs = id_seqs, 
      bio_scores = bio_scores
    )
    print("Computed pair frequencies")
    if (length(pair_freqs) == 0) break
    
    # Find the most frequent pair
    best_idx <- which.max(pair_freqs)
    pair_key <- names(pair_freqs)[best_idx]
    
    # Decode the integer IDs of the best pair found, storing the merge
    pair_ids <- as.integer(strsplit(pair_key, " ")[[1]])
    if (length(pair_ids) != 2 || any(is.na(pair_ids))) break
    a <- pair_ids[1]; b <- pair_ids[2]
    
    # Stop if frequency was returned as zero or if the vocabulary size is max
    if (pair_freqs[best_idx] == 0 || length(id_to_token) >= vocab_size) break
    
    # Assign a new ID to the merged token
    current_vocab_id <- current_vocab_id + 1
    new_id <- current_vocab_id
    merges[[length(merges) + 1]] <- c(a, b)

    # Merge the pair in all sequences
    id_seqs <- .BioTokenizeR_merge_best_pair(id_seqs = id_seqs, a = a, b = b, 
                                             new_id = new_id)
    print("Merged best pair")
    
    # Update the vocabulary (integer-token mapping) with the merged pair
    id_to_token[[as.character(new_id)]] <- paste0(id_to_token[[as.character(a)]], 
                                                  id_to_token[[as.character(b)]])
  }
  
  vocab <- unname(unlist(id_to_token))
  
  # Return the vocabulary as a list
  bpe_vocabulary <- list(
    vocab = vocab,
    merges = merges,
    bio_scores = bio_scores
  )
  return (bpe_vocabulary)
}


.BioTokenizeR_compute_bio_pair_frequencies <- function(id_seqs, bio_scores) {
  
  # Verify that the number of bio-scores match the number of sequences
  if (length(id_seqs) != length(bio_scores)) {
    stop("Length of 'id_seqs' and 'bio_scores' must match.")
  }
  
  # Concatenate all sequences into a single vector, skipping those too short
  lengths <- vapply(id_seqs, length, integer(1))
  if (any(lengths < 2)) {
    id_seqs <- id_seqs[lengths >= 2]
    bio_scores <- bio_scores[lengths >= 2]
    lengths <- lengths[lengths >= 2]
  }
  
  offsets <- cumsum(c(0, lengths))
  all_ids <- unlist(id_seqs)
  n_total <- length(all_ids)
  
  # Prepare left and right adjacent token vectors
  left <- all_ids[-n_total]
  right <- all_ids[-1]
  
  # Mask to keep only pairs within the same sequence
  seq_ends <- offsets[-1]
  mask <- !(seq_along(left) %in% seq_ends)
  left <- left[mask]
  right <- right[mask]
  
  # Assign bio_scores to each pair
  score_vec <- rep(bio_scores, times = lengths - 1)
  score_vec <- score_vec[mask]
  
  # Encode pair as single integers for faster tabulation
  max_id <- max(all_ids) + 1L 
  pair_ids <- left * max_id + right
  
  # Compute weighted counts
  counts <- tapply(score_vec, pair_ids, sum)
  
  # Decode the pair IDs back to two integers
  pair_keys <- as.integer(names(counts))
  a <- pair_keys %/% max_id
  b <- pair_keys %% max_id
  names(counts) <- paste(a, b)
  counts <- sort(counts, decreasing = TRUE)
  
  return (counts)
}


.BioTokenizeR_merge_best_pair <- function(id_seqs, a, b, new_id) {
  
  # Verify that the input was provided appropriately
  if (!is.list(id_seqs)) stop("'id_seqs' must be a list.")
  if (!is.numeric(a) || !is.numeric(b)) stop("'a' and 'b' must be numeric.")
  
  merged_seqs <- lapply(id_seqs, function(seq_ids) {
    
    # Return early if there is nothing to merge
    n <- length(seq_ids)
    if (n < 2) return (seq_ids)
    
    # Identify pairs to merge, returning if no pairs to merge were found
    left <- seq_ids[-n]
    right <- seq_ids[-1]
    merge_position <- which(left == a & right == b)
    if (length(merge_position) == 0) return (seq_ids)
    
    # Create a mask for positions to keep
    keep <- rep(TRUE, n)
    keep[merge_position + 1] <- FALSE
    
    # Replace the first token of the pair with the new merged ID
    seq_ids[merge_position] <- new_id
    
    seq_ids[keep]
  })
  
  return (merged_seqs)
}
