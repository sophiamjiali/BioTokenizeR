# ==============================================================================
# Purpose:            Analyzes BPE-tokenized sequences
# Author:             Sophia Li
# Date:               2025-11-01
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Added GC-content summary only due to time constraints
# ==============================================================================

summarize_tokens <- function(tokens) {
  
  # Verify the input is a list of strings and is non-empty
  if (length(tokens) == 0) stop("'tokens' must not be empty.")
  if (!is.character(tokens)) stop("'tokens' must be a character vector.")
  
  # Split the tokenized sequences into a single list of tokens
  tokens <- strsplit(tokens, " ")
  tokens <- unlist(tokens)
  
  # Compute basic corpus-level statistics
  num_sequences <- length(tokens)
  total_tokens <- length(all_tokens)
  avg_seq_length <- mean(lengths(token_lists))
  median_seq_length <- median(lengths(token_lists))
  
  # Compute token-level frequency summary
  token_freq <- sort(table(all_tokens), decreasing = TRUE)
  vocab_size <- length(token_freq)
  
  # Compute token length summary
  token_length <- nchar(names(token_freq))
  token_length_summary <- summary(token_length)
  
  # Compute biological summaries
  gc_like <- grepl("[GC]", names(token_freq))
  gc_token_fraction <- mean(gc_like)
  
  
  # Return a summary object
  statistics <- list(
    corpus = list(
      num_sequences = num_sequences,
      total_tokens = total_tokens,
      avg_seq_length = avg_seq_length,
      median_seq_length = median_seq_length,
      vocab_size = vocab_size
    ),
    token_summary = data.frame(
      token = names(token_freq),
      frequency = as.numeric(token_freq),
      length = token_length,
      gc_like = gc_like,
      origin = token_origin
    ),
    token_length_summary = token_length_summary
  )
  class(statistics) <- "bioBPE_summary"
  
  return (statistics)
}
