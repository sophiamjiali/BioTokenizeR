# ==============================================================================
# Purpose:            Analyzes BPE-tokenized sequences
# Author:             Sophia Li
# Date:               2025-11-01
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Added GC-content summary only due to time constraints
# ==============================================================================

#' Summarize Tokenized Biological Sequences
#'
#' Computes summary statistics for a list of tokenized biological sequences,
#' including corpus-level metrics, token frequencies, token lengths, and
#' basic biological features (e.g., GC-like tokens).
#'
#' @param tokens A non-empty list of character vectors representing tokenized
#'    sequences.
#'
#' @return An object of class `"bioBPE_summary"`, being a list containing:
#'    \describe{
#'        \item{`corpus`}{List of corpus-level statistics: number of sequences,
#'            total tokens, average and median sequence lengths, and vocabulary 
#'            size.}
#'        \item{`token_summary`}{Data frame with each token's frequency, length,
#'            and GC-like flag.}
#'        \item{`token_length_summary`}{Summary of token lengths.}
#'    }
#'    
#' @examples
#' \dontrun{
#'    # Generate simulated data
#'    data <- generate_data(
#'        n          = 3, 
#'        length     = 1000, 
#'        vocab_size = 25, 
#'        preprocess = TRUE,
#'        annotate   = TRUE,
#'        tokenize   = TRUE,
#'        summarize  = FALSE,
#'        verbose    = FALSE
#'    )
#'    
#'    # Summarize the preprocessed, annotated, and tokenized sequences
#'    dna_summary <- summarize_tokens(tokens = data$dna_tokens$tokens)
#'    rna_summary <- summarize_tokens(tokens = data$rna_tokens$tokens)
#'    aa_summary <- summarize_tokens(tokens = data$aa_tokens$tokens)
#' }
#' 
#'
#' @family summary
#' @keywords summary
#' 
#' @importFrom stats median
#' @export
summarize_tokens <- function(tokens) {
  
  # Verify the input is a non-empty list of character vectors
  if (!is.list(tokens) || length(tokens) == 0) {
    stop("'tokens' must be a non-empty list.")
  }
  
  # Verify all elements of the tokenized sequences are character vectors
  if (!all(vapply(tokens, is.character, logical(1)))) {
    stop("All elements of 'tokens' must be character vectors.")
  }
  
  # Flatten all tokens into one vector for global analysis
  all_tokens <- unlist(tokens, use.names = FALSE)
  
  # Compute basic corpus-level statistics
  num_sequences <- length(tokens)
  total_tokens <- length(all_tokens)
  avg_seq_length <- mean(lengths(tokens))
  median_seq_length <- stats::median(lengths(tokens))
  
  # Compute token-level frequency summary
  token_freq <- sort(table(all_tokens), decreasing = TRUE)
  vocab_size <- length(unique(token_freq))
  
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
      gc_like = gc_like
    ),
    token_length_summary = token_length_summary
  )
  class(statistics) <- "bioBPE_summary"
  
  return (statistics)
}

# [END]