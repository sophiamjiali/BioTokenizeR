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

#' Tokenize Biological Sequences Using BPE
#'
#' This function learns a byte-pair encoding (BPE) vocabulary from preprocessed 
#' sequences and applies it to generate tokenized representations for downstream 
#' analysis. A vocabulary of the specified size is first learned from the 
#' sequence dataset, through which merges are learned in order and applied to
#' tokenize the sequences and returned. This vocabulary and the tokenized 
#' sequences are used for downstream analysis in the pipeline.
#'
#' @param bioBPE_seqs A `bioBPE_preprocessed` object containing preprocessed 
#'    DNA, RNA, or AA sequences.
#' @param vocab_size An integer specifying the maximum size of the learned BPE 
#'    vocabulary (default is 15).
#'
#' @return A list containing:
#'    \describe{
#'        \item{`vocab`}{The learned BPE vocabulary, including merges and base 
#'            tokens.}
#'        \item{`tokens`}{A list of tokenized sequences, where each sequence is 
#'            a vector of token strings.}
#'    }
#'    
#' @details The tokenization step includes learning the BPE vocabulary and 
#'    applying it to the sequences. The exact operations are delegated into
#'    internal helper functions:
#'    \itemize{
#'        \item `.BioTokenizeR_apply_bpe()` to apply the learned vocabulary to
#'            the sequences.
#'        \item `.BioTokenizeR_learn_bpe_vocabulary` to learn the BPE vocabulary
#'        \item `.BioTokenizeR_compute_bio_pair_frequencies` to compute 
#'            biology-aware token pair frequencies within the BPE algorithm
#'        \item `.BioTokenizeR_merge_best_pair` to merge the best token pair
#'            within the BPE algorithm
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
#'        tokenize   = FALSE,
#'        summarize  = FALSE,
#'        verbose    = FALSE
#'    )
#'    
#'    # Tokenize the preprocessed and annotated sequences
#'    dna_tokens <- tokenize_sequences(bioBPE_seqs = data$dna_annot,
#'                                     vocab_size  = 15)
#'    rna_tokens <- tokenize_sequences(bioBPE_seqs = data$rna_annot,
#'                                     vocab_size  = 15)
#'    aa_tokens <- tokenize_sequences(bioBPE_seqs = data$aa_annot,
#'                                    vocab_size  = 15)
#' }
#' 
#' @references {
#'     Medvedev A, Viswanathan K, Kanithi P (2025).BioToken and BioFM - 
#'     Biologically‑Informed Tokenization Framework. bioRxiv. 
#'     https://doi.org/10.1101/2025.03.27.645711
#' }
#'
#' @family tokenization
#' @keywords tokenization internal
#' 
#' @export
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
    seqs  = as.character(bioBPE_seqs$seqs), 
    vocab = vocab
  )
  
  return (list(
    vocab  = vocab, 
    tokens = tokens
  ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Apply Learned BPE Vocabulary to Sequences
#'
#' Converts sequences into tokenized form using a learned byte-pair encoding (BPE)
#' vocabulary. Base tokens are first converted to integer IDs, and learned merges
#' are applied sequentially to generate final tokenized sequences.
#'
#' @param seqs A character vector of sequences to tokenize.
#' @param vocab A list returned by `.BioTokenizeR_learn_bpe_vocabulary` 
#'    containing base tokens and the learned merge operations.
#'
#' @return A list of tokenized sequences, where each element is a character  
#'    vector of tokens corresponding to a single sequence.
#'    
#' @references {
#'    Medvedev A, Viswanathan K, Kanithi P (2025). BioToken and BioFM - 
#'    Biologically‑Informed Tokenization Framework. bioRxiv. 
#'    https://doi.org/10.1101/2025.03.27.645711
#'    
#'    R Core Team (2025). R: A Language and Environment for Statistical
#'    Computing. R Foundation for Statistical Computing, Vienna, Austria.
#'    https://www.R-project.org/.
#' }
#'
#' @family tokenization
#' @keywords tokenization internal
#' 
#' @importFrom stats setNames
.BioTokenizeR_apply_bpe <- function(seqs, vocab) {
  
  # Verify that the input was provided appropriately
  if (!is.character(seqs)) stop("'seqs' must be a character vector.")
  if (!is.list(vocab)) {
    stop(paste0("'vocab' must be a list returned by ",
                ".BioTokenizeR_learn_bpe_vocabulary."))
  }
  
  # Initialize integer sequences using the base token mapping
  base_tokens <- vocab$vocab[1:(length(vocab$vocab) - length(vocab$merges))]
  token_to_id <- stats::setNames(seq_along(base_tokens), base_tokens)
  id_to_token <- stats::setNames(base_tokens, seq_along(base_tokens))
  
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

#' Learn Byte-Pair Encoding (BPE) Vocabulary from Biological Sequences
#'
#' Learns a BPE vocabulary from preprocessed biological sequences by 
#' iteratively identifying the most frequent adjacent token pairs and merging 
#' them into new tokens until the specified vocabulary size is reached. 
#' The process is weighted by biological scores derived from sequence annotations.
#'
#' @param bioBPE_seqs A `bioBPE_preprocessed` object containing sequences and 
#'    annotation metadata.
#' @param vocab_size Integer specifying the desired maximum size of the learned 
#'    BPE vocabulary (default 15).
#'
#' @return A list containing:
#'    \describe{
#'        \item{vocab}{Character vector of all tokens in the learned BPE vocabulary.}
#'        \item{merges}{List of integer pairs representing the merge operations 
#'            applied.}
#'        \item{bio_scores}{Numeric vector of biological scores associated with each
#'            sequence.}
#'    }
#'    
#' @references {
#'    Medvedev A, Viswanathan K, Kanithi P (2025). BioToken and BioFM - 
#'    Biologically‑Informed Tokenization Framework. bioRxiv. 
#'    https://doi.org/10.1101/2025.03.27.645711
#'    
#'    R Core Team (2025). R: A Language and Environment for Statistical
#'    Computing. R Foundation for Statistical Computing, Vienna, Austria.
#'    https://www.R-project.org/.
#' }
#'
#' @family tokenization
#' @keywords tokenization internal
#' 
#' @importFrom stats setNames
.BioTokenizeR_learn_bpe_vocabulary <- function(bioBPE_seqs, vocab_size = 15) {
  
  # Initialize sequences as integer token vectors, initializing a mapping
  seqs <- as.character(bioBPE_seqs$seqs)
  vocab <- unique(unlist(strsplit(seqs, "")))

  token_to_id <- stats::setNames(seq_along(vocab), vocab)
  id_to_token <- stats::setNames(vocab, seq_along(vocab))
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Compute Biologically-Weighted Pair Frequencies for BPE
#'
#' Calculates the frequency of all adjacent token pairs in a set of integer-
#' encoded sequences, weighting each pair by a biological score associated 
#' with its sequence. Pairs are represented as space-separated strings of 
#' integer token IDs.
#'
#' @param id_seqs A list of integer vectors representing tokenized sequences.
#' @param bio_scores Numeric vector of biological scores, one per sequence, 
#'    used to weight the pair frequencies.
#'
#' @return A named numeric vector of weighted pair counts, with names being 
#'    space-separated token ID pairs (e.g., `"1 2"`), sorted in decreasing order.
#'    
#' @references {
#'    Medvedev A, Viswanathan K, Kanithi P (2025).BioToken and BioFM - 
#'    Biologically‑Informed Tokenization Framework. bioRxiv. 
#'    https://doi.org/10.1101/2025.03.27.645711
#' }
#'
#' @family tokenization
#' @keywords tokenization internal
.BioTokenizeR_compute_bio_pair_frequencies <- function(id_seqs, bio_scores) {
  
  # Verify that the number of bio-scores match the number of sequences
  if (length(id_seqs) != length(bio_scores)) {
    stop("Length of 'id_seqs' and 'bio_scores' must match.")
  }
  
  # Filter out sequences too short to form pairs
  keep <- vapply(id_seqs, length, integer(1)) >= 2
  if (!any(keep)) return (numeric(0))
  id_seqs <- id_seqs[keep]
  bio_scores <- bio_scores[keep]
  
  # Build vectors of adjacent pairs and repeat bio_scores for each pair
  left <- unlist(lapply(id_seqs, function(x) x[-length(x)]))
  right <- unlist(lapply(id_seqs, function(x) x[-1]))
  score_vec <- unlist(mapply(function(ids, s) rep(s, length(ids) - 1),
                             id_seqs, bio_scores, SIMPLIFY = FALSE))
  
  # Encode pairs as strings for counting
  pair_keys <- paste(left, right)
  
  # Compute weighted counts per pair
  pair_counts <- tapply(score_vec, pair_keys, sum)
  pair_counts <- sort(pair_counts, decreasing = TRUE)
  
  return (pair_counts)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Merge a Specified Token Pair in Integer-Encoded Sequences
#'
#' Replaces all occurrences of a specified adjacent token pair `(a, b)` in a 
#' list of integer-encoded sequences with a new merged token ID. Sequences 
#' that are too short or do not contain the pair are returned unchanged.
#'
#' @param id_seqs A list of integer vectors representing tokenized sequences.
#' @param a Integer ID of the first token in the pair to merge.
#' @param b Integer ID of the second token in the pair to merge.
#' @param new_id Integer ID to assign to the merged token.
#'
#' @return A list of integer vectors, with the specified pairs replaced by 
#'    `new_id`.
#'    
#' @references {
#'    Medvedev A, Viswanathan K, Kanithi P (2025).BioToken and BioFM - 
#'    Biologically‑Informed Tokenization Framework. bioRxiv. 
#'    https://doi.org/10.1101/2025.03.27.645711
#' }
#'
#' @family tokenization
#' @keywords tokenization internal
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

# [END]