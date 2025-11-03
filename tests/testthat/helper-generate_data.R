# ==============================================================================
# Purpose:            Helper functions to generate data for testing
# Author:             Sophia Li
# Date:               2025-11-02
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Built for integration with the usethis package
# ==============================================================================

# =====| Sequence Generation |==================================================

generate_sequences <- function(type = NULL, n = 1, length = 21) {
  
  # Generate a set of n sequences of the given type
  if (type == "DNA") {
    seqs <- DNAStringSet(replicate(n = n, expr = paste0(
          sample(x = Biostrings::DNA_ALPHABET, size = length, replace = TRUE), 
          collapse = ""
    )))
    
  } else if (type == "RNA") {
    seqs <- RNAStringSet(replicate(n = n, expr = paste0(
      sample(x = Biostrings::RNA_ALPHABET, size = length, replace = TRUE), 
      collapse = ""
    )))
    
  } else if (type == "AA") {
    seqs <- AAStringSet(replicate(n = n, expr = paste0(
      sample(x = Biostrings::AA_ALPHABET, size = length, replace = TRUE), 
      collapse = ""
    )))
    
  } else {
    stop("'type' must be DNA, RNA, or AA.")
  }
  
  return (seqs)
}