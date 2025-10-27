# ==============================================================================
# Purpose:            S3 class to store preprocessed sequences for tokenization
# Author:             Sophia Li
# Date:               2025-10-27
# Version:            1.0
# Bugs and Issues:    N/A
# ==============================================================================

# =====| Class Definition |=====================================================

bioBPE_preprocessed <- function(seqs,
                                preproc_steps = list(),
                                annot_steps = list(),
                                metadata = list()) {
  
  # Verify that the sequences provided are a Biostrings sequence set object
  if (!inherits(seqs, "XStringSet")) {
    stop(paste0(
      "'seqs' must be a Biostrings::XStringSet (e.g., DNAStringSet, ",
      "RNAStringSet, AAStringSet)."
    ))
  }
  
  # Verify that the sequences object is non-empty
  if (length(seqs) == 0) {
    stop("'seqs' cannot be empty.")
  }
  
  
  # Fetch the sequence type
  type <- switch(class(seqs),
                 DNAStringSet = "DNA",
                 RNAStringSet = "RNA",
                 AAStringSet = "")

  # Initialize metadata if not provided
  if (is.null(metadata$timestamp)) {
    metadata$timestamp <- Sys.time()
  }
  
  if (is.null(metadata$alphabet)) {
    metadata$alphabet <- Biostrings::alphabetFrequency(seqs)
  }
  
  # Initialize the S3 wrapper class
  return (structure(list(
      seqs = seqs,
      type = type, 
      metadata = metadata,
      preproc_steps = preproc_steps,
      annot_steps = NULL
    ), 
    class = "bioBPE_preprocessed"
  ))
}


# =====| Core Methods |=====================================================

print.bioBPE_preprocesed <- function(x, ...) {
  cat("<bioBPE_processed>\n")
  cat("  Sequences :", length(x$seqs), "\n")
  cat("  Type      :", class(x$seqs)[1], "\n")
  cat("  Steps     :", paste(x$preproc_steps, collapse = ", "), "\n")
  invisible(x)
}

as.character.bioBPE_preprocessed <- function(x, ...) {
  return (as.character(x$seqs))
}

as.XStringSet.bioBPE_preprocessed <- function(x, ...) {
  return (x$seqs)
}

length.bioBPE_preprocessed <- function(x) {
  return (length(x$seqs))
}

