

bioBPE_preprocessed <- function(seqs,
                                type = character(0),
                                preproc_steps = character(0),
                                metadata = list()) {
  
  # Verify that the sequences provided are a Biostrings sequence set object
  if (!inherits(seqs, "XStringSet")) {
    stop("'seqs' must be a Biostrings::XStringSet (e.g., DNAStringSet, RNAStringSet, AAStringSet).")
  }
  
  # Initialize metadata if not provided
  if (is.null(metadata$timestamp)) metadata$timestamp <- Sys.time()
  if (is.null(metadata$alphabet)) metadata$alphabet <- Biostrings::alphabetFrequency(seqs)
  
  # Initialize the S3 wrapper class
  return (
    structure(
      list(
        seqs = seqs,
        type = type.
        preproc_steps = preproc_steps,
        metadata = metadata
      ), 
      class = "bioBPE_preprocessed"
    )
  )
}



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

