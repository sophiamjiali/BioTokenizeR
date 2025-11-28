# ==============================================================================
# Purpose:            Visualizes tokenized sequences and summary statistics
# Author:             Sophia Li
# Date:               2025-11-01
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              Provides a wrapper to generate all three plots along with
#                     individual generation and return
# ==============================================================================

# =====| Visualization Wrapper |================================================

#' Visualize Tokenized Sequence Statistics
#'
#' Generates all provided visual summaries of tokenized biological sequences of
#' the package, including token frequency distribution, top tokens, and 
#' cumulative coverage. Vocabulary and tokenized sequence statistics are used
#' for generation.
#'
#' @param statistics An object of class `"bioBPE_summary"` returned by
#'    `summarize_tokens()`.
#' @param top_n Integer specifying the number of top tokens to display in
#'    the top tokens plot. Default is 30.
#' @param output_dir Optional directory to save plots. If `NULL`, plots are
#'    returned but not saved.
#'
#' @return A named list of plots: 
#'    \describe{
#'        \item{`frequency_distribution`}{A line plot visualizing token rank 
#'            (log-scale) versus frequency (log-scale).}
#'        \item{`top_tokens`}{A bar plot visualizing the most frequent tokens.}
#'        \item{`cumulative_coverage`}{A line plot visualizing the cumulative
#'            coverage of ranked tokens.}
#'    }
#'    
#' @details The visualization step includes three plots. The exact operations 
#'    are delegated to the following functions:
#'    \itemize{
#'        \item `plot_token_frequency_distribution` to visualize token frequency
#'        \item `plot_top_tokens` to visualize the top N tokens
#'        \item `plot_cumulative_coverage` to visualize the cumulative coverage
#'            across token ranks
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
#'        summarize  = TRUE,
#'        verbose    = FALSE
#'    )
#'    
#'    # Visualize the tokenized sequences
#'    dna_plots <- visualize_tokens(statistics = data$dna_summary,
#'                                  top_n = 30,
#'                                  output_dir = NULL)
#'    rna_plots <- visualize_tokens(statistics = data$rna_summary,
#'                                  top_n = 30,
#'                                  output_dir = NULL)
#'    aa_plots <- visualize_tokens(statistics = data$aa_summary,
#'                                  top_n = 30,
#'                                  output_dir = NULL)
#' }
#' 
#' @references {
#'     Medvedev A, Viswanathan K, Kanithi P (2025). BioToken and BioFM -
#'     Biologically‑Informed Tokenization Framework. bioRxiv.
#'     https://doi.org/10.1101/2025.03.27.645711
#' }
#'
#' @family visualization
#' @keywords visualization
#' 
#' @export
visualize_tokens <- function(statistics, top_n = 30, output_dir = NULL) {
  
  # Visualize and save all plots
  plots <- list(
    frequency_distribution = (
      plot_token_frequency_distribution(statistics = statistics, 
                                        output_dir = output_dir)
    ),
    top_tokens = (
      plot_top_tokens(statistics = statistics, 
                      top_n      = top_n, 
                      output_dir = output_dir)
    ),
    cumulative_coverage = (
      plot_cumulative_coverage(statistics = statistics, 
                               output_dir = output_dir)
    )
  )
  
  return (plots)
}

# =====| Token Frequency Distribution |=========================================

#' Plot Token Rank-Frequency Distribution
#'
#' Creates a log-log plot of token frequency versus rank, providing a
#' visual overview of token distribution across the corpus.
#'
#' @param statistics An object of class `"bioBPE_summary"` containing
#'    token frequency information.
#' @param output_dir Optional directory to save the plot. If `NULL`, the plot
#'    is returned but not saved.
#'
#' @return A `ggplot2` object representing the rank-frequency distribution
#'    of tokens.
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
#'        summarize  = TRUE,
#'        verbose    = FALSE
#'    )
#'    
#'    # Visualize token frequency distribution
#'    plot <- plot_token_frequency_distribution(statistics = data$dna_summary,
#'                                              output_dir = NULL)
#' }
#' 
#' @references {
#'     Müller K, Wickham H (2025). _tibble: Simple Data Frames_. 
#'     doi:10.32614/CRAN.package.tibble <https://doi.org/10.32614/CRAN.package.tibble>, 
#'     R package version 3.3.0, <https://CRAN.R-project.org/package=tibble>.
#' 
#'     Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. 
#'     Springer-Verlag New York. ISBN 978-3-319-24277-4, 
#'     https://ggplot2.tidyverse.org.
#' }
#'
#' @family visualization
#' @keywords visualization
#' 
#' @import ggplot2
#' @importFrom tibble tibble
#' @export
plot_token_frequency_distribution <- function(statistics, output_dir = NULL) {
  
  # Verify that the statistics provided includes token frequencies
  token_freq <- statistics$token_summary$frequency
  if (is.null(token_freq)) stop("'statistics' must include token frequencies.")
  
  # Rank-transform the frequencies
  token_freq <- tibble::tibble(rank = seq_along(token_freq), 
                               freq = as.numeric(token_freq))
  
  # Plot token lengths as a line-plot
  line_plot <- ggplot2::ggplot(token_freq, ggplot2::aes(x = rank, y = freq)) +
    ggplot2::geom_line(color = "#1B4F72", linewidth = 1.1) +
    ggplot2::scale_x_log10() + 
    ggplot2::scale_y_log10() +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(
      title = "Token Rank-Frequency Distribution (log-scale)",
      x     = "Token Rank (log-scale)", 
      y     = "Frequency (log-scale)"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                      hjust = 0.5))
  
  # Save the plot if an output directory was provided, initializing if necessary
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    ggplot2::ggsave(
      filename = file.path(output_dir, "token_frequency_distribution.png"),
      plot     = line_plot,
      dpi      = 300,
      width    = 6,
      height   = 4
    )
  }
  
  return (line_plot)
}


# =====| Top N Most Frequent Tokens |===========================================

#' Plot Top N Most Frequent Tokens
#'
#' Generates a horizontal bar plot showing the top `N` most frequent tokens
#' in the tokenized sequences.
#'
#' @param statistics An object of class `"bioBPE_summary"` containing token
#'    frequency data.
#' @param top_n Integer specifying the number of top tokens to display.
#' @param output_dir Optional directory to save the plot. If `NULL`, the plot
#'    is returned but not saved.
#'
#' @return A `ggplot2` object representing the top `N` most frequent tokens.
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
#'        summarize  = TRUE,
#'        verbose    = FALSE
#'    )
#'    
#'    # Visualize the top 10 tokens
#'    plot <- plot_top_tokens(statistics = data$dna_summary,
#'                            top_n      = 10,
#'                            output_dir = NULL)
#' }
#' 
#' @references {
#'     R Core Team (2025). R: A Language and Environment for Statistical
#'     Computing. R Foundation for Statistical Computing, Vienna, Austria.
#'     https://www.R-project.org/.
#'     
#'     Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. 
#'     Springer-Verlag New York. ISBN 978-3-319-24277-4, 
#'     https://ggplot2.tidyverse.org.
#' 
#'     Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A 
#'     Grammar of Data Manipulation_. doi:10.32614/CRAN.package.dplyr 
#'     <https://doi.org/10.32614/CRAN.package.dplyr>, R package version 1.1.4,
#'     <https://CRAN.R-project.org/package=dplyr>.
#' }
#'
#' @family visualization
#' @keywords visualization
#' 
#' @import ggplot2
#' @importFrom dplyr arrange desc slice_head
#' @importFrom stats reorder
#' @export
plot_top_tokens <- function(statistics, top_n = 30, output_dir = NULL) {
  
  # Verify that the statistics provided include token summaries
  token_summary <- statistics$token_summary
  if (is.null(token_summary)) stop("'statistics' must include token summary.")
  
  # Extract the top N tokens
  top_tokens <- token_summary |>
                dplyr::arrange(dplyr::desc(frequency)) |>
                dplyr::slice_head(n = top_n)
  
  # Plot the top N most frequent tokens as a bar plot
  bar_plot <- ggplot2::ggplot(top_tokens, 
                              ggplot2::aes(x = stats::reorder(token, frequency), 
                                          y = frequency)) +
    ggplot2::geom_bar(
      stat      = "identity", 
      fill      = "#2E86AB", 
      color     = "black", 
      linewidth = 0.3
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(
      title = paste("Top", top_n, "Most Frequent Tokens"),
      x     = "Token",
      y     = "Frequency"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                      hjust = 0.5))
  
  # Save the plot if an output directory was provided, initializing if necessary
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    ggplot2::ggsave(
      filename = file.path(output_dir, "top_tokens.png"),
      plot     = bar_plot,
      dpi      = 300,
      width    = 6,
      height   = 6
    )
  }
  
  return (bar_plot)
} 


# =====| Cumulative Coverage |==================================================

#' Plot Cumulative Token Frequency Coverage
#'
#' Generates a line plot showing the cumulative frequency coverage of tokens
#' in the tokenized sequences, ranked from most to least frequent.
#'
#' @param statistics An object of class `"bioBPE_summary"` containing token
#'    frequency data.
#' @param output_dir Optional directory to save the plot. If `NULL`, the plot
#'    is returned but not saved.
#'
#' @return A `ggplot2` object representing cumulative token frequency coverage.
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
#'        summarize  = TRUE,
#'        verbose    = FALSE
#'    )
#'    
#'    # Visualize the cumulative coverage of all tokens
#'    plot <- plot_cumulative_coverage(statistics = data$dna_summary,
#'                                     output_dir = NULL)
#' }
#' 
#' @references {
#'     Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. 
#'     Springer-Verlag New York. ISBN 978-3-319-24277-4, 
#'     https://ggplot2.tidyverse.org.
#' 
#'     Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A 
#'     Grammar of Data Manipulation_. doi:10.32614/CRAN.package.dplyr 
#'     <https://doi.org/10.32614/CRAN.package.dplyr>, R package version 1.1.4,
#'     <https://CRAN.R-project.org/package=dplyr>.
#' }
#'
#' @family visualization
#' @keywords visualization plotting tokenization
#'
#' @import ggplot2
#' @importFrom dplyr arrange desc mutate
#' @export
plot_cumulative_coverage <- function(statistics, output_dir = NULL) {
  
  # Verify that the statistics provided include token summaries
  token_summary <- statistics$token_summary
  if (is.null(token_summary)) stop("'statistics' must include token summary.")
  
  # Compute the cumulative coverage of each token
  token_summary <- token_summary |>
                   dplyr::arrange(dplyr::desc(frequency)) |>
                   dplyr::mutate(cumulative = cumsum(frequency) / sum(frequency))
  
  # Plot the cumulative coverage of each frequency
  line_plot <- ggplot2::ggplot(token_summary, 
                               ggplot2::aes(x = seq_along(frequency), 
                                           y = cumulative)) +
    ggplot2::geom_line(color = "#1B4F72", linewidth = 1.1) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(
      title = "Cumulative Token Frequency Coverage",
      x     = "Ranked Tokens",
      y     = "Cumulative Frequency"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                      hjust = 0.5))
  
  # Save the plot if an output directory was provided, initializing if necessary
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) { dir.create(output_dir, recursive = TRUE) }
    
    ggplot2::ggsave(
      filename = file.path(output_dir, "cumulative_coverage.png"),
      plot     = line_plot,
      dpi      = 300,
      width    = 6,
      height   = 4
    )
  }
  
  return (line_plot)
}

# [END]