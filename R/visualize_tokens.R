# ==============================================================================
# Purpose:            Visualizes tokenized sequences and summary statistics
# Author:             Sophia Li
# Date:               2025-11-01
# Version:            1.0
# Bugs and Issues:    N/A
# Notes:              
# ==============================================================================

# =====| Visualization Wrapper |================================================

visualize_tokens <- function(statistics, top_n = 30, output_dir = NULL) {
  
  # Visualize and save all plots
  plots <- list(
    frequency_distribution = plot_token_frequency_distribution(
      statistics = statistics, output_dir = output_dir
    ),
    top_tokens             = plot_top_tokens(
      statistics = statistics, top_n = top_n, output_dir = output_dir
    ),
    cumulative_coverage    = plot_cumulative_coverage(
      statistics = statistics, output_dir = output_dir
    )
  )
  
  return (plots)
}

# =====| Token Frequency Distribution |=========================================

plot_token_frequency_distribution <- function(statistics, output_dir = NULL) {
  
  # Verify that the statistics provided includes token frequencies
  token_freq <- statistics$token_summary$frequency
  if (is.null(token_freq)) stop("'statistics' must include token frequencies.")
  
  # Rank-transform the frequencies
  token_freq <- tibble::tibble(rank = seq_along(token_freq), 
                               freq = as.numeric(token_freq))
  
  # Plot token lengths as a line-plot
  bar_plot <- ggplot2::ggplot(token_freq, ggplot2::aes(x = rank, y = freq)) +
    ggplot2::geom_line(color = "#1B4F72", linewidth = 1.1) +
    ggplot2::scale_y_log10() +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(
      title = "Token Rank-Frequency Distribution",
      x     = "Token Rank (log-scale)", 
      y     = "Frequency (log-scale)"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  
  # Save the plot if an output directory was provided, initializing if necessary
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    ggplot2::ggsave(
      filename = file.path(output_dir, "token_frequency_distribution.png"),
      plot     = bar_plot,
      dpi      = 300,
      width    = 6,
      height   = 4
    )
  }
  
  return (bar_plot)
}


# =====| Top N Most Frequent Tokens |===========================================

plot_top_tokens <- function(statistics, top_n = 30, output_dir = NULL) {
  
  # Verify that the statistics provided include token summaries
  token_summary <- statistics$token_summary
  if (is.null(token_summary)) stop("'statistics' must include token summary.")
  
  # Extract the top N tokens
  top_tokens <- token_summary %>% 
                dplyr::arrange(desc(frequency)) %>%
                dplyr::slice_head(n = top_n)
  
  # Plot the top N most frequent tokens as a bar plot
  bar_plot <- ggplot2::ggplot(top_tokens, 
                              ggplot2::aes(x = reorder(token, frequency), 
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
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  
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
  
  return (top_tokens)
} 


# =====| Cumulative Coverage |==================================================

plot_cumulative_coverage <- function(statistics, output_dir = NULL) {
  
  # Verify that the statistics provided include token summaries
  token_summary <- statistics$token_summary
  if (is.null(token_summary)) stop("'statistics' must include token summary.")
  
  # Compute the cumulative coverage of each token
  token_summary <- token_summary %>% 
                   dplyr::arrange(desc(frequency)) %>%
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
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  
  # Save the plot if an output directory was provided, initializing if necessary
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
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
