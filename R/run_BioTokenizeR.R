# ==============================================================================
# Purpose:            Launches the Shiny app for the BioTokenizeR Package
# Author:             Sophia Li
# Date:               2025-11-26
# Version:            1.0
# Bugs and Issues:    N/A
# ==============================================================================

#' Launch Shiny App for BioTokenizeR
#' 
#' This function launches the Shiny app for the BioTokenizeR package, displaying
#' an interactive web app user interface.
#' 
#' @examples 
#' \dontrun{
#'     BioTokenizeR::run_BioTokenizeR()
#' }
#' 
#' @importFrom shiny runApp
#' @export
run_BioTokenizeR <- function() {
  app_location <- system.file("shiny-scripts", package = "BioTokenizeR")
  action_shiny <- shiny::runApp(app_location, display.mode = "normal")
  return (action_shiny)
}
# [END]