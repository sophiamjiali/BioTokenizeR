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
#' @returns No return value. Opens a Shiny page.
#' 
#' @examples 
#' \dontrun{
#'     BioTokenizeR::run_BioTokenizeR()
#' }
#' 
#' @references {
#'     Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, 
#'     McPherson J, Dipert A, Borges B (2025). _shiny: Web Application Framework 
#'     for R_. doi:10.32614/CRAN.package.shiny 
#'     <https://doi.org/10.32614/CRAN.package.shiny>, R package
#'     version 1.11.1, <https://CRAN.R-project.org/package=shiny>.
#'     
#'     Grolemund, G. (2015). Learn Shiny - Video Tutorials. 
#'     https://shiny.rstudio.com/tutorial/.
#' }
#' 
#' @family shiny
#' @keywords shiny
#' 
#' @importFrom shiny runApp
#' @export
run_BioTokenizeR <- function() {
  app_location <- system.file("shiny-scripts", package = "BioTokenizeR")
  action_shiny <- shiny::runApp(app_location, display.mode = "normal")
  return (action_shiny)
}

# [END]