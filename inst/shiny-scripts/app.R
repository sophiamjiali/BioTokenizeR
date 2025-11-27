# ==============================================================================
# Purpose:            Defines the BioTokenizeR Shiny Web App User Interface
# Author:             Sophia Li
# Date:               2025-11-26
# Version:            1.0
# Bugs and Issues:    N/A
# ==============================================================================

library(shiny)
library(bslib)
library(Biostrings)

# # Define the User Interface (UI)
ui <- fluidPage(

  # Theme of the UI
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # Title of main page
  titlePanel("BioTokenizeR: NLP-style Tokenization Explorer"),

  bslib::navset_card_tab(
  
    # -----| Home Tab |-----
    bslib::nav_panel("Home", div(
      class = "card",
      tags$h3("Tokenization Explorer"),
      tags$p(paste0("This Shiny App demonstrates the tokenization workflow. ",
                    "Upload a FASTA file to run the pipeline step-by-step and ",
                    "Inspect intermediate outputs.")),
      tags$ul(
        tags$li("Input & Preprocessing: upload sequences and run preprocessing."),
        tags$li("Annotation: add biological annotations to preprocessed sequences."),
        tags$li("Tokenization: run biology-aware tokenization and inspect tokens."),
        tags$li("Analysis & Visualization: token summaries and exploration.")
      ), tags$p("Expected input format: FASTA file.")
    ))
  )
)

# =====| User Interface |=======================================================

# # Define the User Interface (UI)
# ui <- fluidPage(
# 
#   # Theme of the UI
#   theme = bs_theme(version = 5, bootswatch = "flatly"),
# 
#   # Title of main page
#   titlePanel("BioTokenizeR: NLP-style Tokenization Explorer"),
# 
#   bslib::navset_card_tab(
#   
#     # -----| Home Tab |-----
#     bslib::nav_panel("Home", div(
#       class = "card",
#       tags$h3("Tokenization Explorer"),
#       tags$p(paste0("This Shiny App demonstrates the tokenization workflow. ",
#                     "Upload a FASTA file to run the pipeline step-by-sep and ",
#                     "Inspect intermediate outputs.")),
#       tags$ul(
#         tags$li("Input & Preprocessing: upload sequences and run preprocessing."),
#         tags$li("Annotation: add biological annotations to preprocessed sequences."),
#         tags$li("Tokenization: run biology-aware tokenization and inspect tokens."),
#         tags$li("Analysis & Visualization: token summaries and exploration.")
#       ), tags$p("Expected input format: FASTA file.")
#     )),
#   
#     # -----| Upload Tab |-----
#     bslib::nav_panel("Upload", bslib::page_sidebar(bslib::sidebar("Input",
#   
#         # User-provided FASTA File
#         shiny::fileInput(inputId = "fasta_file",
#                          label   = "Upload a FASTA file (.fa, .fasta)",
#                          accept  = c(".fa", ".fasta")),
#   
#         # User-provided sequence type
#         shiny::selectInput(inputId  = "sequence_type",
#                            label    = "Select sequence type:",
#                            choices  = c("DNA", "RNA", "AA"),
#                            selected = "DNA"),
#   
#         shiny::actionButton(inputId = "load_seqs",
#                             label   = "Load Sequences")
#       ), bslib::card("Sequence Preview", verbatimTextOutput("seq_preview"))
#     )),
#   
#     # -----| Preprocessing Tab |-----
#     bslib::nav_panel("Preprocess", bslib::page_sidebar(bslib::sidebar("Parameters",
#         shiny::actionButton(inputId = "run_preprocess",
#                             label   = "Run Preprocessing")
#       ), bslib::card("Output", verbatimTextOutput("preprocess_out"))
#     )),
#   
#     # -----| Annotation Tab |-----
#     bslib::nav_panel("Annotate", bslib::page_sidebar(bslib::sidebar("Parameters",
#         shiny::actionButton(inputId = "run_annotate",
#                             label   = "Run Annotation")
#       ), bslib::card("Output", verbatimTextOutput("annotate_out"))
#     )),
#   
#     # -----| Tokenization Tab |-----
#     bslib::nav_panel("Tokenize", bslib::page_sidebar(bslib::sidebar("Parameters",
#   
#         # User-provided vocabulary size for NLP-stype tokenization
#         shiny::numericInput(inputId = "vocab_size",
#                             label   = "Enter vocabulary size:",
#                             value   = 10,
#                             min     = 4),
#   
#         shiny::actionButton(inputId = "run_tokenize",
#                             label   = "Run Tokenization")
#       ), bslib::card("Tokens", verbatimTextOutput("token_out"))
#     )),
#   
#     # -----| Analysis Tab |-----
#     bslib::nav_panel("Analyze", bslib::page_sidebar(bslib::sidebar("Parameters",
#         shiny::actionButton(inputId = "run_analysis",
#                             label   = "Run Analysis")
#       ), bslib::card("Analysis Summary", verbatimTextOutput("analysis_out"))
#     )),
#   
#     # -----| Visualization Tab |-----
#     bslib::nav_panel("Visualize", bslib::page_sidebar(bslib::sidebar("Parameters",
#   
#         # User-provided number of tokens to visualize
#         shiny::numericInput(inputId  = "top_n",
#                             label    = "Enter the number of top tokens to visualize:",
#                             value    = 30,
#                             min      = 1),
#   
#         # (Optional) User-provided output directory to save plots
#         shiny::textInput(inputId     = "output_dir",
#                          label       = "Optional: enter an output directory to save plots:",
#                          placeholder = "Leave blank if not saving."),
#   
#         shiny::actionButton(inputId  = "run_visual",
#                             label    = "Generate Visualizations")
#       ), body = tagList(
#         bslib::card("Token Frequency Distribution", plotOutput("plot_freq_dist")),
#         bslib::card("Top Tokens", plotOutput("plot_top_tokens")),
#         bslib::card("Cumulative Coverage", plotOutput("plot_cum_cov"))
#     )))
#   )
# )


# =====| Server |===============================================================

# Define the server logic
server <- function(input, output) {

  # Initialize reactive storage for intermediates
  sequences    <- shiny::reactiveVal(NULL)
  preprocessed <- shiny::reactiveVal(NULL)
  annotated    <- shiny::reactiveVal(NULL)
  tokens       <- shiny::reactiveVal(NULL)
  analyzed     <- shiny::reactiveVal(NULL)

  # # -----| Load the FASTA input sequences |-----
  # shiny::observeEvent(input$load_seqs, {
  #   req(input$sequence_type)
  #   req(input$fasta_file)
  # 
  #   # Load the FASTA into a Biostrings XStringSet object
  #   tryCatch({
  #     seqs <- switch(input$sequence_type,
  #        "DNA" = Biostrings::readDNAStringSet(input$fasta_file$datapath),
  #        "RNA" = Biostrings::readRNAStringSet(input$fasta_file$datapath),
  #        "AA"  = Biostrings::readAAStringSet(input$fasta_file$datapath)
  #     )
  # 
  #     # Set the loaded data into reactive storage and the output object
  #     sequences(seqs)
  #     output$seq_preview <- shiny::renderPrint({seqs})
  # 
  #   }, error = function(e) {
  #     shiny::showNotification(paste0("Failed to read FASTA. Ensure it is a ",
  #                                    "valid and matches the selected type."),
  #                             type = "error")
  #   })
  # })
  # 
  # # -----| Preprocess the input sequences |-----
  # shiny::observeEvent(input$run_preprocess, {
  #   req(sequences())
  # 
  #   # Preprocess the sequences, setting into reactive storage and output
  #   out <- BioTokenizeR::preprocess_sequences(seqs = sequences())
  #   preprocessed(out)
  #   output$preprocess_out <- shiny::renderPrint(out)
  # })
  # 
  # # -----| Annotate |-----
  # shiny::observeEvent(input$run_annotate, {
  #   req(preprocessed())
  # 
  #   # Annotate the sequences, setting into reactive storage and output
  #   out <- BioTokenizeR::annotate_sequences(bioBPE_seqs = preprocessed())
  #   annotated(out)
  #   output$annotate_out <- shiny::renderPrint(out)
  # })
  # 
  # # -----| Tokenization |-----
  # observeEvent(input$run_tokenize, {
  #   req(annotated())
  #   req(input$vocab_size)
  # 
  #   # Tokenize the sequences, setting into reactive storage and output
  #   out <- BioTokenizeR::tokenize_sequences(bioBPE_seqs = annotated(),
  #                                           vocab_size = input$vocab_size)
  #   tokens(out)
  #   output$token_out <- shiny::renderPrint(out)
  # })
  # 
  # # -----| Analysis |-----
  # shiny::observeEvent(input$run_analysis, {
  #   req(tokens())
  # 
  #   # Summarize token statistics, setting into reactive storage and output
  #   out <- BioTokenizeR::summarize_tokens(tokens = tokens())
  #   analyzed(out)
  #   output$analysis_out <- shiny::renderPrint(out)
  # })
  # 
  # # ----| Visualize |-----
  # shiny::observeEvent(input$run_visual, {
  #   req(analyzed())
  #   req(input$top_n)
  # 
  #   # Output directory is optional
  #   output_dir <- ifelse(nzchar(input$output_dir), input$output_dir, NULL)
  # 
  #   # Visualize token summary plots, setting into output only (no reactive)
  #   plots <- BioTokenizeR::visualize_tokens(statistics = analyzed(),
  #                                           top_n = input$top_n,
  #                                           output_dir = output_dir)
  # 
  #   output$plot_freq_dist  <- shiny::renderPlot({plots$frequency_distribution})
  #   output$plot_top_tokens <- shiny::renderPlot({plots$top_tokens})
  #   output$plot_cum_cov    <- shiny::renderPlot({plots$cumulative_coverage})
  # })
  
}

shiny::shinyApp(ui, server)
# [END]
