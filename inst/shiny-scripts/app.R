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

# =====| User Interface |=======================================================

# # Define the User Interface (UI)
ui <- page_fluid(

  # Theme of the UI
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # Title of main page
  titlePanel("BioTokenizeR: NLP-style Tokenization Explorer"),

  # Define the main navigation as tabs per step in the pipeline
  bslib::navset_card_tab(
    
    # -----| Overview: default landing page |-----------------------------------
    bslib::nav_panel(title = "Overview", bslib::card(
      
      # Main title description
      shiny::tags$h3(paste0("Welcome to BioTokenizeR: NLP-style Biology-Aware ",
                            "Tokenization")),
      shiny::tags$p(paste0("This application demonstrates the BioTokenizeR ",
                           "workflow for processing biological sequences using ",
                           "NLP-style tokenization that incorporates ",
                           "biological annotations.")),
      shiny::tags$hr(),
      
      # Workflow description
      shiny::tags$h4("Workflow"),
      shiny::tags$ul(
        shiny::tags$li(paste0("Input: upload a FASTA file (DNA, RNA, AA) and ",
                              "indicate sequence type")),
        shiny::tags$li(paste0("Preprocessing: run preprocessing and ",
                              "annotation")),
        shiny::tags$li(paste0("Tokenization: tokenize sequences using a ", 
                              "biology-aware BPE algorithm")),
        shiny::tags$li(paste0("Analysis: inspect token statistics and ",
                              "summaries")),
        shiny::tags$li(paste0("Visualize: inspect token visualizations"))
      ),
      
      # Input and output descriptions
      shiny::tags$h4("Input Requirements"),
      shiny::tags$ul(
        shiny::tags$li("Valid FASTA file (.fa or .fasta)"),
        shiny::tags$li("All sequences must be of the same type (DNA, RNA, AA)"),
        shiny::tags$li("Sequence type must match what is provided in the FASTA")
      ),
      shiny::tags$h4("Output"),
      shiny::tags$ul(
        shiny::tags$li("Tokenized sequences"),
        shiny::tags$li("Token frequency summaries"),
        shiny::tags$li("Visualization of token distributions")
      )
    )),
    
    # -----| Upload: upload FASTA files |---------------------------------------
    bslib::nav_panel("Upload Sequences", shiny::fluidRow(
      
      # Upload, preprocess, and annotate sidebar
      shiny::column(width = 3,
        shiny::tags$h4("Upload Sequences"),
        shiny::hr(),
        shiny::tags$p(paste0("Upload biological sequences for downstream ",
                             "tokenization and analysis.")),
        shiny::fileInput(inputId    = "fasta_file", 
                         label      = "Upload FASTA sequence(s) file",
                         accept     = c(".fa", ".fasta")),
        shiny::selectInput(inputId  = "sequence_type",
                           label    = "Select sequence type:",
                           choices  = c("DNA", "RNA", "AA"),
                           selected = "DNA"),
        shiny::actionButton(inputId = "run_preprocess", 
                            label   = "Preprocess Sequences")),
      
      shiny::column(width = 1),
      
      # Download sample data
      shiny::column(width = 7,
        shiny::tags$h3("Upload Sequences for Preprocessing"),
        shiny::hr(),
        shiny::br(),
        shiny::uiOutput("upload_message"),
        shiny::br(),
        shiny::hr(),
        shiny::tags$h4("Sample Data"),
        shiny::tags$p(paste0("Sample data of DNA, RNA, and AA sequences are ", 
                             "included in the package. Indicate below whether ",
                             "or not to use the example data for testing.")),
        shiny::selectInput(inputId = "use_sample_data",
                           label   = "Select Sample Data",
                           placeholder = "Do Not Use Sample Data")
        
        
        
        shiny::tags$p(paste0("Sample data for DNA, RNA, and AA sequences are ",
                             "included in the package under 'inst/extdata/.")),
        shiny::tags$p(paste0("Users can also access the raw files on GitHub at",
                             ": 'https://github/com/sophiamjiali/BioTokenizeR",
                             "/tree/main/inst/extdata")),
        
        

        shiny::downloadButton(outputId = "download_dna", 
                              label    = "Download Sample DNA FASTA File",
                              style    = "margin-bottom: 10px;"),
        shiny::downloadButton(outputId = "download_rna", 
                              label    = "Download Sample RNA FASTA File",
                              style    = "margin-bottom: 10px;"),
        shiny::downloadButton(outputId = "download_aa", 
                              label    = "Download Sample AA FASTA File",
                              style    = "margin-bottom: 10px;"),
        shiny::hr())
    )),
        
    
    # -----| Token Summary: displays summary statistics |-----------------------
    bslib::nav_panel("Token Summary", shiny::fluidRow(
      
      # Run tokenization sidebar
      shiny::column(width = 3,
        shiny::tags$h4("Tokenize Sequences"),
        shiny::hr(),
        shiny::tags$p(paste0("Tokenization can be performed after sequences are",
                             " uploaded, preprocessed, and annotated.")),
        shiny::br(),
        shiny::numericInput(inputId = "vocab_size",
                            label   = "Enter Vocabulary Size",
                            value   = 10,
                            min     = 4),
        shiny::br(),
        shiny::actionButton(inputId = "run_tokenize",
                            label   = "Run BPE Tokenization")),
      
      shiny::column(width = 1),
      
      # Token analysis UI
      shiny::column(width = 7,
        shiny::tags$h3("Tokenization Result Summary"),
        shiny::hr(),
        shiny::uiOutput("token_summary_ui"))
    )),
    
    # -----| Visualizations: key visuals of the pipeline |----------------------
    bslib::nav_panel("Visualizations", shiny::fluidRow(
      
      # Run visualization sidebar
      shiny::column(width = 3,
        shiny::tags$h4("Visualize Results"),
        shiny::hr(),
        shiny::tags$p(paste0("Tokenization statistics can be visualized after ",
                             "sequences are tokenized and summarized.")),
        shiny::br(),
        shiny::numericInput(inputId  = "top_n",
                            label    = "Enter the Top Number of Tokens",
                            value    = 10,
                            min      = 4),
        shiny::textInput(inputId     = "output_dir",
                         label       = "Enter an output directory to save plots",
                         placeholder = "Leave blank if not saving."),
        shiny::br(),
        shiny::actionButton(inputId  = "run_visual",
                            label    = "Generate Visualizations")),
      
      shiny::column(width = 1),
      
      # Visualization UI
      shiny::column(width = 7,
        shiny::tags$h3("Tokenization Result Visualization"),
        shiny::hr(),
        shiny::uiOutput("token_visual_ui"))
    ))
))


# =====| Server |===============================================================

# Define the server logic
server <- function(input, output) {

  # Initialize reactive storage for intermediates
  sequences <- shiny::reactiveVal(NULL)
  processed <- shiny::reactiveVal(NULL)
  tokens    <- shiny::reactiveVal(NULL)
  analyzed  <- shiny::reactiveVal(NULL)
  
  # -----| Download Sample Data |-----------------------------------------------
  output$download_dna <- shiny::downloadHandler(
    filename = ""
  )
  
  # -----| Upload, Preprocess, and Annotate |-----------------------------------
  shiny::observeEvent(input$run_preprocess, {
    req(input$fasta_file)
    req(input$sequence_type)

    # Load the FASTA into its corresponding Biostrings::XStringSet object
    tryCatch({
      seqs <- switch(input$sequence_type,
         "DNA" = Biostrings::readDNAStringSet(input$fasta_file$datapath),
         "RNA" = Biostrings::readRNAStringSet(input$fasta_file$datapath),
         "AA"  = Biostrings::readAAStringSet(input$fasta_file$datapath)
      )

      # Place the loaded sequences into reactive storage
      sequences(seqs)

    }, error = function(e) {
      shiny::showNotification(paste0("Failed to read FASTA. Ensure it is a ",
                                     "valid and matches the selected type."),
                              type = "error")
    })

    # Preprocess and annotate the loaded data and place into reactive storage
    preproc <- BioTokenizeR::preprocess_sequences(seqs = sequences())
    annot <- BioTokenizeR::annotate_sequences(bioBPE_seqs = preproc)
    processed(annot)
  })

  
  output$upload_message <- renderUI({

    # Show placeholder if sequences have not been uploaded and processed yet
    if (is.null(processed())) {
      shiny::tagList(
        shiny::tags$h4("Sequences have not been processed yet."),
        shiny::tags$p(paste0("Upload a FASTA file and click 'Preprocess and ",
                             "Annotate Data'."))
      )

    # Display success message once uploaded, preprocessed, and annotated
    } else {
      shiny::tagList(
        shiny::tags$h4("Sequences have been successfully processed and annotated."),
        shiny::tags$p(paste0("Continue to the 'Token Summary' tab to tokenize ",
                             "the sequences."))
      )
    }
  })
  
  
  # -----| Tokenize and Analyze Sequences |-------------------------------------
  output$token_summary_ui <- shiny::renderUI(
    
    # Show placeholder if tokenization is not run yet
    if (is.null(tokens())) {
      shiny::tagList(
        shiny::br(),
        shiny::hr(),
        shiny::tags$h4("There are no tokens to summarize yet."),
        shiny::tags$p(paste0("Tokenization has not yet been run. Please ",
                             "upload and preprocess sequences using the ",
                             "'Upload Sequences' tab and click 'Run ",
                             "Tokenization' to view results.")),
        shiny::hr())
      
    # Display tokenization summary statistics once pipeline is run
    } else {
      
    }
  )
  
  # -----| Visualize Tokens |---------------------------------------------------
  output$token_visual_ui <- shiny::renderUI(
    
    # Show placeholder if tokenization is not run yet
    if (is.null(tokens())) {
      shiny::tagList(
        shiny::br(),
        shiny::hr(),
        shiny::tags$h4("There are no token statistics to visualize yet."),
        shiny::tags$p(paste0("Token summarization has not yet been run.", 
                             "Please tokenize sequences using the 'Token ",
                             "Summary' tab and click 'Generate Visualizations'",
                             "to view results.")),
        shiny::hr())
      
      # Display tokenization summary statistics once pipeline is run
    } else {
      
  })
}
  
  
  







# ==============================================================================

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
  


shiny::shinyApp(ui, server)
# [END]
