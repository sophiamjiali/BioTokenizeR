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
      shiny::column(width = 4,
                    
        shiny::tags$h3("Upload Sequences"),
        shiny::tags$br(),
        
        
        # Upload external file
        shiny::tags$h4("External Data"),
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
                            label   = "Preprocess User Sequences",
                            style = "margin-bottom: 70px"),
        
        # Use sample data
        shiny::tags$h4("Sample Data"),
        shiny::hr(),
        shiny::tags$p("Sample data is included in the package and available ",
                      "for use."),
        shiny::selectInput(inputId = "sample_fasta_file",
                           label   = "Select Sample Data",
                           choices = c("Do not use sample data",
                                       "Sample DNA FASTA",
                                       "Sample RNA FASTA",
                                       "Sample AA FASTA"),
                           selected = "Do not use sample data"),
        shiny::actionButton(inputId = "run_preprocess_sample",
                            label = "Preprocess Sample Sequences")),

      
      shiny::column(width = 1),
      
      # Download sample data
      shiny::column(width = 7,
        shiny::tags$h3("Entering the BioTokenizeR Pipeline"),
        shiny::hr(),
        shiny::br(),
        shiny::uiOutput("upload_message"),
        shiny::br(),
        shiny::hr(),
        shiny::tags$h4("Download Sample Data"),
        shiny::tags$p(paste0("The sample data is additionally directly available ",
                             "for download.")),
        shiny::downloadButton(outputId = "download_dna", 
                              label    = "Download Sample DNA FASTA File",
                              style    = "margin-bottom: 10px;"),
        shiny::downloadButton(outputId = "download_rna", 
                              label    = "Download Sample RNA FASTA File",
                              style    = "margin-bottom: 10px;"),
        shiny::downloadButton(outputId = "download_aa", 
                              label    = "Download Sample AA FASTA File",
                              style    = "margin-bottom: 10px;"),
        shiny::hr(),
        shiny::tags$p(paste0("Raw sample data files can be accessed on GitHub at:",
                             " https://github/com/sophiamjiali/BioTokenizeR",
                             "/tree/main/inst/extdata")))
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
  sequences   <- shiny::reactiveVal(NULL)
  processed   <- shiny::reactiveVal(NULL)
  tokens      <- shiny::reactiveVal(NULL)
  statistics  <- shiny::reactiveVal(NULL)
  visuals     <- shiny::reactiveVal(NULL)
  
  # -----| Download Sample Data |-----------------------------------------------
  output$download_dna <- shiny::downloadHandler(
    filename = "sample_dna.fasta",
    content = function(file) {
      sample_path = system.file("extdata", "sample_dna.fasta", 
                                package = "BioTokenizeR")
      file.copy(sample_path, file, overwrite = TRUE)
    }
  )
  
  output$download_rna <- shiny::downloadHandler(
    filename = "sample_rna.fasta",
    content = function(file) {
      sample_path = system.file("extdata", "sample_rna.fasta", 
                                package = "BioTokenizeR")
      file.copy(sample_path, file, overwrite = TRUE)
    }
  )
  
  output$download_aa <- shiny::downloadHandler(
    filename = "sample_aa.fasta",
    content = function(file) {
      sample_path = system.file("extdata", "sample_aa.fasta", 
                                package = "BioTokenizeR")
      file.copy(sample_path, file, overwrite = TRUE)
    }
  )
  
  # -----| Upload External Data |-----------------------------------------------
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
  
  
  # -----| Upload Sample Data |-------------------------------------------------
  shiny::observeEvent(input$run_preprocess_sample, {
    req(input$sample_fasta_file != "Do not use sample data")
    
    # Load the sample data FASTA into its corresponding Biostrings::XStringSet
    if (input$sample_fasta_file == "Sample DNA FASTA") {
      data_path <- system.file("extdata", "sample_dna.fasta", 
                               package = "BioTokenizeR")
      seqs <- Biostrings::readDNAStringSet(data_path)
      
    } else if (input$sample_fasta_file == "Sample RNA FASTA") {
      data_path <- system.file("extdata", "sample_rna.fasta", 
                               package = "BioTokenizeR")
      seqs <- Biostrings::readRNAStringSet(data_path)
      
    } else {
      data_path <- system.file("extdata", "sample_aa.fasta", 
                               package = "BioTokenizeR")
      seqs <- Biostrings::readAAStringSet(data_path)
    }

    # Place the loaded sequences into reactive storage
    sequences(seqs)
    
    # Preprocess and annotate the loaded data and place into reactive storage
    preproc <- BioTokenizeR::preprocess_sequences(seqs = sequences())
    annot <- BioTokenizeR::annotate_sequences(bioBPE_seqs = preproc)
    processed(annot)
  })

  # -----| Successful Data Upload |---------------------------------------------
  output$upload_message <- renderUI({

    # Show placeholder if sequences have not been uploaded and processed yet
    if (is.null(processed())) {
      shiny::tagList(
        shiny::tags$h4("Sequences have not been processed yet"),
        shiny::tags$p(paste0("Upload a FASTA file and click 'Preprocess and ",
                             "Annotate Data'."))
      )

    # Display success message once uploaded, preprocessed, and annotated
    } else {
      shiny::tagList(
        shiny::tags$h4("Sequences have been successfully processed"),
        shiny::tags$p(paste0("Continue to the 'Token Summary' tab to tokenize ",
                             "the sequences."))
      )
    }
  })
  
  
  # -----| Tokenize and Analyze Sequences |-------------------------------------
  shiny::observeEvent(input$run_tokenize, {
    req(processed())
    req(input$vocab_size)
    
    # Tokenize the sequences, setting into reactive storage
    out <- BioTokenizeR::tokenize_sequences(bioBPE_seqs = processed(),
                                            vocab_size = input$vocab_size)
    tokens(out)
    
    # Analyze the tokens, setting into reactive storage
    out <- BioTokenizeR::summarize_tokens(tokens = tokens()$tokens)
    statistics(out)
  })
  
  
  output$token_summary_ui <- shiny::renderUI({
    
    # Show placeholder if tokenization is not run yet
    if (is.null(statistics())) {
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
      corpus <- statistics()$corpus

      # Display corpus statistics as cards
      
      shiny::tagList(
        shiny::tags$h4("Corpus Summary Statistics"),
        bslib::layout_columns(
          bslib::card(bslib::card_header("Total Number of Sequences"), 
                      bslib::card_body(corpus$num_sequences)),
          bslib::card(bslib::card_header("Total Number of Tokens"), 
                      bslib::card_body(corpus$total_tokens))),
        bslib::layout_columns(
          bslib::card(bslib::card_header("Average Sequence Length"), 
                      bslib::card_body(corpus$avg_seq_length)),
          bslib::card(bslib::card_header("Median Sequence Length"), 
                      bslib::card_body(corpus$median_seq_length)),
          bslib::card(bslib::card_header("Vocabulary Size"), 
                      bslib::card_body(corpus$vocab_size))
        ),
        
        # Display token statistics as a table
        shiny::tags$br(),
        shiny::tags$h4("Token Summary Statistics"),
        bslib::card(bslib::card_header("Token Summary Table"),
                    shiny::tableOutput("token_summary_table"))
      )
    }
  })
  
  output$token_summary_table <- shiny::renderTable({
    
    # Render a table for the UI showing token statistics
    statistics()$token_summary
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # -----| Visualize Tokens |---------------------------------------------------
  shiny::observeEvent(input$run_visual, {
    req(statistics())
    req(input$top_n)
    
    # Set the output directory to NULL if the field was not specified
    if (input$output_dir == "") {
      output_dir = NULL
    } else {
      output_dir = input$output_dir
    }
    
    # Visualize the tokens, setting into reactive storage
    plots <- BioTokenizeR::visualize_tokens(statistics = statistics(),
                                            top_n      = input$top_n,
                                            output_dir = output_dir)
    visuals(plots)
    
    output$plot_freq_dist <- shiny::renderPlot({plots$frequency_distribution})
    output$plot_top_tokens <- shiny::renderPlot({plots$top_tokens})
    output$plot_cum_cov    <- shiny::renderPlot({plots$cumulative_coverage})
  })
  
  
  output$token_visual_ui <- shiny::renderUI(
    
    # Show placeholder if tokenization is not run yet
    if (is.null(statistics())) {
      shiny::tagList(
        shiny::br(),
        shiny::hr(),
        shiny::tags$h4("There are no token statistics to visualize yet"),
        shiny::tags$p(paste0("Token summarization has not yet been run.", 
                             "Please tokenize sequences using the 'Token ",
                             "Summary' tab and click 'Generate Visualizations'",
                             "to view results.")),
        shiny::hr())
      
    # Display tokenization visualizations once pipeline is run
    } else if (is.null(visuals())) {
      shiny::tagList(
        shiny::br(),
        shiny::hr(),
        shiny::tags$h4("Token statistics have been generated"),
        shiny::tags$p(paste0("Please visualize tokenization statistics by ",
                             "clicking 'Generate Visualizations'."))
      )
    } else {
      shiny::tagList(
        bslib::card(bslib::card_header("Token Frequency Distribution"),
                    bslib::card_body(plotOutput("plot_freq_dist"))),
        bslib::card(bslib::card_header("Top Tokens"),
                    bslib::card_body(plotOutput("plot_top_tokens"))),
        bslib::card(bslib::card_header("Cumulative Coverage"),
                    bslib::card_body(plotOutput("plot_cum_cov")))
      )
    }
  )
}

shiny::shinyApp(ui, server)
# [END]
