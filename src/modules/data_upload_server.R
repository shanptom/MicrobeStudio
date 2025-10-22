# Data Upload Module Server
# Handles file uploads, demo data loading, and phyloseq object creation

data_upload_server <- function(input, output, session, raw_physeq, ordering_rules, analysis_tabs, analysis_ready) {
  # Ensure Upload button starts disabled; enable once files are ready
  shinyjs::disable("process_files")

  processing <- reactiveVal(FALSE)

  # Enable/disable Upload button based on file readiness and processing state
  observe({
    ready <- (!is.null(input$phylo) && !is.null(input$phylo$datapath) && nzchar(input$phylo$datapath)) ||
      (!is.null(input$asv) && !is.null(input$tax) && !is.null(input$meta) &&
       !is.null(input$asv$datapath) && !is.null(input$tax$datapath) && !is.null(input$meta$datapath) &&
       nzchar(input$asv$datapath) && nzchar(input$tax$datapath) && nzchar(input$meta$datapath))

    if (isTRUE(processing())) {
      shinyjs::disable("process_files")
    } else if (ready) {
      shinyjs::enable("process_files")
    } else {
      shinyjs::disable("process_files")
    }
  })

  # Observer for demo button on upload tab
  observeEvent(input$load_demo, {
    req(input$demo_file)
    load_demo_data(input$demo_file)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Observer for demo button on home page
  observeEvent(input$load_demo_from_home, {
    # Home page button defaults to RDS
    load_demo_data("rds")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Observer for processing uploaded files
  observeEvent(input$process_files, {
    processing(TRUE)
    shinyjs::disable("process_files")
    shinyjs::show("upload_spinner")
    on.exit({
      processing(FALSE)
      shinyjs::hide("upload_spinner")
    }, add = TRUE)
    process_uploaded_files()
  })

  # Function to load demo data
  load_demo_data <- function(type) {
    demo_path <- "data/"
    phy_obj <- NULL
    
    tryCatch({
      if (type == "rds") {
        phy_obj <- readRDS(file.path(demo_path, "demo_ps.rds"))
        showNotification("Demo Phyloseq RDS loaded!", type = "message", duration = 5)
      } else { # csv
        otu <- read.csv(file.path(demo_path, "demo_asv.csv"), row.names = 1)
        tax <- as.matrix(read.csv(file.path(demo_path, "demo_tax.csv"), row.names = 1))
        meta <- read.csv(file.path(demo_path, "demo_meta.csv"), row.names = 1)
        phy_obj <- phyloseq(
          otu_table(as.matrix(otu), taxa_are_rows = TRUE),
          tax_table(tax),
          sample_data(meta)
        )
        showNotification("Demo CSV files loaded!", type = "message", duration = 5)
      }
      
      # Common post-load logic
      post_data_load(phy_obj)
      
    }, error = function(e) {
      showNotification(paste("Error loading demo data:", e$message), type = "error", duration = 10)
    })
  }

  # Function to process user-uploaded files
  process_uploaded_files <- function() {
    phy_obj <- NULL
    
    # Helper: wait for files to exist (handles async upload race)
    wait_for_files <- function(paths, timeout_ms = 5000L) {
      paths <- paths[!is.null(paths) & !is.na(paths) & nzchar(paths)]
      if (length(paths) == 0) return(FALSE)
      start <- Sys.time()
      while (any(!file.exists(paths))) {
        if (as.numeric(difftime(Sys.time(), start, units = "secs")) * 1000 > timeout_ms) {
          return(FALSE)
        }
        Sys.sleep(0.05)
      }
      TRUE
    }
    
    tryCatch({
      # Prefer RDS if provided
      if (!is.null(input$phylo) && !is.null(input$phylo$datapath)) {
        if (!wait_for_files(input$phylo$datapath)) {
          showNotification("File is still uploading. Please wait a moment and try again.", type = "warning", duration = 6)
          return()
        }
        phy_obj <- readRDS(input$phylo$datapath)
      } else if (!is.null(input$asv) && !is.null(input$tax) && !is.null(input$meta) &&
                 !is.null(input$asv$datapath) && !is.null(input$tax$datapath) && !is.null(input$meta$datapath)) {
        paths <- c(input$asv$datapath, input$tax$datapath, input$meta$datapath)
        if (!wait_for_files(paths)) {
          showNotification("Files are still uploading. Please wait a moment and try again.", type = "warning", duration = 6)
          return()
        }
        otu <- read.csv(input$asv$datapath, row.names = 1, check.names = FALSE)
        tax <- as.matrix(read.csv(input$tax$datapath, row.names = 1, check.names = FALSE))
        meta <- read.csv(input$meta$datapath, row.names = 1, check.names = FALSE)
        phy_obj <- phyloseq(
          otu_table(as.matrix(otu), taxa_are_rows = TRUE),
          tax_table(tax),
          sample_data(meta)
        )
      } else {
        showNotification("Please upload a complete dataset (either a .rds file or all three .csv files).", type = "warning", duration = 8)
        return()
      }
      
      showNotification("Files processed successfully!", type = "message", duration = 5)
      post_data_load(phy_obj)
      
    }, error = function(e) {
      showNotification(paste("Error processing uploaded files:", e$message), type = "error", duration = 10)
    })
  }

  # Common logic after any data is loaded
  post_data_load <- function(phy_obj) {
    req(phy_obj)

    # Store raw phyloseq data (not final yet)
    raw_physeq(phy_obj)
    df <- as.data.frame(sample_data(phy_obj))

    # Reset ordering rules
    for (var in colnames(df)) {
      if (is.character(df[[var]]) || is.factor(df[[var]])) {
        ordering_rules[[var]] <- unique(df[[var]])
      }
    }
    ordering_rules$Sample <- sample_names(phy_obj)

    # Enable filter tab only
    # final_physeq will be created when user applies filters or clicks "Go to Analysis"
    session$sendCustomMessage('enableTab', 'filter')
    session$sendCustomMessage('showTab', 'filter')
  }

  # UI to show the status of the data upload
  output$upload_status_ui <- renderUI({
    if (analysis_ready()) {
      div(class = "ui positive message",
          tags$i(class = "check circle icon"),
          div(class = "content",
              div(class = "header", "Data Ready for Analysis"),
              p("Your data has been loaded and is ready. Proceed to the 'Filter' tab to continue.")
          )
      )
    } else {
      div(class = "ui info message",
          tags$i(class = "info circle icon"),
          div(class = "content",
              div(class = "header", "Waiting for Data"),
              p("Select your files and wait for the 'Upload Files' button to enable, or load a demo dataset.")
          )
      )
    }
  })
}
