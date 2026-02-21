library(phyloseq)
library(ggplot2)
library(vegan)
library(ranacapa)
library(phylosmith)
library(microeco)
library(file2meco)
library(GUniFrac)
library(RColorBrewer)
library(ggalluvial)
library(dplyr)
library(ggcor)
library(ggpubr)
library(DT)
library(ggrepel)
# library(plotly) # Commented out as we're using static ggplot2 plots

# Source telemetry configuration
source("src/telemetry_config.R", local = TRUE)

# Source all server modules
source("src/modules/data_upload_server.R", local = TRUE)
source("src/modules/filter_server.R", local = TRUE)
source("src/modules/rarefaction_server.R", local = TRUE)
source("src/modules/abundance_server.R", local = TRUE)
source("src/modules/alpha_diversity_server.R", local = TRUE)
source("src/modules/beta_diversity_server.R", local = TRUE)
source("src/modules/dendrogram_server.R", local = TRUE)
source("src/modules/metadata_server.R", local = TRUE)
source("src/modules/regression_server.R", local = TRUE)
source("src/modules/indicator_server.R", local = TRUE)

# Initialize telemetry with multi-user support
telemetry <- initialize_telemetry()

server <- function(input, output, session) {
  # Start privacy-compliant telemetry session with location tracking
  # Collects: anonymous session ID, country/region, browser/OS
  # Does NOT collect: PII, uploaded data, input values, full IP addresses
  session_uuid <- start_telemetry_session(telemetry, session)

  # End telemetry session on disconnect (calculate session duration)
  session$onSessionEnded(function() {
    end_telemetry_session(telemetry, session)
  })

  # Track tab navigation (usage patterns)
  observeEvent(input$current_tab, {
    req(input$current_tab)
    log_tab_view(telemetry, session, input$current_tab)
  })
  # ============================================================
  # DATA CONSENT MODAL
  # ============================================================
  data_consent_given <- reactiveVal(FALSE)

  # Show consent modal when user tries to access upload tab without consent
  observeEvent(input$request_consent, {
    if (!data_consent_given()) {
      session$sendCustomMessage("showConsentModal", list())
    }
  })

  # Handle consent response
  observeEvent(input$data_consent, {
    if (isTRUE(input$data_consent)) {
      data_consent_given(TRUE)
      # Now navigate to the upload tab
      session$sendCustomMessage("showTab", "upload")
    }
  })

  # Track uploaded file paths for cleanup
  uploaded_file_paths <- reactiveVal(character(0))

  # Initialize reactive values and variables
  raw_physeq <- reactiveVal() # Raw uploaded data (not processed)
  final_physeq <- reactiveVal() # Filtered/normalized data (ready for analysis)
  ordering_rules <- reactiveValues()
  reactiveValues_envfit <- reactiveValues(transenv = NULL)
  selected_analysis <- reactiveVal(NULL)
  show_tsne <- reactiveVal(FALSE)
  analysis_ready <- reactiveVal(FALSE)

  # Define analysis tabs
  analysis_tabs <- c(
    "filter", "rarefaction", "abundance", "alpha",
    "dendrogram", "ordination", "permanova", "metadata", "regression", "indicator"
  )

  # Initially disable all analysis tabs
  observe({
    for (tab in analysis_tabs) {
      session$sendCustomMessage("disableTab", tab)
    }
  })

  # Enable analysis tabs when ready
  observe({
    if (analysis_ready()) {
      for (tab in analysis_tabs) {
        session$sendCustomMessage("enableTab", tab)
      }
    }
  })

  # ============================================================
  # TRACK UPLOADED FILES FOR CLEANUP
  # ============================================================
  # Collect file paths whenever a file input changes
  observe({
    paths <- character(0)
    for (id in c("asv", "tax", "meta", "phylo", "biom")) {
      file_info <- input[[id]]
      if (!is.null(file_info) && !is.null(file_info$datapath)) {
        paths <- c(paths, file_info$datapath)
      }
    }
    if (length(paths) > 0) {
      uploaded_file_paths(unique(c(uploaded_file_paths(), paths)))
    }
  })

  # ============================================================
  # DELETE MY DATA
  # ============================================================
  observeEvent(input$delete_my_data, {
    # 1. Delete uploaded temp files from disk
    paths <- uploaded_file_paths()
    deleted_count <- 0L
    for (p in paths) {
      if (file.exists(p)) {
        unlink(p)
        deleted_count <- deleted_count + 1L
      }
      # Also remove the parent directory if it's now empty
      parent_dir <- dirname(p)
      if (dir.exists(parent_dir) && length(list.files(parent_dir)) == 0) {
        unlink(parent_dir, recursive = TRUE)
      }
    }
    uploaded_file_paths(character(0))

    # 2. Clear in-memory data
    raw_physeq(NULL)
    final_physeq(NULL)
    reactiveValues_envfit$transenv <- NULL
    selected_analysis(NULL)
    show_tsne(FALSE)
    analysis_ready(FALSE)

    # 3. Disable all analysis tabs
    for (tab in analysis_tabs) {
      session$sendCustomMessage("disableTab", tab)
    }

    # 4. Reset file input displays in the browser
    session$sendCustomMessage("resetFileInputs", list())

    # 5. Navigate to upload tab
    session$sendCustomMessage("showTab", "upload")

    # 6. Notify user
    showNotification(
      sprintf("All your data has been deleted from the server (%d file%s removed).",
              deleted_count, if (deleted_count == 1) "" else "s"),
      type = "message", duration = 6
    )
  })

  # ============================================================
  # AUTO-CLEANUP ON SESSION END
  # ============================================================
  session$onSessionEnded(function() {
    # Delete any remaining uploaded temp files
    paths <- isolate(uploaded_file_paths())
    for (p in paths) {
      if (file.exists(p)) {
        unlink(p)
      }
      parent_dir <- dirname(p)
      if (dir.exists(parent_dir) && length(list.files(parent_dir)) == 0) {
        unlink(parent_dir, recursive = TRUE)
      }
    }
  })

  # Call module servers (pass telemetry for consistent logging)
  data_upload_server(input, output, session, raw_physeq, ordering_rules, analysis_tabs, analysis_ready, telemetry)
  filter_server(input, output, session, raw_physeq, final_physeq, ordering_rules, analysis_ready, telemetry)
  rarefaction_server(input, output, session, final_physeq, analysis_ready)
  abundance_server(input, output, session, final_physeq, ordering_rules, analysis_ready)
  alpha_diversity_server(input, output, session, final_physeq, analysis_ready)
  beta_diversity_server(input, output, session, final_physeq, show_tsne, analysis_ready, telemetry)
  dendrogram_server(input, output, session, final_physeq, analysis_ready)
  metadata_server(input, output, session, final_physeq, reactiveValues_envfit, selected_analysis, analysis_ready, telemetry)
  regression_server(input, output, session, final_physeq, analysis_ready)
  indicator_server(input, output, session, final_physeq, analysis_ready)
}
