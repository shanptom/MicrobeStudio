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

server <- function(input, output, session) {
  # Initialize reactive values and variables
  raw_physeq <- reactiveVal()        # Raw uploaded data (not processed)
  final_physeq <- reactiveVal()      # Filtered/normalized data (ready for analysis)
  ordering_rules <- reactiveValues()
  reactiveValues_envfit <- reactiveValues(transenv = NULL)
  selected_analysis <- reactiveVal(NULL)
  show_tsne <- reactiveVal(FALSE)
  analysis_ready <- reactiveVal(FALSE)

  # Define analysis tabs
  analysis_tabs <- c("filter", "rarefaction", "abundance", "alpha",
                     "dendrogram", "ordination", "permanova", "metadata", "regression", "indicator")

  # Initially disable all analysis tabs
  observe({
    for(tab in analysis_tabs) {
      session$sendCustomMessage('disableTab', tab)
    }
  })

  # Enable analysis tabs when ready
  observe({
    if (analysis_ready()) {
      for(tab in analysis_tabs) {
        session$sendCustomMessage('enableTab', tab)
      }
    }
  })

  # Call module servers
  data_upload_server(input, output, session, raw_physeq, ordering_rules, analysis_tabs, analysis_ready)
  filter_server(input, output, session, raw_physeq, final_physeq, ordering_rules, analysis_ready)
  rarefaction_server(input, output, session, final_physeq, analysis_ready)
  abundance_server(input, output, session, final_physeq, ordering_rules, analysis_ready)
  alpha_diversity_server(input, output, session, final_physeq, analysis_ready)
  beta_diversity_server(input, output, session, final_physeq, show_tsne, analysis_ready)
  dendrogram_server(input, output, session, final_physeq, analysis_ready)
  metadata_server(input, output, session, final_physeq, reactiveValues_envfit, selected_analysis, analysis_ready)
  regression_server(input, output, session, final_physeq, analysis_ready)
  indicator_server(input, output, session, final_physeq, analysis_ready)
}
