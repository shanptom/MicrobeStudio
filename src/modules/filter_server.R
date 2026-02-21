# Filter Module Server
# Handles taxonomic filtering, rarefaction, and TSS normalization

filter_server <- function(input, output, session, raw_physeq, final_physeq, ordering_rules, analysis_ready, telemetry = NULL) {
  # Taxa filter UI - based on raw uploaded data
  output$taxa_filters <- renderUI({
    req(raw_physeq())
    ps <- raw_physeq()

    if (is.null(ps)) {
      return(div(class = "ui warning message", p("Waiting for data to load...")))
    }

    ranks <- colnames(tax_table(ps))

    tagList(
      lapply(ranks, function(rank) {
        div(
          class = "field",
          tags$label(paste("Exclude", rank, "(comma-separated):")),
          textInput(paste0("filter_", rank), NULL, "", width = "100%")
        )
      })
    )
  })

  # Ensure the UI output is never suspended
  outputOptions(output, "taxa_filters", suspendWhenHidden = FALSE)

  # Apply filter button - filters raw data and updates raw_physeq
  observeEvent(input$apply_filter, {
    if (!is.null(telemetry)) {
      log_tool_usage(telemetry, session, "filter_apply")
    }
    req(raw_physeq())
    ps <- raw_physeq()
    taxdf <- as.data.frame(tax_table(ps))
    ranks <- colnames(taxdf)
    to_remove <- rep(FALSE, ntaxa(ps))

    for (rank in ranks) {
      vals <- input[[paste0("filter_", rank)]]
      # Check if vals exists and has content
      if (!is.null(vals) && length(vals) > 0 && nzchar(vals)) {
        exclude_list <- trimws(unlist(strsplit(vals, ",")))
        to_remove <- to_remove | taxdf[[rank]] %in% exclude_list
      }
    }

    # Apply filtering to raw data
    ps <- prune_taxa(!to_remove, ps)
    df <- as.data.frame(sample_data(ps))
    for (var in colnames(df)) {
      if (is.character(df[[var]]) || is.factor(df[[var]])) ordering_rules[[var]] <- unique(df[[var]])
    }
    ordering_rules$Sample <- sample_names(ps)

    # Update raw_physeq with filtered data (not final yet - still needs normalization)
    raw_physeq(ps)

    if (any(sample_sums(ps) == 0)) {
      showNotification("Warning: some samples have zero reads after filtering.", type = "warning")
    } else {
      showNotification("Filtering applied successfully.", type = "message")
    }
  })

  # Go to analysis button - creates final_physeq from raw_physeq
  observeEvent(input$go_analysis, {
    req(raw_physeq())

    # Start with raw (possibly filtered) data
    ps <- raw_physeq()

    # Apply normalization if selected
    if (input$doRarefy) {
      ps <- rarefy_even_depth(ps,
        sample.size = min(sample_sums(ps)),
        rngseed = 123, replace = TRUE,
        trimOTUs = TRUE, verbose = FALSE
      )
      showNotification("Rarefaction applied.", type = "message")
    } else if (input$doTSS) {
      ps <- transform_sample_counts(ps, function(x) x / sum(x))
      showNotification("TSS normalization applied.", type = "message")
    }

    # Create final_physeq (this triggers all analysis plots)
    final_physeq(ps)

    # Enable all analysis tabs now that data is finalized
    analysis_tabs <- c(
      "rarefaction", "abundance", "alpha", "dendrogram",
      "ordination", "metadata", "regression", "indicator"
    )
    for (tab in analysis_tabs) {
      session$sendCustomMessage("enableTab", tab)
    }

    # Set analysis ready flag and switch to rarefaction tab
    analysis_ready(TRUE)
    session$sendCustomMessage("showTab", "rarefaction")
  })

  # Prevent both normalization methods from being selected
  observeEvent(input$doRarefy, {
    if (input$doRarefy && input$doTSS) {
      updateCheckboxInput(session, "doTSS", value = FALSE)
    }
  })

  observeEvent(input$doTSS, {
    if (input$doTSS && input$doRarefy) {
      updateCheckboxInput(session, "doRarefy", value = FALSE)
    }
  })

  # Filter status display - rich data summary
  output$filter_status <- renderUI({
    req(raw_physeq())
    ps <- raw_physeq()
    df <- as.data.frame(sample_data(ps))
    lib_sizes <- sample_sums(ps)
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    num_cols <- names(df)[sapply(df, is.numeric)]
    tax_ranks <- colnames(tax_table(ps))

    div(
      class = "ui mini statistics",
      style = "flex-wrap: wrap;",
      div(
        class = "statistic",
        div(class = "value", nsamples(ps)),
        div(class = "label", "Samples")
      ),
      div(
        class = "statistic",
        div(class = "value", ntaxa(ps)),
        div(class = "label", "ASVs / OTUs")
      ),
      div(
        class = "statistic",
        div(class = "value", length(tax_ranks)),
        div(class = "label", paste("Ranks (", paste(tax_ranks[c(1, length(tax_ranks))], collapse = " \u2192 "), ")"))
      ),
      div(
        class = "statistic",
        div(class = "value", length(cat_cols)),
        div(class = "label", "Grouping Vars")
      ),
      div(
        class = "statistic",
        div(class = "value", length(num_cols)),
        div(class = "label", "Numeric Vars")
      ),
      div(
        class = "ui mini message", style = "width: 100%; margin-top: 0.5rem;",
        tags$strong("Library sizes: "),
        sprintf(
          "min = %s, median = %s, max = %s",
          format(min(lib_sizes), big.mark = ","),
          format(median(lib_sizes), big.mark = ","),
          format(max(lib_sizes), big.mark = ",")
        )
      )
    )
  })

  # Download filtered phyloseq as .rds
  output$download_phyloseq <- downloadHandler(
    filename = function() paste0("filtered_phyloseq_", Sys.Date(), ".rds"),
    content = function(file) {
      tryCatch(
        {
          req(raw_physeq())
          saveRDS(raw_physeq(), file)
        },
        error = function(e) {
          showNotification(paste("Download failed:", e$message), type = "error")
        }
      )
    }
  )
}
