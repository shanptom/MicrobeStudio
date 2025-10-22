# Regression Module Server
# Handles taxon-environment regression analysis

regression_server <- function(input, output, session, final_physeq, analysis_ready) {

  # Build a single microeco dataset from final_physeq for this module
  meco_dataset <- reactive({
    req(analysis_ready(), final_physeq())
    ds <- phyloseq2meco(final_physeq())
    ds$tidy_dataset()
    ds$cal_abund()
    ds
  })

  # Render Environmental Variable selector via UI output (numeric-only)
  output$env_var_selector <- renderUI({
    req(meco_dataset())
    st <- meco_dataset()$sample_table
    numeric_cols <- names(st)[sapply(st, is.numeric)]
    if (length(numeric_cols) == 0) {
      return(div(class = "ui warning message",
                 tags$i(class = "exclamation triangle icon"),
                 "No numeric metadata columns found. Please provide numeric environmental variables."))
    }
    div(class = "field",
      tags$label("Environmental Variable"),
      selectInput("env_var", NULL, choices = numeric_cols,
                  selected = numeric_cols[1])
    )
  })

  # Update environment variable selector when data is ready and tab is active (numeric-only)
  observe({
    req(analysis_ready(), final_physeq())
    if (!identical(input$current_tab, "regression")) return()
    # Use numeric columns from microeco dataset's sample_table
    st <- meco_dataset()$sample_table
    numeric_cols <- names(st)[sapply(st, is.numeric)]
    updateSelectInput(session, "env_var",
                      choices = numeric_cols,
                      selected = if (length(numeric_cols) > 0) numeric_cols[1] else NULL)
  })

  # Taxonomic rank selector for regression
  output$tax_rank_selector_regression <- renderUI({
    req(final_physeq())
    ranks <- colnames(as.data.frame(tax_table(final_physeq())))
    selectInput("tax_rank_regression", "Select Taxonomic Rank:",
                choices = ranks, selected = tail(ranks, 1))
  })

  # Taxa selector updates based on selected rank
  output$taxa_selector_regression <- renderUI({
    req(meco_dataset(), input$tax_rank_regression)

    rank_table <- meco_dataset()$taxa_abund[[input$tax_rank_regression]]
    validate(need(!is.null(rank_table), "Abundance table for selected rank not found."))

    taxa_names <- rownames(rank_table)
    selectInput("selected_taxon", "Select Taxon (Lineage):",
                choices = taxa_names, selected = taxa_names[1])
  })

  # Regression group selector
  output$regression_group_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("group", "Group by:", choices = c("None", categorical_cols), selected = "None")
  })

  # Regression plot
  output$regression_plot <- renderPlot({
    req(input$run_scatter)
    if (!analysis_ready() || is.null(final_physeq())) return(NULL)
    req(input$tax_rank_regression, input$selected_taxon, input$env_var, input$point_size, input$text_size)

    # Prepare dataset and env object
    dataset <- meco_dataset()

    # Create trans_env using the selected environmental variable (by name)
    env_obj <- trans_env$new(dataset = dataset, env_cols = input$env_var)

    # Safety: ensure selected env var is numeric
    validate(need(is.numeric(dataset$sample_table[[input$env_var]]),
                  "Selected environmental variable must be numeric."))

    # Get abundance vector for selected taxon
    rank_table <- dataset$taxa_abund[[input$tax_rank_regression]]
    validate(need(input$selected_taxon %in% rownames(rank_table), "Selected taxon not found."))
    lineage_vector <- as.numeric(rank_table[input$selected_taxon, ])

    group_val <- if (input$group == "None") NULL else input$group

    # Generate plot using plot_scatterfit()
    # Extract the last taxon name from input$selected_taxon (format: K_Taxa1|p_Taxa2|...)
    taxon_parts <- unlist(strsplit(input$selected_taxon, "|", fixed = TRUE))
    last_taxon <- tail(taxon_parts, 1)
    # Remove prefix like x__ from the last taxon (e.g., c__ from c__Bacilli)
    last_taxon <- sub("^[a-zA-Z]__", "", last_taxon)

    p <- env_obj$plot_scatterfit(
      x = lineage_vector,
      y = input$env_var,
      group = group_val,
      type = "lm",
      point_size = input$point_size,
      point_alpha = 1,
      line_color = "#2A0E3C",
      line_se_color = "#A87CA0",
      label.x.npc = "left", label.y.npc = "top",
      x_axis_title = last_taxon,
      y_axis_title = input$env_var
    ) + theme_classic() +
    theme(
      axis.text = element_text(size = input$text_size),
      axis.title = element_text(size = input$text_size),
      legend.text = element_text(size = input$text_size),
      legend.title = element_text(size = input$text_size),
      text = element_text(size = input$text_size)  # This should cover annotations like equations
    )

    print(p)
  })

  # Ensure UI elements render even when tab is hidden
  outputOptions(output, "tax_rank_selector_regression", suspendWhenHidden = FALSE)
  outputOptions(output, "taxa_selector_regression", suspendWhenHidden = FALSE)
  outputOptions(output, "regression_group_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "env_var_selector", suspendWhenHidden = FALSE)

  # Suspend plot rendering when hidden (only render when tab is active)
  outputOptions(output, "regression_plot", suspendWhenHidden = TRUE)
}
