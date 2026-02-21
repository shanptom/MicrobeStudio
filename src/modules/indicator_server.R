# Indicator Species Module Server
# Handles SHAP-based indicator species analysis

indicator_server <- function(input, output, session, final_physeq, analysis_ready) {
  # Source SHAP analysis functions
  source("src/shap.R", local = TRUE)

  # Indicator variable selector
  output$indicator_variable_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("indicator_var", "Select Metadata Variable:", choices = categorical_cols)
  })

  # Group 1 selector
  output$indicator_group1_selector <- renderUI({
    req(final_physeq(), input$indicator_var)
    choices <- unique(as.data.frame(sample_data(final_physeq()))[[input$indicator_var]])
    selectInput("indicator_group1", "Select Group 1 (will be coded as 1):", choices = choices, selected = choices[1])
  })

  # Group 2 selector
  output$indicator_group2_selector <- renderUI({
    req(final_physeq(), input$indicator_var)
    choices <- unique(as.data.frame(sample_data(final_physeq()))[[input$indicator_var]])
    selectInput("indicator_group2", "Select Group 2 (will be coded as 0):", choices = choices, selected = choices[2])
  })

  # Run indicator analysis
  indicator_results <- eventReactive(input$run_indicator_analysis, {
    req(final_physeq(), input$indicator_var, input$indicator_group1, input$indicator_group2)

    # Validate that groups are different
    validate(need(
      input$indicator_group1 != input$indicator_group2,
      "Group 1 and Group 2 must be different. Please select two distinct groups for comparison."
    ))

    ps <- final_physeq()

    # Validate minimum sample sizes
    meta <- as.data.frame(sample_data(ps))
    group_counts <- table(meta[[input$indicator_var]])
    n_g1 <- sum(meta[[input$indicator_var]] == input$indicator_group1)
    n_g2 <- sum(meta[[input$indicator_var]] == input$indicator_group2)
    validate(need(
      n_g1 >= 3 && n_g2 >= 3,
      paste0(
        "Each group needs at least 3 samples for SHAP analysis. ",
        "Group 1 ('", input$indicator_group1, "') has ", n_g1, " samples, ",
        "Group 2 ('", input$indicator_group2, "') has ", n_g2, " samples."
      )
    ))

    # Run the SHAP analysis with progress indicator
    withProgress(message = "Running SHAP analysis...", value = 0, {
      incProgress(0.1, detail = "Preparing data")

      results <- tryCatch(
        {
          incProgress(0.3, detail = "Training XGBoost model")
          run_shap_analysis(
            phyloseq_obj = ps,
            variable = input$indicator_var,
            group1 = input$indicator_group1,
            top_n = input$top_n_taxa,
            font_size = input$indicator_font_size
          )
        },
        error = function(e) {
          msg <- e$message
          if (grepl("binary", msg, ignore.case = TRUE)) {
            stop("SHAP analysis requires exactly two groups. Filter your data to include only the two groups of interest.")
          }
          stop(paste("SHAP analysis failed:", msg))
        }
      )

      incProgress(0.6, detail = "Complete")
      results
    })
  })

  # Indicator plot
  output$indicator_plot <- renderPlot({
    if (!analysis_ready() || is.null(final_physeq())) {
      return(NULL)
    }
    req(input$top_n_taxa, input$indicator_font_size)

    results <- indicator_results()
    req(results$plot)
    results$plot
  })

  # Indicator table
  output$indicator_table <- DT::renderDataTable({
    results <- indicator_results()
    req(results$table)
    results$table
  })

  # Ensure UI elements render even when tab is hidden
  outputOptions(output, "indicator_variable_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "indicator_group1_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "indicator_group2_selector", suspendWhenHidden = FALSE)

  # Suspend plot rendering when hidden (only render when tab is active)
  outputOptions(output, "indicator_plot", suspendWhenHidden = TRUE)

  # Download indicator species plot as PDF
  output$download_indicator_plot <- downloadHandler(
    filename = function() paste0("indicator_species_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch(
        {
          results <- indicator_results()
          req(results$plot)
          ggplot2::ggsave(file, plot = results$plot, width = 10, height = 8, device = "pdf")
        },
        error = function(e) {
          showNotification(paste("Download failed:", e$message), type = "error")
        }
      )
    }
  )

  # Download indicator species table as CSV
  output$download_indicator_table <- downloadHandler(
    filename = function() paste0("indicator_species_table_", Sys.Date(), ".csv"),
    content = function(file) {
      tryCatch(
        {
          results <- indicator_results()
          req(results$table)
          utils::write.csv(results$table, file, row.names = FALSE)
        },
        error = function(e) {
          showNotification(paste("Download failed:", e$message), type = "error")
        }
      )
    }
  )
  outputOptions(output, "indicator_table", suspendWhenHidden = TRUE)
}
