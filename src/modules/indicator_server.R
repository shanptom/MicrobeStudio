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

    ps <- final_physeq()

    # Run the SHAP analysis function from the sourced script
    results <- run_shap_analysis(
      phyloseq_obj = ps,
      variable = input$indicator_var,
      group1 = input$indicator_group1,
      top_n = input$top_n_taxa,
      font_size = input$indicator_font_size
    )

    return(results)
  })

  # Indicator plot
  output$indicator_plot <- renderPlot({
    if (!analysis_ready() || is.null(final_physeq())) return(NULL)
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
  outputOptions(output, "indicator_table", suspendWhenHidden = TRUE)
}
