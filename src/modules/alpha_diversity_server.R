alpha_diversity_server <- function(input, output, session, final_physeq, analysis_ready) {

  # Group selector
  output$alpha_group_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("alpha_group", "Group by:",
                choices = c("None", categorical_cols),
                selected = "None")
  })

  # Order selector
  output$alpha_order_selector <- renderUI({
    req(final_physeq(), input$alpha_group)
    if (!is.null(input$alpha_group) && input$alpha_group != "None") {
      df <- as.data.frame(sample_data(final_physeq()))
      choices <- unique(df[[input$alpha_group]])
      selectizeInput("alpha_order", "Custom Order (drag to reorder):",
                     choices = choices, selected = choices, multiple = TRUE,
                     options = list(plugins = list('drag_drop')))
    } else {
      choices <- sample_names(final_physeq())
      selectizeInput("alpha_order", "Custom Sample Order (drag to reorder):",
                     choices = choices, selected = choices, multiple = TRUE,
                     options = list(plugins = list('drag_drop')))
    }
  })

  # Color selector
  output$alpha_colour_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("alpha_colour", "Colour", choices = c("None", categorical_cols),
                selected = "None")
  })

  # Alpha diversity plot output wrapper
  output$alpha_plot_output <- renderUI({
    plotOutput("alphaPlot", width = "1200px", height = "755px")
  })

  # Alpha diversity plot
  output$alphaPlot <- renderPlot(width = 1200, height = 755, {
    if (!analysis_ready() || is.null(final_physeq())) {
      plot.new()
      text(0.5, 0.5, "Waiting for data...", cex = 2)
      return()
    }
    # Do not require flip checkbox to be TRUE; only ensure inputs exist
    req(input$alpha_index, input$alpha_beta_label_size)

    validate(need(length(input$alpha_index) > 0, "Please select at least one diversity index."))

    tryCatch({
    scale_type <- if (isTRUE(input$flip_alpha)) "free_x" else "free"

      # Base plot
      if (!is.null(input$alpha_group) && input$alpha_group != "None") {
        p <- plot_richness(
          final_physeq(),
          x = input$alpha_group,
          measures = input$alpha_index,
          scales = scale_type
        ) + geom_point(size = 5, alpha = 0.7) + theme_minimal() + theme(plot.margin = margin(0,0,0,0))
      } else {
        p <- plot_richness(
          final_physeq(),
          measures = input$alpha_index,
          scales = scale_type
        ) + geom_point(size = 5, alpha = 0.7) + theme_minimal() + theme(plot.margin = margin(0,0,0,0))
      }

      # Apply color only if valid
      if (!is.null(input$alpha_colour) && input$alpha_colour != "None") {
        p <- p + aes_string(color = input$alpha_colour)
      }

      # Reorder if custom order given
      if (!is.null(input$alpha_order) && length(input$alpha_order) > 0) {
        if (!is.null(input$alpha_group) && input$alpha_group != "None" && input$alpha_group %in% colnames(p$data)) {
          p$data[[input$alpha_group]] <- factor(p$data[[input$alpha_group]], levels = input$alpha_order)
        } else if ("samples" %in% colnames(p$data)) {
          p$data$samples <- factor(p$data$samples, levels = input$alpha_order)
        }
      }

      if (isTRUE(input$flip_alpha)) {
        p <- p + coord_flip()
      }

      p +
        theme(
          axis.text = element_text(size = input$alpha_beta_label_size),
          axis.title = element_text(size = input$alpha_beta_label_size),
          legend.text = element_text(size = input$alpha_beta_label_size),
          legend.title = element_text(size = input$alpha_beta_label_size),
          strip.text = element_text(size = input$alpha_beta_label_size)
        )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.5)
    })
  })

  # Ensure UI elements render even when tab is hidden
  outputOptions(output, "alpha_group_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "alpha_order_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "alpha_colour_selector", suspendWhenHidden = FALSE)

  # Allow plot rendering even when tab is hidden (to ensure it renders when tab becomes active)
  outputOptions(output, "alphaPlot", suspendWhenHidden = FALSE)
  outputOptions(output, "alpha_plot_output", suspendWhenHidden = FALSE)
}
