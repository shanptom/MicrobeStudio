# Rarefaction Module Server
# Handles rarefaction curve plots

rarefaction_server <- function(input, output, session, final_physeq, analysis_ready) {

  # Color selector
  output$rarefaction_color_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("rare_color", "Color by:", choices = categorical_cols, selected = categorical_cols[1] %||% "None")
  })

  # Facet selector
  output$rarefaction_facet_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("rare_facet", "Facet by:", choices = c("None", categorical_cols), selected = "None")
  })

  # Facet order selector
  output$rarefaction_facet_order_selector <- renderUI({
    req(final_physeq(), input$rare_facet)
    if (!is.null(input$rare_facet) && input$rare_facet != "None") {
      df <- as.data.frame(sample_data(final_physeq()))
      choices <- unique(df[[input$rare_facet]])
      selectizeInput("rarefaction_facet_order", "Custom Facet Order (drag to reorder):",
                     choices = choices, selected = choices, multiple = TRUE,
                     options = list(plugins = list('drag_drop')))
    } else {
      return(NULL)
    }
  })

  # Base rarefaction plot - only recalculates when data changes
  base_rarefaction_plot <- reactive({
    req(final_physeq())
    ps <- final_physeq()
    otu_table(ps) <- otu_table(round(otu_table(ps)), taxa_are_rows = TRUE)

    if (any(sample_sums(ps) == 0)) {
      showNotification("Some samples have 0 counts. Rarefaction plot may not work.", type = "error")
      return(NULL)
    }

    # Generate base rarefaction curve (expensive operation)
    p <- ggrare(ps, step = 100, label = "Sample", se = FALSE) + theme_minimal()
    return(p)
  })

  # Rarefaction plot with aesthetic modifications
  output$rarefactionPlot <- renderPlot({
    req(base_rarefaction_plot(), input$rare_color)

    # Get the cached base plot
    p <- base_rarefaction_plot()

    # Apply color aesthetic
    p <- p + aes(color = !!sym(input$rare_color))

    # Apply faceting if selected
    if (!is.null(input$rare_facet) && input$rare_facet != "None") {
      if (!is.null(input$rarefaction_facet_order) && length(input$rarefaction_facet_order) > 0) {
        # Apply custom facet order using factor with specified levels
        levels_str <- paste0("c(\"", paste(input$rarefaction_facet_order, collapse = "\", \""), "\")")
        p <- p + facet_wrap(as.formula(paste0("~factor(", input$rare_facet, ", levels = ", levels_str, ")")), scales = "free")
      } else {
        # Default facet without custom order
        p <- p + facet_wrap(as.formula(paste("~", input$rare_facet)), scales = "free")
      }
    }

    # Handle sample labels
    if (!input$show_rarefaction_labels) {
      # Remove geom_text layer if labels are not to be shown
      p$layers <- lapply(p$layers, function(layer) {
        if (inherits(layer$geom, "GeomText")) {
          return(NULL)
        }
        return(layer)
      })
      p$layers <- Filter(Negate(is.null), p$layers)
    } else {
      # Override the default label size for sample labels
      p$layers <- lapply(p$layers, function(layer) {
        if (inherits(layer$geom, "GeomText")) {
          layer$aes_params$size <- input$rarefaction_label_size
        }
        return(layer)
      })
    }

    # Apply theme with user-specified text sizes
    p +
      theme(
        axis.text = element_text(size = input$rarefaction_beta_label_size),
        axis.title = element_text(size = input$rarefaction_beta_label_size),
        strip.text = element_text(size = input$rarefaction_beta_label_size),
        legend.position = "none"
      )
  })

  # Ensure UI elements render even when tab is hidden
  outputOptions(output, "rarefaction_color_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "rarefaction_facet_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "rarefaction_facet_order_selector", suspendWhenHidden = FALSE)

  # Suspend plot rendering when hidden (only render when tab is active)
  outputOptions(output, "rarefactionPlot", suspendWhenHidden = TRUE)
}
