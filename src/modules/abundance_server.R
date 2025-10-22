# Abundance Module Server
# Handles abundance plots (bar, line, heatmap)

abundance_server <- function(input, output, session, final_physeq, ordering_rules, analysis_ready) {

  # Taxonomic rank selector
  output$tax_rank_selector <- renderUI({
    req(final_physeq())

    tax_ranks <- colnames(as.data.frame(tax_table(final_physeq())))

    # Default to 2nd rank (typically Phylum) for faster loading
    default_rank <- if(length(tax_ranks) >= 2) tax_ranks[2] else tax_ranks[1]

    div(class = "field",
      label("Select Taxonomic Rank"),
      selectInput("tax_rank", NULL, choices = tax_ranks, selected = default_rank)
    )
  })

  # Facet selector
  output$abundance_facet_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]

    div(class = "field",
      label("Facet by"),
      selectInput("abund_facet", NULL, choices = c("None", categorical_cols), selected = "None")
    )
  })

  output$abundance_order_selector <- renderUI({
    req(final_physeq(), input$abund_facet)
    if (!is.null(input$abund_facet) && input$abund_facet != "None") {
      df <- as.data.frame(sample_data(final_physeq()))
      choices <- unique(df[[input$abund_facet]])
      selectizeInput("abund_order", "Custom Order (drag to reorder):",
                     choices = choices, selected = choices, multiple = TRUE,
                     options = list(plugins = list('drag_drop')))
    } else {
      choices <- sample_names(final_physeq())
      selectizeInput("abund_order", "Custom Sample Order (drag to reorder):",
                     choices = choices, selected = choices, multiple = TRUE,
                     options = list(plugins = list('drag_drop')))
    }
  })

  output$abundance_plot_output <- renderUI({
    if (input$abund_plot_type == "line" || input$abund_plot_type == "heat") {
      plotOutput("abundancePlotStatic", height = "755px", width = "100%")
    } else {
      plotOutput("abundancePlot", height = "755px", width = "100%")
    }
  })

  abundance_plot_obj <- reactive({
    req(final_physeq(), input$tax_rank, input$ntaxa)

    dataset <- phyloseq2meco(final_physeq())
    dataset$tidy_dataset()
    dataset$cal_abund()
    t1 <- trans_abund$new(dataset = dataset, taxrank = input$tax_rank, ntaxa = input$ntaxa)
    scale_type <- if (input$flip_abundance) "free_y" else "free_x"

    if (input$abund_plot_type == "line") {
    # Create color palette that scales with ntaxa
    base_colors <- RColorBrewer::brewer.pal(min(input$ntaxa, 8), "Set2")
    if (input$ntaxa > 8) {
      color_palette <- colorRampPalette(base_colors)(input$ntaxa)
    } else {
      color_palette <- base_colors
    }
    p4 <- t1$plot_bar(
      bar_type = "notfull",
      use_alluvium = TRUE,
      clustering = TRUE,
      xtext_angle = 90,
      xtext_size = input$abundance_beta_label_size,
      color_values = color_palette
    )
  } else if (input$abund_plot_type == "bar") {
      p4 <- t1$plot_bar(
        others_color = "grey70",
        xtext_angle = 90,
        xtext_size = input$abundance_beta_label_size,
        legend_text_italic = FALSE
      )
    } else if (input$abund_plot_type == "heat") {
      p4 <- t1$plot_heatmap(
        xtext_keep = TRUE,
        xtext_angle = 90,
        xtext_size = input$abundance_beta_label_size,
        ytext_size = input$abundance_beta_label_size,
        withmargin = FALSE,
        plot_breaks = c(0.01, 0.1, 1, 10)
      )
    }

    for (var in names(ordering_rules)) {
    if (var %in% colnames(p4$data)) {
      p4$data[[var]] <- factor(p4$data[[var]], levels = ordering_rules[[var]])
    }
  }

  if (!is.null(input$abund_facet) && input$abund_facet != "None") {
    facet_formula <- as.formula(paste("~", input$abund_facet))
    if (input$abund_plot_type == "heat") {
      if (input$flip_abundance) {
        p4 <- p4 + facet_wrap(facet_formula, scales = "free_y")
      } else {
        p4 <- p4 + facet_wrap(facet_formula, scales = "free_x")
      }
    } else {
      p4 <- p4 + facet_wrap(facet_formula, scales = scale_type)
    }
  }

  if (!is.null(input$abund_order) && length(input$abund_order) > 0) {
    if (input$abund_facet != "None" && input$abund_facet %in% colnames(p4$data)) {
      p4$data[[input$abund_facet]] <- factor(p4$data[[input$abund_facet]], levels = input$abund_order)
    } else if ("Sample" %in% colnames(p4$data)) {
      p4$data$Sample <- factor(p4$data$Sample, levels = input$abund_order)
    }
  }

  if (input$flip_abundance) {
    p4 <- p4 + coord_flip()
  }

  if (input$abund_plot_type == "heat" && input$flip_abundance) {
  # Identify the variable used as x-axis (was y-axis before flipping)
  # In most microeco heatmaps, y = taxa, x = sample (or vice versa depending on transposition)
  axis_var <- names(p4$data)[1]  # or explicitly set e.g., "Sample" or "Taxon"

  # Drop unused levels in that axis_var
p4$data$Sample <- factor(p4$data$Sample)
p4$data$Sample <- droplevels(p4$data$Sample)
}

    final_plot <- p4 +
      theme(
        axis.text = element_text(size = input$abundance_beta_label_size),
        axis.title = element_text(size = input$abundance_beta_label_size),
        legend.text = element_text(size = input$abundance_beta_label_size),
        legend.title = element_text(size = input$abundance_beta_label_size),
        strip.text = element_text(size = input$abundance_beta_label_size),
        plot.margin = margin(0, 0, 0, 0)
      )

    return(final_plot)
  })

  output$abundancePlot <- renderPlot({
    if (!analysis_ready() || is.null(final_physeq())) {
      return(NULL)
    }

    plot_result <- abundance_plot_obj()
    return(plot_result)
  }, width = 1200, height = 755)

  output$abundancePlotStatic <- renderPlot({
    plot_result <- abundance_plot_obj()
    return(plot_result)
  }, width = 1200, height = 755)

  # Ensure UI elements render even when tab is hidden
  outputOptions(output, "tax_rank_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "abundance_facet_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "abundance_order_selector", suspendWhenHidden = FALSE)

  # Suspend plot rendering when tab is hidden (only render when tab is active)
  outputOptions(output, "abundancePlot", suspendWhenHidden = TRUE)
  outputOptions(output, "abundancePlotStatic", suspendWhenHidden = TRUE)
}
