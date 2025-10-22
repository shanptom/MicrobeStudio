# Metadata Analysis Module Server
# Handles RDA, correlation analysis, and Mantel tests

metadata_server <- function(input, output, session, final_physeq, reactiveValues_envfit, selected_analysis, analysis_ready) {

  # Numeric column selector
  output$numeric_column_selector_ui <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("numeric_cols", "Select numeric metadata columns:",
                choices = numeric_cols, multiple = TRUE)
  })

  # Create trans_env object
  observeEvent(input$create_transenv, {
    req(final_physeq(), input$numeric_cols)

    dataset <- phyloseq2meco(final_physeq())
    dataset$tidy_dataset()
    dataset$cal_abund()
    dataset$cal_alphadiv()
    dataset$cal_betadiv()

    # Get the full metadata data frame
    meta_df <- as.data.frame(sample_data(final_physeq()))

    # Find the numeric indices of the selected column names
    selected_indices <- which(names(meta_df) %in% input$numeric_cols)

    # Create the trans_env object using the indices
    env_obj <- trans_env$new(dataset = dataset, env_cols = selected_indices)
    reactiveValues_envfit$transenv <- env_obj

    output$transenv_display <- renderPrint({
      env_obj
    })
  })

  # Visualization sidebar
  output$visualization_sidebar <- renderUI({
    req(reactiveValues_envfit$transenv)

    sample_meta <- as.data.frame(sample_data(final_physeq()))
    factor_vars <- names(sample_meta)[sapply(sample_meta, function(x) is.factor(x) || is.character(x))]

    tagList(
      h4("Select Analysis"),
      div(class = "ui buttons fluid",
        actionButton("run_rda", "RDA", class = "ui primary button"),
        actionButton("run_corr", "Correlation", class = "ui button"),
        actionButton("run_mantel", "Mantel", class = "ui orange button")
      ),

      # RDA controls
      conditionalPanel(
        condition = "output.analysis_mode == 'rda'",
        div(class = "ui segment",
          div(class = "ui form",
            div(class = "field",
              checkboxInput("adjust_arrow_length", "Adjust Arrow Length", TRUE)
            ),
            div(class = "field",
              sliderInput("max_perc_env", "Max Percentage of Explained Env Fit (arrows)", min = 0.05, max = 1, value = 0.3, step = 0.05)
            ),
            div(class = "field",
              selectInput("rda_color", "Color by:", choices = factor_vars, selected = factor_vars[1] %||% "None")
            ),
            div(class = "field",
              selectInput("rda_shape", "Point Shape", choices = factor_vars, selected = factor_vars[1] %||% "None")
            ),
            div(class = "field",
              selectInput("rda_label", "Sample Labels:", choices = c("None", names(sample_meta)), selected = "None")
            ),
            div(class = "field",
              sliderInput("rda_textsize", "Text Size", value = 6, min = 6, max = 15, step = 1)
            )
          )
        )
      ),

      # Correlation controls
      conditionalPanel(
        condition = "output.analysis_mode == 'corr'",
        div(class = "ui segment",
          div(class = "ui form",
            div(class = "field",
              selectInput("input_method", "Correlation Method", choices = c("pearson", "spearman", "kendall"), selected = "spearman")
            ),
            div(class = "field",
              numericInput("input_threshold", "Abundance Threshold", value = 0.001, min = 0.00001, max = 0.9, step = 0.001)
            ),
            div(class = "field",
              selectInput("p_type", "P-value Adjustment Type", choices = c("All", "Taxa", "Env"), selected = "All")
            ),
            div(class = "field",
              selectInput("group_by", "Group By", choices = c("None", names(sample_meta)), selected = "None")
            ),
            div(class = "field",
              sliderInput("corr_text_size", "Text Size", value = 10, min = 7, max = 20, step = 2)
            ),
            div(class = "field",
              sliderInput("xtextangle", "Text angle",  value = 0, min = 0, max = 90, step = 10)
            )
          )
        )
      ),

      # Mantel controls
      conditionalPanel(
        condition = "output.analysis_mode == 'mantel'",
        div(class = "ui segment",
          div(class = "ui form",
            div(class = "field",
              selectInput("mantel_group", "Group By:", choices = factor_vars, selected = factor_vars[1] %||% "None")
            ),
            actionButton("run_mantel_analysis", "Run Mantel Test", class = "ui primary button fluid")
          )
        )
      )
    )
  })

  # Analysis button observers
  observeEvent(input$run_rda, {
    selected_analysis("rda")
  })

  observeEvent(input$run_corr, {
    selected_analysis("corr")
  })

  observeEvent(input$run_mantel, {
    selected_analysis("mantel")
  })

  # Analysis mode output
  output$analysis_mode <- reactive({
    selected_analysis()
  })
  outputOptions(output, "analysis_mode", suspendWhenHidden = FALSE)

  # Ensure UI elements render even when tab is hidden
  outputOptions(output, "numeric_column_selector_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "visualization_sidebar", suspendWhenHidden = FALSE)



  # RDA plot
  output$rda_plot <- renderPlot({
    req(selected_analysis())
    req(reactiveValues_envfit$transenv, analysis_ready())

    e1 <- reactiveValues_envfit$transenv

    e1$cal_ordination(
      method = "RDA",
      feature_sel = FALSE
    )

    e1$trans_ordination(
      adjust_arrow_length = input$adjust_arrow_length,
      max_perc_env = input$max_perc_env
    )

    label_input <- if (input$rda_label == "None") NULL else input$rda_label
    shape_input <- if (input$rda_shape == "None") NULL else input$rda_shape

    e1$plot_ordination(
      plot_color = input$rda_color,
      plot_shape = shape_input,
      env_text_size = input$rda_textsize,
      taxa_text_size = input$rda_textsize,
      add_sample_label = label_input,
      point_size = input$rda_textsize
    )
  })

  # Correlation plot
  output$corr_plot <- renderPlot({
    req(reactiveValues_envfit$transenv, analysis_ready())

    e1 <- reactiveValues_envfit$transenv
    group_by <- if (input$group_by == "None") NULL else input$group_by

    e1$cal_cor(
      method = input$input_method,
      add_abund_table = NULL,
      filter_thres = input$input_threshold,
      p_adjust_method = "fdr",
      p_adjust_type = input$p_type,
      by_group = group_by,
      group_use = NULL
    )

    corr_plot <- e1$plot_cor(
      xtext_angle = input$xtextangle,
      xtext_size = input$corr_text_size,
      ytext_size = input$corr_text_size
    )
    print(corr_plot)
  })

  # Mantel test
  observeEvent(input$run_mantel_analysis, {
    req(final_physeq(), input$numeric_cols)

    dataset <- phyloseq2meco(final_physeq())
    dataset$tidy_dataset()

    # Get group variable
    group_col <- input$mantel_group
    group_vals <- unique(sample_data(final_physeq())[[group_col]])

    # Get the full metadata data frame
    meta_df <- as.data.frame(sample_data(final_physeq()))

    # Find the numeric indices of the selected column names
    selected_indices <- which(names(meta_df) %in% input$numeric_cols)

    if (length(group_vals) < 2) {
      showNotification("You need at least two groups to compare.", type = "error")
      return(NULL)
    }

    # Pre-allocate results
    mantel_tables <- list()
    mantel_objs <- list()

    for (g in group_vals) {
      d <- clone(dataset)
      d$sample_table <- d$sample_table[d$sample_table[[group_col]] == g, ]
      d$tidy_dataset()
      d$cal_betadiv()

      t <- trans_env$new(dataset = d, env_cols = selected_indices)
      t$cal_mantel(use_measure = "bray", partial_mantel = TRUE)

      x <- data.frame(spec = g, t$res_mantel)[, c(1, 3, 6, 8)]
      colnames(x) <- c("spec", "env", "r", "p.value")
      x <- x %>% mutate(
        rd = cut(r, breaks = c(-Inf, 0.3, 0.6, Inf), labels = c("< 0.3", "0.3 - 0.6", ">= 0.6")),
        pd = cut(p.value, breaks = c(-Inf, 0.01, 0.05, Inf), labels = c("< 0.01", "0.01 - 0.05", ">= 0.05"))
      )

      mantel_tables[[g]] <- x
      mantel_objs[[g]] <- t
    }

    combined_table <- do.call(rbind, mantel_tables)

    # Mantel plot
    output$mantel_plot <- renderPlot({
      req(mantel_objs[[1]], analysis_ready())

      MantCorr.Sn <- quickcor(mantel_objs[[1]]$data_env, type = "upper", cor.test = TRUE, show.diag = TRUE) +
        geom_square() +scale_fill_distiller(palette = "RdBu", direction = 1)+
        #geom_mark(sig.thres = 0.05, color = "black", size = 0) +
        anno_link(aes(colour = pd, size = rd), data = combined_table) +
        scale_size_manual(values = c(0.5, 1.5, 3)) +
        scale_colour_manual(values = c("#D95F02", "#1B9E77", "#A2A2A288")) +
        guides(size = guide_legend(title = "Mantel's r", override.aes = list(colour = "grey35"), order = 2),
               colour = guide_legend(title = "Mantel's p", override.aes = list(size = 3), order = 1),
               fill = guide_colorbar(title = "Pearson's r", order = 3))+
        theme(
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)
        )

      MantCorr.Sn
    })
    outputOptions(output, "mantel_plot", suspendWhenHidden = TRUE)
  })

  # Suspend plot rendering when hidden (only render when tab is active)
  outputOptions(output, "rda_plot", suspendWhenHidden = TRUE)
  outputOptions(output, "corr_plot", suspendWhenHidden = TRUE)
}
