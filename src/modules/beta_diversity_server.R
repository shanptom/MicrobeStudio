# Beta Diversity Module Server
# Handles ordination plots, PERMANOVA, and t-SNE

beta_diversity_server <- function(input, output, session, final_physeq, show_tsne, analysis_ready, telemetry = NULL) {
  # Color selector
  output$beta_color_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("beta_color", "Color by:", choices = c("None", categorical_cols), selected = "None")
  })

  # Shape selector
  output$beta_shape_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("beta_shape", "Shape by:", choices = c("None", categorical_cols), selected = "None")
  })

  # Label selector
  output$beta_label_selector <- renderUI({
    req(final_physeq())
    cols <- colnames(sample_data(final_physeq()))
    selectInput("beta_label", "Label points by:", choices = c("None", cols), selected = "None")
  })

  # Facet selector
  output$beta_facet_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("beta_facet", "Facet by:", choices = c("None", categorical_cols), selected = "None")
  })

  # Facet order selector
  output$beta_facet_order_selector <- renderUI({
    req(final_physeq(), input$beta_facet)
    if (!is.null(input$beta_facet) && input$beta_facet != "None") {
      df <- as.data.frame(sample_data(final_physeq()))
      choices <- unique(df[[input$beta_facet]])
      selectizeInput("beta_facet_order", "Custom Facet Order (drag to reorder):",
        choices = choices, selected = choices, multiple = TRUE,
        options = list(plugins = list("drag_drop"))
      )
    } else {
      return(NULL)
    }
  })

  # Cached ordination computation (avoids recomputing distance + ordination twice)
  ordination_result <- reactive({
    req(final_physeq(), input$beta_dist, input$beta_ord)

    # Guard UniFrac when phylogeny is missing
    if (input$beta_dist %in% c("unifrac", "wunifrac")) {
      validate(need(
        !is.null(phyloseq::phy_tree(final_physeq(), errorIfNULL = FALSE)),
        "UniFrac requires a phylogenetic tree in the dataset."
      ))
    }

    # Seed for stochastic ordinations (e.g., NMDS)
    if (!is.null(input$ord_seed) && !is.na(input$ord_seed)) {
      set.seed(as.integer(input$ord_seed))
    }

    dist_mat <- distance(final_physeq(), method = input$beta_dist)
    ord_obj <- ordinate(final_physeq(), method = input$beta_ord, distance = dist_mat)
    list(dist = dist_mat, ord = ord_obj)
  })

  # Beta diversity ordination plot
  output$betaPlot <- renderPlot({
    if (!analysis_ready() || is.null(final_physeq())) {
      return(NULL)
    }
    req(input$beta_dist, input$beta_ord, input$beta_shape_size, input$beta_label_size)

    ord_res <- ordination_result()
    ord <- ord_res$ord

    # Safely check color and shape inputs
    color_val <- if (!is.null(input$beta_color) && input$beta_color != "None") input$beta_color else NULL
    shape_val <- if (!is.null(input$beta_shape) && input$beta_shape != "None") input$beta_shape else NULL

    p <- plot_ordination(final_physeq(), ord,
      color = color_val,
      shape = shape_val
    ) +
      theme_minimal()

    # Point geometry with alpha and optional jitter
    pos <- if (!is.null(input$beta_jitter) && isTRUE(input$beta_jitter)) {
      ggplot2::position_jitter(width = 0.05, height = 0.05)
    } else {
      ggplot2::position_identity()
    }
    p <- p + geom_point(
      size = input$beta_shape_size,
      alpha = if (is.null(input$beta_alpha)) 0.9 else input$beta_alpha,
      position = pos
    )

    # Labels only if selected
    if (!is.null(input$beta_label) && input$beta_label != "None") {
      if (!is.null(input$beta_label_repel) && isTRUE(input$beta_label_repel)) {
        p <- p + ggrepel::geom_text_repel(aes_string(label = input$beta_label),
          size = input$beta_label_text_size
        )
      } else {
        p <- p + geom_text(aes_string(label = input$beta_label),
          size = input$beta_label_text_size,
          vjust = -1
        )
      }
    }

    # Apply theme and text size
    p <- p + theme(
      axis.text = element_text(size = input$beta_label_size),
      axis.title = element_text(size = input$beta_label_size),
      legend.text = element_text(size = input$beta_label_size),
      legend.title = element_text(size = input$beta_label_size),
      strip.text = element_text(size = input$beta_label_size)
    )

    if (!is.null(input$beta_facet) && input$beta_facet != "None") {
      if (!is.null(input$beta_facet_order) && length(input$beta_facet_order) > 0) {
        p$data[[input$beta_facet]] <- factor(p$data[[input$beta_facet]], levels = input$beta_facet_order)
      }
      p <- p + facet_wrap(as.formula(paste("~", input$beta_facet)))
    }

    p
  })

  # Ordination info text (uses cached ordination result)
  output$ordination_info <- renderText({
    if (!analysis_ready() || is.null(final_physeq())) {
      return("")
    }
    req(input$beta_dist, input$beta_ord)

    ord_res <- tryCatch(ordination_result(), error = function(e) NULL)
    if (is.null(ord_res)) {
      return("")
    }
    ord <- ord_res$ord

    info <- NULL
    if (toupper(input$beta_ord) == "NMDS" && !is.null(ord$stress)) {
      stress_val <- round(ord$stress, 4)
      # Clarke (1993) interpretation scale for NMDS stress
      quality <- if (is.na(stress_val)) {
        "unknown"
      } else if (stress_val < 0.05) {
        "Excellent representation"
      } else if (stress_val < 0.1) {
        "Good representation"
      } else if (stress_val < 0.2) {
        "Fair — interpret with caution"
      } else {
        "Poor — do not interpret"
      }
      info <- paste0("NMDS stress = ", stress_val, " (", quality, ")")
    } else if (toupper(input$beta_ord) %in% c("PCOA", "MDS")) {
      # Try to extract variance explained
      rel <- tryCatch(ord$values$Relative_eig, error = function(e) NULL)
      if (!is.null(rel) && length(rel) >= 2) {
        info <- paste0("PCoA variance: Axis1 ", round(rel[1] * 100, 1), "%, Axis2 ", round(rel[2] * 100, 1), "%")
      }
    }
    if (is.null(info)) info <- paste("Method:", input$beta_ord)
    info
  })

  # Analysis summary
  output$analysis_summary <- renderText({
    if (!analysis_ready() || is.null(final_physeq())) {
      return("")
    }
    parts <- c(
      paste0("Distance = ", input$beta_dist),
      paste0("Ordination = ", input$beta_ord),
      if (!is.null(input$beta_color) && input$beta_color != "None") paste0("Color = ", input$beta_color) else NULL,
      if (!is.null(input$beta_shape) && input$beta_shape != "None") paste0("Shape = ", input$beta_shape) else NULL,
      if (!is.null(input$beta_label) && input$beta_label != "None") paste0("Labels = ", input$beta_label) else NULL
    )
    paste(parts, collapse = " | ")
  })

  # PERMANOVA group selector
  output$permanova_group_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]

    if (length(categorical_cols) == 0) {
      return(tagList(
        div(
          class = "ui warning message",
          "No categorical metadata columns found. Add a grouping variable to run PERMANOVA."
        ),
        selectInput("permanova_group", "Select grouping variable:",
          choices = character(0), selected = NULL
        )
      ))
    }

    selectInput("permanova_group", "Select grouping variable:",
      choices = categorical_cols,
      selected = categorical_cols[1]
    )
  })

  permanova_result <- reactiveVal()
  permdisp_text <- reactiveVal("")

  # Run PERMANOVA (vegan::adonis2 with explicit alignment)
  observeEvent(input$run_permanova, {
    if (!is.null(telemetry)) {
      log_tool_usage(telemetry, session, "beta_run_permanova")
    }
    req(final_physeq(), !is.null(input$permanova_group), nzchar(input$permanova_group))

    withProgress(message = "Running PERMANOVA...", value = 0, {
      tryCatch(
        {
          shinyjs::disable("run_permanova")
          on.exit(shinyjs::enable("run_permanova"), add = TRUE)
          incProgress(0.1, detail = "Preparing distance matrix")
          ps <- final_physeq()

          # Subset to samples with non-missing group values
          md <- as.data.frame(sample_data(ps), stringsAsFactors = FALSE)
          grp_name <- input$permanova_group
          if (!grp_name %in% colnames(md)) stop("Selected grouping variable not found in metadata")
          keep <- !is.na(md[[grp_name]])
          ps_sub <- prune_samples(keep, ps)

          md_sub <- as.data.frame(sample_data(ps_sub), stringsAsFactors = FALSE)
          grp_vec <- factor(md_sub[[grp_name]])
          if (nlevels(grp_vec) < 2) stop("Grouping variable must have at least two levels")

          # Use PERMANOVA-specific distance if available; otherwise fall back to ordination distance
          method <- if (!is.null(input$permanova_dist)) input$permanova_dist else if (!is.null(input$beta_dist)) input$beta_dist else "bray"
          if (method %in% c("unifrac", "wunifrac") && is.null(phyloseq::phy_tree(ps_sub, errorIfNULL = FALSE))) {
            stop("UniFrac requires a phylogenetic tree in the dataset.")
          }
          dist_obj <- beta_distance(ps_sub, method)

          # Explicitly align metadata rows to distance labels
          labs <- labels(dist_obj)
          metadata <- data.frame(group = grp_vec, row.names = sample_names(ps_sub))
          metadata <- metadata[labs, , drop = FALSE]

          perm_n <- if (!is.null(input$permanova_permutations) && !is.na(input$permanova_permutations)) as.integer(input$permanova_permutations) else 999L
          strata_vec <- NULL
          if (!is.null(input$permanova_strata) && nzchar(input$permanova_strata) && input$permanova_strata != "None") {
            strata_vec <- as.data.frame(sample_data(ps_sub), stringsAsFactors = FALSE)[[input$permanova_strata]][match(labs, sample_names(ps_sub))]
          }
          # Adjust pairwise p-values with the selected method; map to valid p.adjust keywords
          padj <- if (!is.null(input$p_adjust_method)) input$p_adjust_method else "BH"
          padj <- if (padj %in% c("BH", "BY")) padj else tolower(padj)

          incProgress(0.3, detail = "Running PERMANOVA")
          res <- run_permanova(dist_obj, metadata$group,
            strata = strata_vec, permutations = perm_n, seed = 123L,
            p_adjust = padj, permdisp = isTRUE(input$permdisp_enable)
          )
          out <- res$table
          out$measure <- method
          out <- out[, c("Groups", "measure", "F", "R2", "p.value", "p.adjusted", "n")]
          # Provenance columns so an exported table is self-describing
          out$permutations <- res$params$permutations
          out$seed <- res$params$seed
          out$p_adjust <- padj
          out$strata <- if (is.null(strata_vec)) "none" else input$permanova_strata

          # PERMDISP: a significant result means PERMANOVA differences may reflect
          # differences in dispersion (spread), not only location
          if (!is.null(res$permdisp)) {
            permdisp_text(paste0(
              "PERMDISP: F = ", signif(res$permdisp$F, 4), ", p = ", signif(res$permdisp$p.value, 4),
              if (res$permdisp$p.value <= 0.05) " - dispersions differ; interpret PERMANOVA with caution." else " - no evidence of unequal dispersion.",
              " (", perm_n, " permutations, seed ", res$params$seed, ")"
            ))
          } else {
            permdisp_text("")
          }

          # Significance codes: adjusted p-values for pairs, raw p for the global test
          sig_code <- function(p) {
            if (is.na(p)) {
              return("")
            }
            if (p <= 0.001) {
              return("***")
            }
            if (p <= 0.01) {
              return("**")
            }
            if (p <= 0.05) {
              return("*")
            }
            if (p <= 0.1) {
              return(".")
            }
            return("")
          }
          out$Significance <- vapply(ifelse(is.na(out$p.adjusted), out$p.value, out$p.adjusted), sig_code, character(1))
          incProgress(0.9, detail = "Complete")

          permanova_result(out)
        },
        error = function(e) {
          permanova_result(paste("PERMANOVA error:", e$message))
        }
      ) # end tryCatch
    }) # end withProgress
  })

  # PERMANOVA results table and download
  output$permanova_table <- DT::renderDataTable({
    req(permanova_result())
    df <- permanova_result()
    if (is.data.frame(df)) {
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, order = list(list(5, "asc"))))
    } else {
      # If error string
      DT::datatable(data.frame(Message = as.character(df)), rownames = FALSE, options = list(dom = "t"))
    }
  })

  output$download_permanova <- downloadHandler(
    filename = function() {
      paste0("permanova_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- permanova_result()
      if (is.data.frame(df)) {
        utils::write.csv(df, file, row.names = FALSE)
      } else {
        utils::write.csv(data.frame(Message = as.character(df)), file, row.names = FALSE)
      }
    }
  )

  # PERMANOVA strata selector
  output$permanova_strata_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("permanova_strata", "Permutation strata (optional):",
      choices = c("None", categorical_cols), selected = "None"
    )
  })

  # PERMDISP info output (define before outputOptions)
  output$permdisp_info <- renderText({
    permdisp_text()
  })

  # t-SNE group selector
  output$tsne_group_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selected_val <- if (length(categorical_cols) >= 2) categorical_cols[2] else categorical_cols[1] %||% "None"
    selectInput("tsne_group", "Group samples by:",
      choices = categorical_cols,
      selected = selected_val
    )
  })

  # t-SNE perplexity selector
  output$tsne_perplexity_selector <- renderUI({
    req(final_physeq())

    n_samples <- nsamples(final_physeq())
    # Rtsne requires 3 * perplexity < n_samples - 1 (and perplexity > 0)
    max_perplexity <- max(1, min(ceiling((n_samples - 1) / 3) - 1, 50))

    sliderInput("tsne_perplexity", "Perplexity:",
      min = 1, max = max_perplexity, value = min(5, max_perplexity)
    )
  })

  # t-SNE label selector
  output$tsne_label_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("tsne_label", "Label samples by:",
      choices = c("None", categorical_cols),
      selected = "None"
    )
  })

  # t-SNE plot
  output$tsne_plot <- renderPlot({
    req(input$run_tsne)

    if (!analysis_ready() || is.null(final_physeq())) {
      return(NULL)
    }

    # Require analysis to be ready and inputs
    req(input$tsne_group, analysis_ready())

    # Seed for t-SNE
    if (!is.null(input$tsne_seed) && !is.na(input$tsne_seed)) {
      withr::local_seed(as.integer(input$tsne_seed))
    }

    p <- tsne_phyloseq(
      phyloseq_obj = final_physeq(),
      treatment = input$tsne_group,
      perplexity = input$tsne_perplexity,
      circle = input$tsne_circle,
      labels = if (input$tsne_label != "None") input$tsne_label else NULL,
      colors = "default"
    )
    p
  })

  # t-SNE event handlers
  observeEvent(input$run_tsne, {
    if (!is.null(telemetry)) {
      log_tool_usage(telemetry, session, "beta_run_tsne")
    }
    show_tsne(TRUE)
  })

  observeEvent(input$reset_tsne, {
    show_tsne(FALSE)
  })

  output$show_tsne_flag <- reactive({
    show_tsne()
  })
  outputOptions(output, "show_tsne_flag", suspendWhenHidden = FALSE)

  # Ensure UI elements render even when tab is hidden
  outputOptions(output, "beta_color_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "beta_shape_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "beta_label_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "beta_facet_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "beta_facet_order_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "permanova_group_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "permanova_strata_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "permanova_table", suspendWhenHidden = FALSE)
  outputOptions(output, "tsne_group_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "tsne_perplexity_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "tsne_label_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "permdisp_info", suspendWhenHidden = FALSE)

  # Suspend plot rendering when hidden (only render when tab is active)
  outputOptions(output, "betaPlot", suspendWhenHidden = TRUE)
  outputOptions(output, "tsne_plot", suspendWhenHidden = TRUE)

  # Reset handlers
  observeEvent(input$reset_ordination_aes, {
    updateSliderInput(session, "beta_label_size", value = 12)
    updateSliderInput(session, "beta_label_text_size", value = 3)
    updateSliderInput(session, "beta_shape_size", value = 4)
    updateSliderInput(session, "beta_alpha", value = 0.9)
    updateCheckboxInput(session, "beta_jitter", value = FALSE)
    updateCheckboxInput(session, "beta_label_repel", value = TRUE)
    updateSelectInput(session, "beta_color", selected = "None")
    updateSelectInput(session, "beta_shape", selected = "None")
    updateSelectInput(session, "beta_label", selected = "None")
    updateSelectInput(session, "beta_facet", selected = "None")
  })

  observeEvent(input$reset_permanova, {
    updateSelectInput(session, "permanova_dist", selected = "bray")
    updateSelectInput(session, "permanova_group", selected = NULL)
    updateSelectInput(session, "permanova_strata", selected = "None")
    updateNumericInput(session, "permanova_permutations", value = 999)
    updateSelectInput(session, "p_adjust_method", selected = "BH")
    updateCheckboxInput(session, "permdisp_enable", value = TRUE)
  })

  # Download ordination plot as PDF
  output$download_ordination_plot <- downloadHandler(
    filename = function() paste0("ordination_plot_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch(
        {
          ord_res <- ordination_result()
          color_val <- if (!is.null(input$beta_color) && input$beta_color != "None") input$beta_color else NULL
          shape_val <- if (!is.null(input$beta_shape) && input$beta_shape != "None") input$beta_shape else NULL
          p <- plot_ordination(final_physeq(), ord_res$ord, color = color_val, shape = shape_val) +
            theme_minimal() +
            geom_point(size = input$beta_shape_size, alpha = if (is.null(input$beta_alpha)) 0.9 else input$beta_alpha) +
            theme(
              axis.text = element_text(size = input$beta_label_size),
              axis.title = element_text(size = input$beta_label_size),
              legend.text = element_text(size = input$beta_label_size),
              legend.title = element_text(size = input$beta_label_size)
            )
          ggplot2::ggsave(file, plot = p, width = 10, height = 8, device = "pdf")
        },
        error = function(e) {
          showNotification(paste("Download failed:", e$message), type = "error")
        }
      )
    }
  )
}
