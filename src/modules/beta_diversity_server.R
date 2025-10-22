# Beta Diversity Module Server
# Handles ordination plots, PERMANOVA, and t-SNE

beta_diversity_server <- function(input, output, session, final_physeq, show_tsne, analysis_ready) {

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
                     options = list(plugins = list('drag_drop')))
    } else {
      return(NULL)
    }
  })

  # Beta diversity ordination plot
  output$betaPlot <- renderPlot({
    if (!analysis_ready() || is.null(final_physeq())) return(NULL)
    req(input$beta_dist, input$beta_ord, input$beta_shape_size, input$beta_label_size)

    # Guard UniFrac when phylogeny is missing
    if (input$beta_dist %in% c("unifrac", "wunifrac")) {
      if (is.null(phyloseq::phy_tree(final_physeq(), errorIfNULL = FALSE))) {
        validate(need(FALSE, "UniFrac requires a phylogenetic tree in the dataset."))
      }
    }

    # Seed for stochastic ordinations (e.g., NMDS)
    if (!is.null(input$ord_seed) && !is.na(input$ord_seed)) {
      set.seed(as.integer(input$ord_seed))
    }

    dist <- distance(final_physeq(), method = input$beta_dist)
    ord <- ordinate(final_physeq(), method = input$beta_ord, distance = dist)

    # Safely check color and shape inputs
    color_val <- if (!is.null(input$beta_color) && input$beta_color != "None") input$beta_color else NULL
    shape_val <- if (!is.null(input$beta_shape) && input$beta_shape != "None") input$beta_shape else NULL

    p <- plot_ordination(final_physeq(), ord,
                         color = color_val,
                         shape = shape_val) +
      theme_minimal()

    # Point geometry with alpha and optional jitter
    pos <- if (!is.null(input$beta_jitter) && isTRUE(input$beta_jitter)) {
      ggplot2::position_jitter(width = 0.05, height = 0.05)
    } else {
      ggplot2::position_identity()
    }
    p <- p + geom_point(size = input$beta_shape_size,
                        alpha = if (is.null(input$beta_alpha)) 0.9 else input$beta_alpha,
                        position = pos)

    # Labels only if selected
    if (!is.null(input$beta_label) && input$beta_label != "None") {
      if (!is.null(input$beta_label_repel) && isTRUE(input$beta_label_repel)) {
        p <- p + ggrepel::geom_text_repel(aes_string(label = input$beta_label),
                                          size = input$beta_label_text_size)
      } else {
        p <- p + geom_text(aes_string(label = input$beta_label),
                           size = input$beta_label_text_size,
                           vjust = -1)
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

  # Ordination info text (e.g., NMDS stress, PCoA variance)
  output$ordination_info <- renderText({
    if (!analysis_ready() || is.null(final_physeq())) return("")
    req(input$beta_dist, input$beta_ord)
    dist <- tryCatch(distance(final_physeq(), method = input$beta_dist), error = function(e) NULL)
    if (is.null(dist)) return("")
    ord <- tryCatch(ordinate(final_physeq(), method = input$beta_ord, distance = dist), error = function(e) NULL)
    if (is.null(ord)) return("")

    info <- NULL
    if (toupper(input$beta_ord) == "NMDS" && !is.null(ord$stress)) {
      info <- paste0("NMDS stress = ", round(ord$stress, 3))
      if (!is.na(ord$stress) && ord$stress > 0.2) info <- paste0(info, " (high stress)")
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
    if (!analysis_ready() || is.null(final_physeq())) return("")
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
        div(class = "ui warning message",
            "No categorical metadata columns found. Add a grouping variable to run PERMANOVA."),
        selectInput("permanova_group", "Select grouping variable:",
                    choices = character(0), selected = NULL)
      ))
    }

    selectInput("permanova_group", "Select grouping variable:",
                choices = categorical_cols,
                selected = categorical_cols[1])
  })

  permanova_result <- reactiveVal()
  permdisp_text <- reactiveVal("")

  # Run PERMANOVA (vegan::adonis2 with explicit alignment)
  observeEvent(input$run_permanova, {
    req(final_physeq(), !is.null(input$permanova_group), nzchar(input$permanova_group))

    tryCatch({
      shinyjs::disable("run_permanova")
      on.exit(shinyjs::enable("run_permanova"), add = TRUE)
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
      dist_obj <- as.dist(phyloseq::distance(ps_sub, method = method))

      # Explicitly align metadata rows to distance labels
      labs <- labels(dist_obj)
      metadata <- data.frame(group = grp_vec, row.names = sample_names(ps_sub))
      metadata <- metadata[labs, , drop = FALSE]

      # Helper to extract F and R2 robustly from adonis2 results
      extract_stats <- function(ad2_res) {
        df <- as.data.frame(ad2_res)
        if (!nrow(df)) return(list(F = NA_real_, R2 = NA_real_, p = NA_real_))
        # The first row corresponds to the model term 'group'
        f_col <- if ("F" %in% colnames(df)) "F" else if ("F.Model" %in% colnames(df)) "F.Model" else NA
        Fval <- if (!is.na(f_col)) suppressWarnings(as.numeric(df[[f_col]][1])) else NA_real_
        R2val <- suppressWarnings(as.numeric(df[["R2"]][1]))
        p_col_name <- "Pr(>F)"
        pval <- if (p_col_name %in% colnames(df)) suppressWarnings(as.numeric(df[[p_col_name]][1])) else NA_real_
        list(F = Fval, R2 = R2val, p = pval)
      }

      # Global PERMANOVA (not shown, but could be reported if needed)
      # global_res <- vegan::adonis2(dist_obj ~ group, data = metadata, permutations = perm_n)

      # Optional: PERMDISP (homogeneity of dispersion)
      if (!is.null(input$permdisp_enable) && isTRUE(input$permdisp_enable)) {
        bd <- vegan::betadisper(dist_obj, metadata$group)
        perm_n <- if (!is.null(input$permanova_permutations) && !is.na(input$permanova_permutations)) as.integer(input$permanova_permutations) else 999
        pd <- vegan::permutest(bd, permutations = perm_n)
        pval_pd <- tryCatch(pd$tab[1, "Pr(>F)"], error = function(e) NA_real_)
        permdisp_text(paste0("Global test p-value = ", signif(as.numeric(pval_pd), 4)))
      } else {
        permdisp_text("")
      }

      # Pairwise PERMANOVA across group levels
      lvls <- levels(metadata$group)
      if (length(lvls) < 2) stop("Grouping variable must have at least two levels")

      pairs <- utils::combn(lvls, 2, simplify = FALSE)
      rows <- lapply(pairs, function(pr) {
        # Subset samples belonging to the two groups
        keep_samples <- rownames(metadata)[metadata$group %in% pr]
        ps_pair <- prune_samples(keep_samples, ps_sub)
        dist_pair <- as.dist(phyloseq::distance(ps_pair, method = method))
        labs_p <- labels(dist_pair)
        md_pair <- as.data.frame(sample_data(ps_pair), stringsAsFactors = FALSE)
        md_p <- data.frame(group = factor(md_pair[[grp_name]], levels = pr),
                           row.names = sample_names(ps_pair))
        # Optional strata
        if (!is.null(input$permanova_strata) && nzchar(input$permanova_strata) && input$permanova_strata != "None") {
          md_p$Strata <- factor(md_pair[[input$permanova_strata]])
        }
        md_p <- md_p[labs_p, , drop = FALSE]
        perm_n <- if (!is.null(input$permanova_permutations) && !is.na(input$permanova_permutations)) as.integer(input$permanova_permutations) else 999
        if (!is.null(md_p$Strata)) {
          ad2 <- vegan::adonis2(dist_pair ~ group, data = md_p, permutations = perm_n, strata = md_p$Strata)
        } else {
          ad2 <- vegan::adonis2(dist_pair ~ group, data = md_p, permutations = perm_n)
        }
        st <- extract_stats(ad2)
        data.frame(
          Groups = paste(pr[1], "vs", pr[2]),
          measure = method,
          F = st$F,
          R2 = st$R2,
          p.value = st$p,
          stringsAsFactors = FALSE
        )
      })

      out <- do.call(rbind, rows)
      # Adjust p-values using selected method; map to valid p.adjust keywords
      padj <- if (!is.null(input$p_adjust_method)) input$p_adjust_method else "BH"
      method <- if (padj %in% c("BH", "BY")) padj else tolower(padj)
      out$p.adjusted <- p.adjust(out$p.value, method = method)
      # Add significance codes based on adjusted p-values
      sig_code <- function(p) {
        if (is.na(p)) return("")
        if (p <= 0.001) return("**")
        if (p <= 0.01) return("**")
        if (p <= 0.05) return("*")
        if (p <= 0.1) return(".")
        return("")
      }
      out$Significance <- vapply(out$p.adjusted, sig_code, character(1))

      permanova_result(out)
    }, error = function(e) {
      permanova_result(paste("PERMANOVA error:", e$message))
    })
  })

  # PERMANOVA results table and download
  output$permanova_table <- DT::renderDataTable({
    req(permanova_result())
    df <- permanova_result()
    if (is.data.frame(df)) {
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, order = list(list(5, 'asc'))))
    } else {
      # If error string
      DT::datatable(data.frame(Message = as.character(df)), rownames = FALSE, options = list(dom = 't'))
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
                choices = c("None", categorical_cols), selected = "None")
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
                selected = selected_val)
  })

  # t-SNE perplexity selector
  output$tsne_perplexity_selector <- renderUI({
    req(final_physeq())

    n_samples <- nsamples(final_physeq())
    max_perplexity <- floor((n_samples - 1) / 3)
    max_perplexity <- max(5, min(max_perplexity, 50))  # reasonable bounds

    sliderInput("tsne_perplexity", "Perplexity:",
                min = 0, max = max_perplexity, value = 2)
  })

  # t-SNE label selector
  output$tsne_label_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("tsne_label", "Label samples by:",
                choices = c("None", categorical_cols),
                selected = "None")
  })

  # t-SNE plot
  output$tsne_plot <- renderPlot({
    req(input$run_tsne)

    if (!analysis_ready() || is.null(final_physeq())) return(NULL)

    # Require analysis to be ready and inputs
    req(input$tsne_group, analysis_ready())

    # Seed for t-SNE
    if (!is.null(input$tsne_seed) && !is.na(input$tsne_seed)) {
      set.seed(as.integer(input$tsne_seed))
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
    updateCheckboxInput(session, "permdisp_enable", value = FALSE)
  })
  
}
