# Dendrogram Module Server
# Handles hierarchical clustering and dendrogram visualization

dendrogram_server <- function(input, output, session, final_physeq, analysis_ready) {

  # Custom dendrogram function with adjustable label size
  dendrogram_phyloseq_custom <- function(phyloseq_obj, treatment = NULL, method = "bray",
                                         colors = "default", label_size = 2.5) {
    dend <- phylosmith::dendrogram_phyloseq(
      phyloseq_obj = phyloseq_obj,
      treatment = treatment,
      method = method,
      colors = colors
    )

    # Identify and replace the geom_label layer with new size
    label_data <- NULL
    label_mapping <- NULL

    for (layer in dend$layers) {
      if (inherits(layer$geom, "GeomLabel") || inherits(layer$geom, "GeomText")) {
        label_data <- layer$data
        label_mapping <- layer$mapping
      }
    }

    if (!is.null(label_data)) {
      dend$layers <- Filter(function(l) {
        !inherits(l$geom, "GeomLabel") && !inherits(l$geom, "GeomText")
      }, dend$layers)

      dend <- dend +
        ggplot2::geom_label(
          data = label_data,
          mapping = label_mapping,
          size = label_size,
          color = "white",
          label.padding = unit(0.2, "lines"),
          fontface = "bold",
          hjust = 1.05
        )
    }

    return(dend)
  }

  # Treatment selector
  output$dend_treatment_selector <- renderUI({
    req(final_physeq())
    df <- as.data.frame(sample_data(final_physeq()))
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    selectInput("dend_treatment", "Select metadata column for grouping:",
                choices = categorical_cols,
                selected = categorical_cols[1] %||% "None")
  })

  # Dendrogram plot
  output$dendrogramPlot <- renderPlot({
    if (!analysis_ready() || is.null(final_physeq())) return(NULL)
    req(input$dend_method, input$dend_treatment, input$dend_label_size, input$dend_text_size)

    dend <- dendrogram_phyloseq_custom(
      phyloseq_obj = final_physeq(),
      treatment = input$dend_treatment,
      method = input$dend_method,
      label_size = input$dend_label_size
    )

    dend <- dend + theme(
      axis.text = element_text(size = input$dend_text_size),
      axis.title = element_text(size = input$dend_text_size),
      legend.text = element_text(size = input$dend_text_size),
      legend.title = element_text(size = input$dend_text_size)
    )

    dend
  })

  # Ensure UI elements render even when tab is hidden
  outputOptions(output, "dend_treatment_selector", suspendWhenHidden = FALSE)

  # Suspend plot rendering when hidden (only render when tab is active)
  outputOptions(output, "dendrogramPlot", suspendWhenHidden = TRUE)
}
