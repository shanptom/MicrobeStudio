# Helper functions for MicrobeStudio Shiny Application
# This file contains reusable functions to reduce code duplication in the application.

#' Reorder Factor Column in Plot Data
#' 
#' Reorders a factor column in a plot's data based on a custom order string.
#' 
#' @param p The plot object.
#' @param group_var The variable to reorder.
#' @param order_string A comma-separated string specifying the custom order.
#' @return The modified plot object.
reorder_factor_column <- function(p, group_var, order_string) {
  if (order_string != "" && group_var %in% colnames(p$data)) {
    custom_order <- trimws(unlist(strsplit(order_string, ",")))
    p$data[[group_var]] <- factor(p$data[[group_var]], levels = custom_order)
  }
  return(p)
}

#' Custom Dendrogram for Phyloseq
#' 
#' Creates a customized dendrogram plot for a phyloseq object with adjustable label size.
#' 
#' @param phyloseq_obj The phyloseq object.
#' @param treatment The metadata column for grouping.
#' @param method The distance method to use.
#' @param colors The color scheme for the dendrogram.
#' @param label_size The size of the labels.
#' @return The dendrogram plot.
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

#' Apply Consistent Theme to Plots
#' 
#' Applies a consistent theme to plots with customizable text sizes.
#' 
#' @param plot The plot object to apply the theme to.
#' @param axis_text_size Size of axis text.
#' @param axis_title_size Size of axis titles.
#' @param legend_text_size Size of legend text.
#' @param legend_title_size Size of legend title.
#' @param strip_text_size Size of strip text (for faceted plots).
#' @return The plot with the applied theme.
apply_plot_theme <- function(plot, axis_text_size = 12, axis_title_size = 12, 
                             legend_text_size = 12, legend_title_size = 12, 
                             strip_text_size = 12) {
  plot +
    theme(
      axis.text = element_text(size = axis_text_size),
      axis.title = element_text(size = axis_title_size),
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_title_size),
      strip.text = element_text(size = strip_text_size)
    )
}
