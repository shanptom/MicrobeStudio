library(xgboost)
library(SHAPforxgboost)
library(Matrix)
library(tidyr)
library(dplyr)

run_shap_analysis <- function(phyloseq_obj, variable, group1, top_n = 10, font_size = 10) {
  
  # Ensure the phyloseq object has the specified variable
  if(!variable %in% colnames(sample_data(phyloseq_obj))) {
    stop("Selected variable not found in phyloseq metadata.")
  }
  
  # Glom to Genus level
  ps_filtered <- tax_glom(phyloseq_obj, taxrank = "Genus", NArm = FALSE)
  
  # Transpose OTU table
  asv_counts <- as.data.frame(t(otu_table(ps_filtered)))
  asv_counts$SampleID <- rownames(asv_counts)
  
  # Extract metadata
  meta <- data.frame(sample_data(ps_filtered), check.names = FALSE)
  meta$SampleID <- rownames(meta)
  
  # Merge
  df <- base::merge(asv_counts, meta, by = "SampleID")
  
  # Create binary label
  df$binary_label <- ifelse(df[[variable]] == group1, 1, 0)
  
  # Create feature matrix (X) and target vector (y)
  X <- df[, grepl("^ASV", colnames(df))]
  y <- df$binary_label
  
  # Check for at least two classes
  if (length(unique(y)) < 2) {
    stop("The selected variable must have at least two unique groups for comparison.")
  }
  
  # Convert to matrix for xgboost
  X_mat <- as(as.matrix(X), "dgCMatrix")
  dtrain <- xgb.DMatrix(data = X_mat, label = y)
  
  # Set up parameters
  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    scale_pos_weight = length(y[y == 0]) / length(y[y == 1])
  )
  
  # Train model
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  # Get SHAP values
  shap_result <- shap.values(xgb_model = xgb_model, X_train = as.matrix(X))
  
  # Prepare data for plotting
  shap_long <- shap.prep(shap_contrib = shap_result$shap_score, X_train = as.matrix(X))
  
  # Create summary plot
  shap_plot <- shap.plot.summary.wrap2(shap_score = shap_result$shap_score, X = X, top_n = top_n)
  
  # Get the ASV IDs from the plot data
  plot_asvs <- as.character(unique(shap_plot$data$variable))
  
  # Get the taxonomy table from the phyloseq object
  tax_table_df <- as.data.frame(tax_table(ps_filtered))
  tax_table_df$ASV_ID <- rownames(tax_table_df)
  
  # Filter for the ASVs in the plot
  plot_taxa_info <- tax_table_df[tax_table_df$ASV_ID %in% plot_asvs, ]
  
  # Create the new labels (Genus or ASV ID if Genus is NA)
  plot_taxa_info$DisplayLabel <- ifelse(
    is.na(plot_taxa_info$Genus) | plot_taxa_info$Genus == "",
    plot_taxa_info$ASV_ID,
    plot_taxa_info$Genus
  )
  
  # Create a named vector for the labels argument
  new_labels <- setNames(plot_taxa_info$DisplayLabel, plot_taxa_info$ASV_ID)
  
  # Apply the new labels and font size to the plot
  shap_plot <- shap_plot +
    scale_y_discrete(labels = new_labels) +
    theme(text = element_text(size = font_size))
  
  # Get top taxa data for the table and add Genus name
  top_taxa_data <- as.data.frame(shap_long) %>%
    filter(variable %in% plot_asvs)
  
  # Add Genus information to the table
  top_taxa_data <- top_taxa_data %>%
    left_join(select(plot_taxa_info, ASV_ID, DisplayLabel), by = c("variable" = "ASV_ID")) %>%
    rename(Taxon = DisplayLabel)
  
  # Return both the plot and the data table
  return(list(plot = shap_plot, table = top_taxa_data))
}