
cran_pkgs <- c("shiny", "shinyjs", "shinycssloaders",
               "shiny.semantic", "semantic.assets", "markdown",
               "ggplot2", "vegan", "microeco", "file2meco", "GUniFrac",
               "ragg", "DT",
               "RColorBrewer", "ggalluvial", "dplyr", "ggpubr", "xgboost", "SHAPforxgboost")


installed <- rownames(installed.packages())
to_install <- setdiff(cran_pkgs, installed)
if (length(to_install)) install.packages(to_install)


if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
github_pkgs <- c("gauravsk/ranacapa", "schuyler-smith/phylosmith", "joey711/phyloseq")
lapply(github_pkgs, devtools::install_github)