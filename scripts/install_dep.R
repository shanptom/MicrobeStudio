# Install everything MicrobeStudio needs, with current package versions.
#
# Preferred for reproducing a tested setup:   renv::restore()   (see README)
# This script is for a fresh machine without a usable lockfile (e.g. a newer R).
#
# System libraries needed to compile some packages (Arch names; Debian/Ubuntu
# equivalents in parentheses):
#   unixodbc (unixodbc-dev)  - odbc, a hard dependency of shiny.telemetry
#   gdal geos proj udunits   - sf, a hard dependency of phylosmith
#                              (libgdal-dev libgeos-dev libproj-dev libudunits2-dev)
#   plus the usual: openssl, libcurl, libxml2, hdf5 (for rhdf5)

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")

cran_pkgs <- c(
  "shiny", "shinyjs", "shinycssloaders", "shiny.semantic", "semantic.assets",
  "shiny.telemetry", "DT", "markdown", "ragg", "plotly",
  "ggplot2", "ggpubr", "ggrepel", "ggalluvial", "RColorBrewer",
  "dplyr", "tidyr", "vegan", "permute", "microeco", "file2meco", "GUniFrac",
  "xgboost", "SHAPforxgboost", "Rtsne", "Matrix",
  "DBI", "RSQLite", "uuid", "httr", "withr", "testthat"
)
bioc_pkgs <- c("phyloseq")
github_pkgs <- c("gauravsk/ranacapa", "schuyler-smith/phylosmith", "Hy4m/ggcor")

installed <- rownames(installed.packages())
BiocManager::install(c(setdiff(cran_pkgs, installed), setdiff(bioc_pkgs, installed)), ask = FALSE, update = FALSE)

if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
for (g in github_pkgs) {
  if (!requireNamespace(sub(".*/", "", g), quietly = TRUE)) remotes::install_github(g, upgrade = "never")
}
