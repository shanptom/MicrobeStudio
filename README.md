# `MicrobeStudio`: Streamlined Phyloseq Analysis for Microbial Communities

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![R Shiny](https://img.shields.io/badge/built%20with-R%20Shiny-blue)](https://shiny.rstudio.com/)
[![Status](https://img.shields.io/badge/status-active-brightgreen)]()

`MicrobeStudio` is an interactive R Shiny application for exploring microbial community data. Designed for researchers and students analyzing high-throughput sequencing data (e.g., 16S/18S rRNA metabarcoding), `MicrobeStudio` provides a no-code interface to perform filtering, diversity analysis, ordination, and more—all from your web browser. This approach eliminates the need for programming expertise, making complex microbiome analysis accessible to a wider audience by simplifying the handling of intricate datasets with a smooth, intuitive workflow.

---

## Demo

A live version of the application is deployed on shinyapps.io.

**[🚀 Launch MicrobeStudio](https://shanptom-gopica.share.connect.posit.cloud)**

---
## 🚀 Features

### Data Input & Processing
- **Flexible Data Upload**: Load your data as ASV, taxonomy, and metadata tables (CSV) or as a pre-built `phyloseq` `.rds` object.
- **Data Filtering**: Interactively remove unwanted taxa and apply normalization (TSS) or rarefaction.

### Analysis & Visualization
- **Alpha & Beta Diversity**: Generate plots for Shannon, Simpson, Chao1, and more. Perform PERMANOVA tests on beta diversity.
- **Ordination**: Visualize community structure using NMDS, PCoA, RDA, CCA, and t-SNE.
- **Abundance Analysis**: Create bar plots, heatmaps, and alluvial plots by taxonomic rank.
- **Metadata Analysis**: Run constrained ordination (RDA/CCA/dbRDA), correlation analysis, and Mantel tests to link environmental variables to community composition.
- **Regression**: Relate individual taxon abundances to metadata variables.
- **Hierarchical Clustering**: Build dendrograms to visualize sample similarity.

---

## 📂 Getting Started

### Prerequisites
- R (version 4.0 or later)
- RStudio (recommended for an optimal experience)

### Step 1: Clone the Repository
Clone this repository to your local machine to get started:
```bash
git clone https://github.com/shanptom/PhyloFlow.git
cd PhyloFlow
```

### Step 2: Install Dependencies
Run the following script from the R console to install all required dependencies:
```R
source("scripts/install_dep.R")
```
This script will install packages from CRAN, Bioconductor, and GitHub. If you encounter installation issues, check package compatibility or consult the GitHub issues page for troubleshooting tips.
It also installs the **ragg** package, which MicrobeStudio uses to render plots on systems without an X11 display.

### Step 3: Run the Application
Once all dependencies are installed, launch the application in one of two ways:
1. **From RStudio**: Open the `app.R` file and click the "Run App" button in the top-right corner of the editor panel.
2. **From the R Console**:
   ```R
   shiny::runApp("app.R")
   ```

The application will open in a new window or your default web browser.

For a more comprehensive walkthrough of the app, see the [User Guide](docs/user_guide.md).

---

## 💾 Input Data Requirements

You can upload your data in two ways:

### Option 1: CSV Files
- **ASV/OTU Table**: A CSV file where rows are taxa and columns are samples. The first column should contain the taxon names/IDs.
  - **Example Format**:
    | Taxon_ID | Sample1 | Sample2 | Sample3 |
    |----------|---------|---------|---------|
    | Taxon1   | 100     | 50      | 75      |
    | Taxon2   | 30      | 20      | 10      |

- **Taxonomy Table**: A CSV file where rows are taxa and columns are taxonomic ranks (e.g., Kingdom, Phylum, Class, Order, Family, Genus, Species). The first column must match the taxon names/IDs from the ASV table.
  - **Example Format**:
    | Taxon_ID | Kingdom  | Phylum          | Class              | Order             | Family              | Genus         | Species      |
    |----------|----------|-----------------|--------------------|-------------------|---------------------|---------------|--------------|
    | Taxon1   | Bacteria | Proteobacteria  | Gammaproteobacteria| Enterobacteriales | Enterobacteriaceae  | Escherichia   | coli         |
    | Taxon2   | Bacteria | Firmicutes      | Bacilli            | Bacillales        | Bacillaceae         | Bacillus      | subtilis     |

- **Metadata Table**: A CSV file where rows are samples and columns are metadata variables. The first column must contain the sample names, matching the sample names in the ASV table.
  - **Example Format**:
    | Sample_ID | Location | Depth | Temperature |
    |-----------|----------|-------|-------------|
    | Sample1   | SiteA    | 10    | 25.5        |
    | Sample2   | SiteB    | 15    | 22.0        |
    | Sample3   | SiteC    | 8     | 23.5        |

### Option 2: Phyloseq Object
- **`.rds` file**: A single `.rds` file containing a valid `phyloseq` object.

Sample data is available in the `data/` directory for testing and learning purposes.

---

## 🔧 Normalization Options

- **None**: Use raw counts (default). Suitable when you want to analyze original data without transformation.
- **Rarefaction**: Subsample all samples to a common read depth. Useful for comparing diversity across samples with varying sequencing depths.
- **TSS**: Total Sum Scaling, which transforms counts to relative abundances. Ideal for comparing proportional differences between taxa.

---

## 🧠 How It Works

`MicrobeStudio` combines several powerful tools behind the scenes to make microbiome analysis straightforward. It's a modular Shiny application built on top of leading R packages from the microbiome analysis ecosystem.

- It uses `phyloseq` as the core data structure, seamlessly converting all user-uploaded CSVs into a `phyloseq` object for analysis.
- For advanced analysis and visualization (e.g., ordination, correlation, regression), it leverages the robust classes from the `microeco` package.
- The user interface is crafted with `shiny` and `bslib` for a smooth experience, with plots generated primarily using `ggplot2` and `phylosmith`.

---

## 🤝 Contact & Contributions

For feedback, bug reports, or feature suggestions, please [open an issue](https://github.com/shanptom/PhyloFlow/issues) on the GitHub repository.

---

## 📜 License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

---

## 📚 Citation

If you use `MicrobeStudio` in a publication, please cite the underlying R packages that power the app:

- **phyloseq**: McMurdie & Holmes (2013). *phyloseq: An R package for reproducible interactive analysis and graphics of microbiome census data*. PLoS ONE.
- **microeco**: Liu et al. (2021). *microeco: an R package for data mining in microbial community ecology*. bioRxiv.
- **phylosmith**: Smith (2021). *phylosmith: An R-package for reproducible and efficient microbiome analysis with phyloseq-objects*. Journal of Open Source Software.
- **VEGAN**: Dixon (2003). *VEGAN, a package of R functions for community ecology*. Journal of Vegetation Science.
- **ggplot2**: Wickham (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer.

We also acknowledge the extensive use of `shiny`, `shinyjs`, `ggcor`, `RColorBrewer`, `ranacapa`, and other essential packages that form the foundation of this tool.
