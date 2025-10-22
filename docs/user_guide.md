# MicrobeStudio User Manual

**MicrobeStudio** is an interactive Shiny application for exploring microbiome data through powerful visual analytics and statistical tools. Designed for streamlined phyloseq analysis, this guide walks you through each feature and provides detailed information on input formats to ensure a smooth experience.

---
## 1. Getting Started


### Quick Start Workflow

1. **Upload** your data (CSV files or phyloseq object)
2. **Filter** unwanted taxa and normalize data
3. **Explore** using various analysis modules
4. **Customize** visualizations and export results

---

## 2. Data Upload

Choose one of two supported formats:

### Option A: CSV Files (Recommended for beginners)

Upload three separate tables:

|File Type|Description|Format|
|---|---|---|
|**Count Table**|Abundance data|Rows = ASVs/Taxa, Columns = Samples|
|**Taxonomy Table**|Taxonomic classifications|Rows = ASVs/Taxa, Columns = Taxonomic ranks|
|**Metadata Table**|Sample information|Rows = Samples, Columns = Metadata variables|  
---



### Option B: phyloseq Object (.rds)

A pre-assembled R object containing all components in a single file.

 **💡 Pro Tip**: For phylogeny-based analyses, your phyloseq object must include a phylogenetic tree. See the [Phyloseq creation guide](#creating-a-phyloseq-object) below for instructions.

---

## 3. Data Filtering

Clean and prepare your data for analysis:

### Available Filters

- **Remove Unwanted Taxa**: Filter out contaminants (e.g., _Chloroplast_, _Mitochondria_)
- **Rarefaction**: Normalize sequencing depth across samples (optional but recommended)

### How to Apply Filters

1. Specify taxa to remove by name
2. Choose rarefaction depth (if desired)
3. Click **Apply Filtering**

> **🔄 Important**: Once filtering is complete, the cleaned dataset is used across all analysis modules automatically.

---

## 4. Analysis Modules

### 4.1 Rarefaction Plot

**Purpose**: Assess sequencing depth adequacy

**Features**:

- Visualize depth distribution across samples
- Color-code and organize by metadata categories
- Identify samples with insufficient coverage

### 4.2 Abundance Analysis

**Purpose**: Explore taxonomic composition

**Plot Types**:

- **Bar Plots**: Stacked relative abundance
- **Line Plots**: Abundance trends over samples/conditions
- **Heatmaps**: Color-coded abundance matrices

**Customization Options**:

- Select taxonomic rank (Genus, Family, etc.)
- Focus on top N most abundant taxa
- Arrange samples by metadata or manually
- Flip axes for temporal/stratigraphic data

### 4.3 Dendrogram

**Purpose**: Analyze sample relationships through clustering

**Features**:

- Multiple distance metrics available
- Group samples by metadata categories
- Adjustable label appearance

### 4.4 Alpha Diversity

**Purpose**: Measure within-sample diversity

**Available Metrics**:

- Shannon diversity
- Simpson diversity
- Other standard indices

**Customization**:

- Group by metadata categories
- Adjust sample ordering and colors
- Modify axis orientation and labels

### 4.5 Beta Diversity & Ordination

**Purpose**: Examine between-sample diversity patterns

**Ordination Methods**:

- **NMDS** (Non-metric Multidimensional Scaling)
- **PCoA** (Principal Coordinates Analysis)
- **tSNE** (t-distributed Stochastic Neighbor Embedding)
- Additional methods available

**Distance Measures**:

- **Bray-Curtis** (recommended for abundance data)
- **Jaccard** (presence/absence)
- **UniFrac** (requires phylogenetic tree)

**Statistical Testing**:

- **PERMANOVA**: Test for significant group differences

### 4.6 Metadata Analysis

**Purpose**: Correlate microbial communities with numerical environmental variables

**Variables**: pH, temperature, age, BMI, etc.

**Analysis Steps**:

1. **Select Numeric Columns**: Choose numerical variables.
2. **Create Environment Dataset**: Click **"Create trans_env"**
3. **Choose Analysis Type**:
    - **RDA (Redundancy Analysis)**: Quantify variation explained by metadata
    - **Correlation**: Direct relationships between genera and variables
    - **Mantel Test**: Distance-based correlations

> **⚠️ Critical**: Only select columns with numeric data. Text or factor data will cause errors.


### 4.7 Regression Analysis

**Purpose**: Model relationships between specific taxa and environmental variables

**Workflow**:

1. Select target taxon (e.g., specific genus)
2. Choose numeric environmental variable
3. Optionally group by metadata categories
4. Generate linear model plots

---

## 5. Tips & Best Practices

### General Usage

- **Tooltips**: Hover over inputs for contextual help
- **Manual Ordering**: Use comma-separated values for custom sample order
- **Dynamic Updates**: Plots refresh automatically with new selections

### Performance Optimization

- **Large Datasets**: Consider running locally for better performance
- **Demo Files**: Practice with sample datasets included in this repository (see `data/`)

### Common Issues & Solutions

- **Slow Performance**: Use local installation for large datasets
- **Analysis Errors**: Ensure metadata columns are properly formatted (numeric vs. categorical)
- **Missing Results**: Check that filtering step was completed successfully

---

## 6. Running MicrobeStudio Locally

For optimal performance with large datasets, run MicrobeStudio on your local machine.

### Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)
- Git (for cloning repository)

### Installation Steps

#### Step 1: Clone Repository

```bash
git clone https://github.com/shanptom/MicrobeStudio.git ~/Path/to/your/folder
```

#### Step 2: Install Dependencies

Run this command in R to install required packages:

```r
source("Path/to/install_dep.R")
```

#### Step 3: Launch Application

```r
library(shiny)
runApp('/Path/to/MicrobeStudio')
```

### HPC Cluster Setup (Optional)

For High-Performance Computing environments, load required modules first:

**Example for Ohio Supercomputer Center (OSC)**:

```bash
ml gcc/12.3.0
ml R/4.4.0
ml gdal/3.7.3
ml proj/9.2.1
ml geos/3.12.0
R
```

> **🖥️ Note**: Modify module commands based on your specific HPC environment.

---

## Getting Help

- **GitHub Issues**: Report bugs or request features
- **Documentation**: Additional guides available in the repository
---
