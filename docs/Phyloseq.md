# Creating a Phyloseq Object

The `phyloseq` package in R is a powerful and widely used tool for analyzing and visualizing microbial community data from metabarcoding studies. It integrates various types of data into a single structured object, enabling efficient and reproducible analysis.

A **phyloseq object** typically consists of the following components:

---

### 1. Count Table (OTU/ASV Table)
This table contains the number of times each taxon (e.g., OTU or ASV) was observed in each sample. Rows represent taxa and columns represent samples.

**Example:**

| Taxa  | Sample1 | Sample2 | Sample3 |
|-------|---------|---------|---------|
| Taxa1 | 10      | 15      | 20      |
| Taxa2 | 12      | 7       | 0       |
| Taxa3 | 3       | 14      | 24      |

---

### 2. Taxonomy Table (Taxa Table)
This table describes the taxonomic classification of each taxon. Each row corresponds to a taxon, and columns represent taxonomic levels such as Kingdom, Phylum, Class, Order, Family, Genus, and Species.

**Example:**

| Taxa  | Kingdom  | Phylum     | Class         | Order        | Family         | Genus      | Species     |
|-------|----------|------------|---------------|--------------|----------------|------------|-------------|
| Taxa1 | Bacteria | Proteobacteria | Alphaproteobacteria | Rhizobiales | Rhizobiaceae | Rhizobium  | R. leguminosarum |
| Taxa2 | Bacteria | Actinobacteria | Actinobacteria     | Actinomycetales | Micrococcaceae | Arthrobacter | A. globiformis |
| Taxa3 | Bacteria | Firmicutes  | Bacilli        | Bacillales   | Bacillaceae    | Bacillus   | B. subtilis |

---

### 3. Sample Metadata
This table includes descriptive information about each sample, such as sampling location, time point, environmental variables, or experimental conditions.

**Example:**

| SampleID | Location     | pH  | Temperature | Season | Treatment |
|----------|--------------|-----|-------------|--------|-----------|
| Sample1  | Lake_North    | 7.2 | 15.3°C      | Spring | Control   |
| Sample2  | Lake_South    | 6.8 | 18.1°C      | Summer | Treated   |
| Sample3  | Lake_East     | 7.0 | 14.7°C      | Fall   | Control   |

---

### 4. Reference Sequences (Optional)
These are the actual DNA sequences (usually ASVs or OTUs) representing each taxon, often in FASTA format. This component is useful for downstream functional or phylogenetic analysis.

---

### 5. Phylogenetic Tree (Optional)
A phylogenetic tree inferred from the reference sequences, often using tools such as `DECIPHER`, `phangorn`, or external software (e.g., FastTree). It helps in calculating phylogeny-aware metrics like UniFrac distances.

---

### 6. Creating a Phyloseq object from csv files

**Expected Input Files**

| File Name            | Description                           |
| -------------------- | ------------------------------------- |
| `count_table.csv`    | ASV/OTU count table (taxa x samples)  |
| `taxonomy_table.csv` | Taxonomic classification per ASV      |
| `metadata.csv`       | Sample metadata (samples x variables) |

---

### Required Package

Open R and  Install latest version of `Phyloseq` from [here](https://joey711.github.io/phyloseq/install.html)

```r
library(phyloseq) 
````

---

### 1. Load the Taxonomy Table

The taxonomy table should have taxa/ASVs as row names and taxonomic ranks as columns.

**Code:**

```r
tax <- read.csv("taxonomy_table.csv", header = TRUE, row.names = 1)
tax <- tax_table(as.matrix(tax))
```

---

### 2. Load the ASV/OTU Count Table

The count table should have ASVs as row names and samples as column names.

**Code:**

```r
asv_counts <- read.csv("asv_table.csv", header = TRUE, row.names = 1)
asv_counts <- otu_table(as.matrix(asv_counts), taxa_are_rows = TRUE)
```

---

###  3. Load the Sample Metadata

The metadata should have sample names as row names.

**Code:**

```r
meta <- read.csv("metadata.csv", header = TRUE, row.names = 1)
meta <- sample_data(meta)
```

---

###  4. Combine into a Phyloseq Object

Now combine all components into a single `phyloseq` object:

```r
physeq <- phyloseq(asv_counts, tax, meta)
```

---
You can now use this `physeq` object for downstream analysis and visualization in `phyloseq`.
###  Summary

A complete phyloseq object provides a coherent structure for integrating and analyzing the following components:

- **Count Data** → What taxa are found in each sample and how abundant?
- **Taxonomic Classification** → What are these taxa?
- **Sample Metadata** → What conditions or contexts do the samples represent?
- **(Optional) DNA Sequences & Phylogenies** → What are the evolutionary relationships between taxa?

Together, these elements enable robust exploration of microbial community composition, structure, and relation to metadata.

| Function           | Description                          |
| ------------------ | ------------------------------------ |
| `read.csv()`       | Load all data tables as CSV files    |
| `as.matrix()`      | Convert to matrix for phyloseq input |
| `tax_table()`      | Create taxonomic data                |
| `otu_table()`      | Create ASV count data                |
| `sample_data()`    | Create sample metadata               |
| `phyloseq()`       | Combine into a single phyloseq object|

# Constructing a Phylogenetic Tree from Reference Sequences in a Phyloseq Workflow

Phylogenetic trees are essential for calculating phylogeny-aware diversity metrics like **UniFrac** and for conducting null model analyses. Below is a detailed step-by-step guide to build a phylogenetic tree from your reference sequences, starting from a `phyloseq` object or an external FASTA file.

---

###  1. Extract Reference Sequences

You can extract reference sequences directly from a `phyloseq` object or load them from an external FASTA file. Ensure the sequence names (headers) exactly match the taxon/ASV names in your count table.

#### Option A: From Phyloseq Object
```r
refseq <- ps@refseq
````
where `ps` is your _phyloseq_ object
#### Option B: From FASTA File

```r
library(Biostrings)
refseq <- readDNAStringSet("refseqs.fasta")
```

> ⚠️ **Important**: Taxon/ASV names in the FASTA headers must exactly match the taxa names in your OTU/ASV table.

---

###  2. Multiple Sequence Alignment

Use the `DECIPHER` package to align sequences. This step is compute-intensive; request sufficient resources if running on an HPC cluster (e.g., OSC).

```r
library(DECIPHER)
alignment <- AlignSeqs(refseq, anchor = NA, processors = 48)
```

---

### 3. Convert Alignment to PhyDat Format

Convert the aligned sequences into a format suitable for distance and tree calculations.

```r
library(phangorn)
phang.align <- phyDat(as(alignment, "matrix"), type = "DNA")
```

---

### 4. Calculate Distance Matrix

Use maximum likelihood distance based on the alignment.

```r
dm <- dist.ml(phang.align)
```

---

###  5. Construct Initial Tree Using Neighbor Joining

```r
treeNJ <- NJ(dm)
```

---

###  6. Fit Tree Using the pml Function

```r
fit <- pml(treeNJ, data = phang.align)
```

---

### 7. Update Tree Using GTR Model (with inv/gamma options)

This step improves the model fit. However, it is **computationally intensive**, especially with large datasets. Use `update()` to test parameters incrementally to avoid crashes.

```r
fitGTR <- update(fit, k = 4, inv = 0.2)
```

> 🧠 **Tip**:
> 
> - If `update()` crashes or takes too long, start with lower values like `k = 2` and `inv = 0`.
>     
> - Increase `k` (number of gamma rate categories) and `inv` (proportion of invariant sites) gradually.
>     
> - You can use `AIC(fitGTR)` to evaluate model fit.
>     

---

###  8. Save the Fitted Tree Object

```r
saveRDS(fitGTR, file = "fitGTR.rds")
```

### 9. Combine with `Phyloseq` object
Use this tree in downstream diversity analyses, like UniFrac, or integrate it into your `phyloseq` object using `phy_tree()`.

```r
ps <- merge_phyloseq(ps, phy_tree(fitGTR$tree))
```
---

### Notes on Runtime

- Tree construction is a **time-consuming** process.
- For ~90 samples and 48 cores on HPC, this step can take **6-8 hours**.
- Always request computational resources based on your dataset size.
    

---

### Summary

|Step|Description|
|---|---|
|1.|Extract reference sequences (from phyloseq or FASTA)|
|2.|Align sequences with `DECIPHER::AlignSeqs`|
|3.|Convert alignment to `phyDat`|
|4.|Compute distance matrix with `phangorn::dist.ml`|
|5.|Build initial NJ tree|
|6.|Fit model with `pml()`|
|7.|Optimize with `update()` using GTR|
|8.|Save tree object for later use|

