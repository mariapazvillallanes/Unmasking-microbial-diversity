# Vineyard Microbiome Figures

This repository contains the data and R scripts used to generate the figures for the manuscript:

**Villanueva-Llanes MP, Carbú M, Cordero-Bueso G. (2025) [Title of the article]. *Journal Name*.**

---

## Structure

- `data/` : Input CSV files.  
- `scripts/` : R scripts for generating the figures.  
- `output/` : Output figures (PDF and TIFF, 600 dpi).  

---

## Requirements

- **R >= 4.0**

### R packages
The following R packages are required:  

- **Data manipulation & plotting**: `tidyverse`, `ggplot2`, `dplyr`, `RColorBrewer`, `patchwork`  
- **Graphics export & fonts**: `Cairo`, `extrafont`, `showtext`, `ggtext`  
- **Ecological analysis**: `vegan`  
- **Statistical annotations**: `ggsignif`  
- **Specialized visualization**: `ComplexUpset`, `cowplot`, `pheatmap`  

You can install them all with:  
```r
install.packages(c("tidyverse", "RColorBrewer", "patchwork", "Cairo",
                   "extrafont", "showtext", "ggtext", "vegan",
                   "ggsignif", "ComplexUpset", "cowplot", "pheatmap"))
