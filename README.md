# Unmasking microbial diversity: the critical role of culture media and niche in capturing the grapevine microbiome (Data and R scripts)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16947382.svg)](https://doi.org/10.5281/zenodo.16947382)

## Structure
- data/: Input CSV files.  
- scripts/: R scripts for generating the figures.  
- output/: Output figures (PDF and TIFF, 600 dpi).  

## Requirements
- R >= 4.0  
- R packages: tidyverse, RColorBrewer, patchwork, Cairo, extrafont, showtext, ggtext, vegan, ggsignif, ComplexUpset, cowplot, pheatmap  

You can install them all with:  
install.packages(c("tidyverse", "RColorBrewer", "patchwork", "Cairo",
                   "extrafont", "showtext", "ggtext", "vegan",
                   "ggsignif", "ComplexUpset", "cowplot", "pheatmap"))

## Usage
Run the scripts from the scripts/ folder.  
Each script reads its input CSV from data/ and saves the figure in output/ as both PDF and TIFF (600 dpi, Times New Roman embedded).  

Example:
# Run one script
source("scripts/Figure_1.R")

# Run all scripts sequentially
source("scripts/Figure_1.R")
source("scripts/Figure_2.R")
...
source("scripts/Figure_8.R")

## Citation
If you use this repository, please cite:  
Villanueva-Llanes, M. P., Carbú Espinosa de los Monteros, M., Cantoral, J. M., & Cordero Bueso, G. (2025). Unmasking microbial diversity: the critical role of culture media and niche in capturing the grapevine microbiome (Data and R scripts) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.16947382

## License
This repository is licensed under the MIT License.
