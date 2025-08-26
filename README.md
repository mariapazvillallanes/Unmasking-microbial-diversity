# From Soil to Grape: Culturable Microbiome of Grapevines  

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
source("scripts/Figure_12.R")

## Citation
If you use this repository, please cite:  
Villanueva-Llanes MP, Carbú M, Cantoral, JM, Cordero-Bueso G. (in preparation). From Soil to Grape: Exploring the Culturable Microbiome of Grapevines Across Organs, Management Systems, and Culture Media.

## License
This repository is licensed under the MIT License.
