# From Soil to Grape – Data and R scripts

This repository contains input data (CSV) and R scripts used to generate figures of the manuscript  
*From Soil to Grape: Exploring the Culturable Microbiome of Grapevines Across Organs, Management Systems, and Culture Media*.

## Structure
- `data/`: Input CSV files (bacteria `_B`, fungi `_H`).
- `scripts/`: R scripts for generating the figures.
- `output/`: Output figures (PDF and TIFF, 600 dpi).

## Requirements
- R >= 4.0
- R packages: tidyverse, RColorBrewer, patchwork, Cairo

## Usage
Run the scripts from the `scripts/` folder:
```R
source("scripts/Figure1_bacteria_bars.R")
source("scripts/Figure4_fungi_bars.R")
