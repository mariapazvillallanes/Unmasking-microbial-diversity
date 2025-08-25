# ---- Packages ----
library(pheatmap)
library(tidyverse)
library(RColorBrewer)
library(Cairo)

# ---- Read data ----
raw_data <- read.csv("data/Figure_6.csv", sep = ";", check.names = FALSE, row.names = 1)

# Convertir coma -> punto y a numérico
numeric_data <- as.data.frame(lapply(raw_data, function(x) as.numeric(gsub(",", ".", x))))
rownames(numeric_data) <- rownames(raw_data)

# TOP20 géneros globales
top20_genera <- numeric_data |>
  rowSums(na.rm = TRUE) |>
  sort(decreasing = TRUE) |>
  head(20) |>
  names()

data_top20 <- numeric_data[top20_genera, , drop = FALSE]

# Clustering por filas
hclust_rows <- hclust(dist(data_top20, method = "euclidean"), method = "ward.D2")

# Ceros a NA
data_top20[data_top20 == 0] <- NA

# Valores como %
formatted_values <- apply(data_top20, c(1, 2), function(x) {
  ifelse(is.na(x), "NA", paste0(formatC(x, format = "f", digits = 1), "%"))
})

# Paleta y cortes 0-100
breaks_manual <- seq(0, 100, by = 1)
color_palette <- colorRampPalette(c("white", "lightblue", "blue", "darkblue"))(length(breaks_manual) - 1)

# Filas en cursiva
italic_labels <- parse(text = paste0("italic('", rownames(data_top20), "')"))

# ---- Export PDF ----
while (!is.null(dev.list())) dev.off()
CairoPDF("output/Figure_6.pdf", width = 12, height = 10, family = "Times New Roman")

pheatmap(
  data_top20,
  color = color_palette,
  cluster_rows = hclust_rows,
  cluster_cols = FALSE,
  scale = "none",
  fontsize = 12,
  labels_row = italic_labels,
  fontsize_row = 12,
  fontsize_col = 12,
  angle_col = 45,
  breaks = breaks_manual,
  legend_breaks = c(0, 25, 50, 75, 100),
  legend_labels = c("0%", "25%", "50%", "75%", "100%"),
  border_color = NA,
  cellwidth = 50,
  cellheight = 22,
  display_numbers = formatted_values,
  number_color = "black",
  na_col = "white",
  drop_levels = FALSE
)

dev.off()

# ---- Export TIFF (600 dpi) ----
tiff("output/Figure_6.tiff", width = 12, height = 10, units = "in", res = 600, compression = "lzw")

pheatmap(
  data_top20,
  color = color_palette,
  cluster_rows = hclust_rows,
  cluster_cols = FALSE,
  scale = "none",
  fontsize = 12,
  labels_row = italic_labels,
  fontsize_row = 12,
  fontsize_col = 12,
  angle_col = 45,
  breaks = breaks_manual,
  legend_breaks = c(0, 25, 50, 75, 100),
  legend_labels = c("0%", "25%", "50%", "75%", "100%"),
  border_color = NA,
  cellwidth = 50,
  cellheight = 22,
  display_numbers = formatted_values,
  number_color = "black",
  na_col = "white",
  drop_levels = FALSE
)

dev.off()
