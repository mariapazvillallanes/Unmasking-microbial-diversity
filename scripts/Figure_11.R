library(vegan)
library(ggplot2)
library(Cairo)

# ---- Read data (expected: data/Figure_11.csv) ----
# Según tu descripción: columnas: Sample, Treatment, OrganMaterial, abundancias desde la col 4
dat <- read.csv("data/Figure_11.csv", sep = ",", stringsAsFactors = FALSE, check.names = FALSE)

need <- c("Sample","Treatment","OrganMaterial")
if (!all(need %in% names(dat))) {
  stop("Se esperaban columnas: Sample, Treatment, OrganMaterial en Figure_11.csv")
}

# Convertir columnas de abundancia a numérico
abund <- dat[, setdiff(names(dat), need), drop = FALSE]
abund[] <- lapply(abund, function(x) as.numeric(gsub(",", ".", as.character(x))))
abund <- abund[, colSums(!is.na(abund)) > 0, drop = FALSE]

# Filtrar filas con todo 0 o NA
row_keep <- rowSums(abund, na.rm = TRUE) > 0
abund <- abund[row_keep, , drop = FALSE]
meta  <- dat[row_keep, need, drop = FALSE]

abund_matrix <- as.matrix(abund)
rownames(abund_matrix) <- meta$Sample

meta$Treatment     <- factor(meta$Treatment)
meta$OrganMaterial <- factor(meta$OrganMaterial, levels = c("Soil","Leaf","Grape","Bark"))

# ---- NMDS ----
set.seed(123)
nmds <- metaMDS(abund_matrix, distance = "bray", k = 2, trymax = 100, autotransform = FALSE)

nmds_df <- data.frame(
  Sample = meta$Sample,
  Treatment = meta$Treatment,
  OrganMaterial = meta$OrganMaterial,
  NMDS1 = nmds$points[,1],
  NMDS2 = nmds$points[,2]
)

ttl <- sprintf("NMDS – Fungal communities (stress = %.3f)", nmds$stress)

p <- ggplot(nmds_df, aes(NMDS1, NMDS2, color = OrganMaterial, shape = Treatment)) +
  geom_point(size = 3) +
  theme_minimal(base_family = "Times New Roman") +
  labs(title = ttl, x = "NMDS1", y = "NMDS2", color = "Organ", shape = "Treatment") +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "right")

# ---- Export ----
CairoPDF("output/Figure_11.pdf", family = "Times New Roman", width = 6, height = 6)
print(p); dev.off()

tiff("output/Figure_11.tiff", width = 6, height = 6, res = 600, units = "in", type = "cairo", compression = "lzw")
print(p); dev.off()
