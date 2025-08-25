# ---- Packages ----
library(vegan)
library(ggplot2)
library(Cairo)

# ---- Read data (expected: data/Figure_8.csv) ----
# Caso típico que comentaste: primera columna = Sample_ID, resto = abundancias
dat <- read.csv("data/Figure_8.csv", sep = ",", stringsAsFactors = FALSE, check.names = FALSE)

# Asegurar que la primera columna es el identificador de muestra
# Admite nombres "Sample_ID" o "Sample"
id_col <- intersect(names(dat), c("Sample_ID","Sample"))
if (length(id_col) == 0) stop("No encuentro columna de identificador (Sample_ID o Sample).")

# Convertir a numérico todas las columnas de abundancia
abund <- dat[, setdiff(names(dat), id_col), drop = FALSE]
abund[] <- lapply(abund, function(x) as.numeric(gsub(",", ".", as.character(x))))
# Quitar columnas completamente NA (por si hay metadatos mezclados)
abund <- abund[, colSums(!is.na(abund)) > 0, drop = FALSE]

# Filtrar filas con todo 0 o NA (NMDS no puede con vectores nulos)
row_keep <- rowSums(abund, na.rm = TRUE) > 0
abund <- abund[row_keep, , drop = FALSE]
samples <- dat[row_keep, id_col][[1]]

# Matriz y nombres de fila
abund_matrix <- as.matrix(abund)
rownames(abund_matrix) <- samples

# ---- Metadata (Treatment / Organ) ----
# Si no vienen en columnas separadas, las extraemos de Sample_ID separando por "_"
# Esperado: algo como "R1_Conventional_Leaf" → Replicate, Treatment, Organ
Treatment <- Organ <- NULL
if (!all(c("Treatment","Organ") %in% names(dat))) {
  parts <- strsplit(samples, "_")
  # Por seguridad: si hay menos de 3 partes, rellena con NA
  parts_pad <- lapply(parts, function(p) { length(p) <- 3; p })
  meta <- do.call(rbind, parts_pad)
  colnames(meta) <- c("Replicate","Treatment","Organ")
  Treatment <- meta[, "Treatment"]
  Organ     <- meta[, "Organ"]
} else {
  Treatment <- dat[row_keep, "Treatment"]
  Organ     <- dat[row_keep, "Organ"]
}

Treatment <- factor(Treatment)
Organ     <- factor(Organ, levels = c("Soil","Leaf","Grape","Bark"))

# ---- NMDS (Bray-Curtis) ----
set.seed(123)
nmds <- metaMDS(abund_matrix, distance = "bray", k = 2, trymax = 100, autotransform = FALSE)

nmds_df <- data.frame(
  Sample = samples,
  NMDS1 = nmds$points[,1],
  NMDS2 = nmds$points[,2],
  Treatment = Treatment,
  Organ = Organ
)

# ---- Plot ----
ttl <- sprintf("NMDS – Bacterial communities (stress = %.3f)", nmds$stress)

p <- ggplot(nmds_df, aes(NMDS1, NMDS2, color = Organ, shape = Treatment)) +
  geom_point(size = 3) +
  theme_minimal(base_family = "Times New Roman") +
  labs(title = ttl, x = "NMDS1", y = "NMDS2", color = "Organ", shape = "Treatment") +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "right")

# ---- Export ----
CairoPDF("output/Figure_8.pdf", family = "Times New Roman", width = 6, height = 6)
print(p); dev.off()

tiff("output/Figure_8.tiff", width = 6, height = 6, res = 600, units = "in", type = "cairo", compression = "lzw")
print(p); dev.off()
