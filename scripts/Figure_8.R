# ============================================================
# FIGURA COMPUESTA a) b) c) d) — estilo "data/" → "output/"
# ============================================================

# ---- Paquetes
library(tidyverse)
library(vegan)
library(ggsignif)
library(RColorBrewer)
library(cowplot)   # montaje final
library(png)
library(grid)
library(ggtext)

# ---- Parámetros generales
panel_width_in  <- 6     # tamaño de CADA panel al exportar
panel_height_in <- 5
dpi_out         <- 600
gutter_pts      <- 2     # margen interior en cada “baldosa” del montaje
base_family     <- "Times New Roman"

# ---- Colores consistentes para ÓRGANOS (b y c)
organ_levels <- c("Bark","Grape","Leaf","Soil")
organ_cols <- c(
  Bark  = "#66C2A5",  # Set2-1
  Grape = "#FC8D62",  # Set2-2
  Leaf  = "#8DA0CB",  # Set2-3
  Soil  = "#E78AC3"   # Set2-4
)

# ---- Utilidades
strip_grid <- function(p) {
  p +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}
fmt_p <- function(p) {
  if (is.na(p) || p > 0.05) "NS"
  else if (p < 0.001) "p < 0.001"
  else paste0("p = ", format(round(p, 3), nsmall = 3, trim = TRUE))
}

# ============================================================
# ===== Panel a) (Shannon por Treatment) — data/Figure_8a.csv
# ============================================================
datos_a <- read.csv("data/Figure_8a-b.csv", sep = ";", dec = ",", stringsAsFactors = FALSE, check.names = FALSE)
names(datos_a) <- gsub("\\s+", ".", trimws(names(datos_a)))
if ("Genera" %in% names(datos_a)) datos_a$Genera <- trimws(datos_a$Genera)
if ("Treatment" %in% names(datos_a)) {
  datos_a$Treatment <- factor(trimws(datos_a$Treatment),
                              levels = c("Organic", "Conventional", "Wild"))
}
if (!"Relative.Abundance" %in% names(datos_a)) {
  stopifnot(all(c("Sample","Abundance") %in% names(datos_a)))
  datos_a$Abundance <- suppressWarnings(as.numeric(gsub(",", ".", datos_a$Abundance)))
  datos_a <- datos_a %>%
    group_by(Sample) %>%
    mutate(Relative.Abundance = Abundance / sum(Abundance, na.rm = TRUE)) %>%
    ungroup()
}
shannon_a <- datos_a %>%
  group_by(Sample, Treatment) %>%
  summarise(Shannon = diversity(Relative.Abundance, index = "shannon"), .groups = "drop")
anova_a <- aov(Shannon ~ Treatment, data = shannon_a)
tukey_a <- TukeyHSD(anova_a)$`Treatment`
comparisons_a <- list(c("Organic","Conventional"), c("Organic","Wild"), c("Conventional","Wild"))
p_labels_a <- sapply(tukey_a[,4], fmt_p)
max_y_a <- max(shannon_a$Shannon, na.rm = TRUE)
min_y_a <- min(shannon_a$Shannon, na.rm = TRUE)

p_a <- ggplot(shannon_a, aes(Treatment, Shannon, fill = Treatment)) +
  geom_boxplot(width = 0.55, outlier.size = 1.8, fatten = 2) +
  theme_minimal(base_family = base_family) +
  labs(x = "Treatment", y = "Shannon Diversity Index", fill = NULL) +
  scale_fill_manual(values = c(Organic="#1b9e77", Conventional="#d95f02", Wild="#7570b3")) +
  ggsignif::geom_signif(
    comparisons = comparisons_a,
    annotations = p_labels_a,
    map_signif_level = FALSE,
    step_increase = 0.35,
    tip_length   = 0.01,
    textsize = 3, vjust = -0.2
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.5, 0.5))) +
  coord_cartesian(ylim = c(min_y_a - 0.05, max_y_a + 0.95), clip = "off") +
  theme(
    legend.position = "bottom",
    legend.text  = element_text(size = 9),
    plot.margin  = margin(5, 5, 5, 5)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = NULL))
panel_a <- strip_grid(p_a)

# ============================================================
# ===== Panel b) (Shannon por órgano) — data/Figure_8b.csv
# ============================================================
datos_b <- read.csv("data/Figure_8a-b.csv", sep = ";", dec = ",", stringsAsFactors = FALSE, check.names = FALSE)
names(datos_b) <- gsub("\\s+", ".", trimws(names(datos_b)))
if ("Genera" %in% names(datos_b)) datos_b$Genera <- trimws(datos_b$Genera)
datos_b$Relative.Abundance <- suppressWarnings(as.numeric(gsub(",", ".", datos_b$Relative.Abundance)))

df_relative_b <- datos_b %>%
  group_by(Sample, Organ.Material, Genera) %>%
  summarise(Abundance = sum(Relative.Abundance, na.rm = TRUE), .groups = "drop") %>%
  group_by(Sample, Organ.Material) %>%
  mutate(Rel_Abundance = Abundance / sum(Abundance, na.rm = TRUE)) %>%
  ungroup()

shannon_b <- df_relative_b %>%
  group_by(Sample, Organ.Material) %>%
  summarise(Shannon = diversity(Rel_Abundance, index = "shannon"), .groups = "drop")

anova_b <- aov(Shannon ~ Organ.Material, data = shannon_b)
tukey_b <- TukeyHSD(anova_b)$`Organ.Material`
comparisons_list_b <- strsplit(rownames(tukey_b), "-")
p_labels_b <- sapply(tukey_b[,4], fmt_p)
max_y_b <- max(shannon_b$Shannon, na.rm = TRUE)
min_y_b <- min(shannon_b$Shannon, na.rm = TRUE)

shannon_b$Organ.Material <- factor(shannon_b$Organ.Material, levels = organ_levels)

p_b <- ggplot(shannon_b, aes(Organ.Material, Shannon, fill = Organ.Material)) +
  geom_boxplot(width = 0.55, outlier.size = 1.8) +
  theme_minimal(base_family = base_family) +
  labs(x = "Plant Organ", y = "Shannon Diversity Index", fill = NULL) +
  scale_fill_manual(values = organ_cols, drop = FALSE) +
  ggsignif::geom_signif(
    comparisons = comparisons_list_b,
    annotations = p_labels_b,
    map_signif_level = FALSE,
    step_increase = 0.12,
    tip_length = 0.01,
    textsize = 2.7, vjust = -0.2
  ) +
  coord_cartesian(ylim = c(min_y_b - 0.10, max_y_b + 0.60)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text  = element_text(size = 9),
    legend.title = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = NULL))
panel_b <- strip_grid(p_b)

# ============================================================
# ===== Panel c) (NMDS) — data/Figure_8c.csv
# ============================================================
abund_c <- read.csv("data/Figure_8c.csv", sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
abund_mat <- as.matrix(abund_c[, -1])
rownames(abund_mat) <- abund_c$Sample_ID
set.seed(123)
nmds_c <- metaMDS(abund_mat, distance = "bray", k = 2, trymax = 100)

metadata_c <- data.frame(do.call(rbind, strsplit(abund_c$Sample_ID, "_")))
colnames(metadata_c) <- c("Replicate","Treatment","Organ")

nmds_df_c <- data.frame(
  Sample = abund_c$Sample_ID,
  NMDS1 = nmds_c$points[,1],
  NMDS2 = nmds_c$points[,2],
  Treatment = metadata_c$Treatment,
  Organ = factor(metadata_c$Organ, levels = organ_levels)
)

p_c <- ggplot(nmds_df_c, aes(NMDS1, NMDS2, color = Organ, shape = Treatment)) +
  geom_point(size = 3) +
  theme_minimal(base_family = base_family) +
  labs(x = "NMDS1", y = "NMDS2", color = NULL, shape = NULL) +
  scale_color_manual(values = organ_cols, drop = FALSE) +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = 8, family = base_family),
    legend.title      = element_blank(),
    legend.key.height = unit(3, "mm"),
    legend.spacing.x  = unit(2, "mm"),
    legend.margin     = margin(0, 0, 0, 0),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 3)),
    shape = guide_legend(nrow = 1, byrow = TRUE)
  )
panel_c <- strip_grid(p_c)

# ============================================================
# ===== Panel d) (Barras apiladas) — data/Figure_8d.csv
# ============================================================
df_d <- read.csv("data/Figure_8d.csv", sep = ";", dec = ",", check.names = FALSE)
names(df_d) <- trimws(names(df_d))
df_d$Abundance <- suppressWarnings(as.numeric(gsub(",", ".", df_d$Abundance)))

df_d <- df_d %>%
  mutate(
    System = case_when(
      grepl("Domesticated", Treatment, ignore.case = TRUE) ~ "V. vinifera subsp. vinifera",
      grepl("Wild",         Treatment, ignore.case = TRUE) ~ "V. vinifera subsp. sylvestris",
      TRUE ~ NA_character_
    ),
    Medium = case_when(
      grepl("Commercial", Treatment, ignore.case = TRUE) ~ "Commercial Medium",
      grepl("Natural",    Treatment, ignore.case = TRUE) ~ "Natural Medium",
      TRUE ~ NA_character_
    ),
    Ecosystem = recode(Ecosystem, "Epi" = "Epiphytic", "End" = "Endophytic")
  ) %>%
  group_by(System, Medium, Ecosystem, Class) %>%
  summarise(Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
  group_by(System, Medium, Ecosystem) %>%
  mutate(Relative_Abundance = 100 * Abundance / sum(Abundance, na.rm = TRUE)) %>%
  ungroup()

ordered_classes_d <- sort(unique(df_d$Class))
df_d$Class <- factor(df_d$Class, levels = ordered_classes_d)

cols_d_pastel_A <- c(
  "#5A9BD4", "#E2C65C", "#A5B843", "#D3B289",
  "#C77F6C", "#8EA2B7", "#85B889", "#A391CF"
)
palette_d <- setNames(cols_d_pastel_A[seq_len(length(ordered_classes_d))], ordered_classes_d)

df_d$System <- factor(df_d$System,
                      levels = c("V. vinifera subsp. vinifera", "V. vinifera subsp. sylvestris"))
system_labels_d <- c(
  "V. vinifera subsp. vinifera"   = "V. *vinifera*",
  "V. vinifera subsp. sylvestris" = "V. *sylvestris*"
)

p_d <- ggplot(df_d, aes(System, Relative_Abundance, fill = Class)) +
  geom_bar(stat = "identity", position = "stack", width = 0.25, color = "white", linewidth = 0.2) +
  geom_text(aes(label = paste0(round(Relative_Abundance, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 2.3, family = base_family) +
  facet_grid(Ecosystem ~ Medium) +
  scale_fill_manual(values = palette_d, drop = FALSE) +
  scale_x_discrete(labels = system_labels_d) +
  labs(x = NULL, y = "Relative Abundance (%)", fill = "Fungal Class") +
  theme_minimal(base_family = base_family, base_size = 12) +
  theme(
    strip.text.x = element_text(face = "bold", size = 10, family = base_family),
    strip.text.y = element_text(face = "bold", size = 9,  family = base_family),
    strip.background = element_rect(fill = "grey90", color = NA),
    axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1),
    legend.position = "right",
    legend.text  = element_text(size = 9, family = base_family),
    legend.title = element_text(size = 9, family = base_family),
    plot.margin = margin(5, 18, 5, 5)
  )
panel_d <- strip_grid(p_d)

# ============================================================
# 2) Exportar CADA panel a TIFF (sin letras) — output/
# ============================================================
tiff("output/panel_a.tiff", width = panel_width_in, height = panel_height_in,
     units = "in", res = dpi_out, compression = "lzw")
print(panel_a); dev.off()

tiff("output/panel_b.tiff", width = panel_width_in, height = panel_height_in,
     units = "in", res = dpi_out, compression = "lzw")
print(panel_b); dev.off()

tiff("output/panel_c.tiff", width = panel_width_in, height = panel_height_in,
     units = "in", res = dpi_out, compression = "lzw")
print(panel_c); dev.off()

tiff("output/panel_d.tiff", width = panel_width_in, height = panel_height_in,
     units = "in", res = dpi_out, compression = "lzw")
print(panel_d); dev.off()

# ============================================================
# 3) Obtener PNGs en output/ (convierte TIFF→PNG si hay 'tiff'; si no, exporta PNG)
# ============================================================
has_tiff <- requireNamespace("tiff", quietly = TRUE)
if (has_tiff) {
  convert_tiff_to_png <- function(input, output) {
    arr <- tiff::readTIFF(input, native = FALSE)
    png::writePNG(arr, target = output)
  }
  convert_tiff_to_png("output/panel_a.tiff", "output/panel_a.png")
  convert_tiff_to_png("output/panel_b.tiff", "output/panel_b.png")
  convert_tiff_to_png("output/panel_c.tiff", "output/panel_c.png")
  convert_tiff_to_png("output/panel_d.tiff", "output/panel_d.png")
} else {
  png("output/panel_a.png", width = panel_width_in, height = panel_height_in, units = "in", res = dpi_out)
  print(panel_a); dev.off()
  png("output/panel_b.png", width = panel_width_in, height = panel_height_in, units = "in", res = dpi_out)
  print(panel_b); dev.off()
  png("output/panel_c.png", width = panel_width_in, height = panel_height_in, units = "in", res = dpi_out)
  print(panel_c); dev.off()
  png("output/panel_d.png", width = panel_width_in, height = panel_height_in, units = "in", res = dpi_out)
  print(panel_d); dev.off()
}

# ============================================================
# 4) Montar las 4 PNG en una figura sin deformar + letras a)–d) — output/
# ============================================================
img_tile_png <- function(path, margin_pts = 2) {
  stopifnot(file.exists(path))
  arr <- png::readPNG(path)
  ar  <- dim(arr)[1] / dim(arr)[2]   # alto/ancho
  gro <- rasterGrob(arr, interpolate = TRUE)
  ggdraw() +
    draw_grob(gro) +
    theme_void() +
    theme(
      plot.margin  = margin(margin_pts, margin_pts, margin_pts, margin_pts),
      aspect.ratio = ar
    )
}

img_a <- img_tile_png("output/panel_a.png", gutter_pts)
img_b <- img_tile_png("output/panel_b.png", gutter_pts)
img_c <- img_tile_png("output/panel_c.png", gutter_pts)
img_d <- img_tile_png("output/panel_d.png", gutter_pts)

fila1 <- plot_grid(img_a, img_b, ncol = 2,
                   labels = c("a)", "b)"),
                   label_size = 14, label_fontface = "bold",
                   label_x = 0.02, label_y = 0.98, hjust = 0, vjust = 1)
fila2 <- plot_grid(img_c, img_d, ncol = 2,
                   labels = c("c)", "d)"),
                   label_size = 14, label_fontface = "bold",
                   label_x = 0.02, label_y = 0.98, hjust = 0, vjust = 1)

fig_final <- plot_grid(fila1, fila2, ncol = 1, rel_heights = c(1, 1))

# Tamaño final coherente con 2×2 de 6×5 in → 12×10 in
final_width_in  <- panel_width_in  * 2
final_height_in <- panel_height_in * 2

tiff("output/Figure_8.tiff",
     width = final_width_in, height = final_height_in,
     units = "in", res = dpi_out, compression = "lzw")
print(fig_final); dev.off()

pdf("output/Figure_8.pdf",
    width = final_width_in, height = final_height_in)
print(fig_final); dev.off()
