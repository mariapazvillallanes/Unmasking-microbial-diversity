# ============================================================
# FIGURA COMPUESTA a) b) c) d) — mismo estilo que ejemplo
# ============================================================

# ---- Paquetes
library(tidyverse)
library(vegan)
library(ggsignif)
library(RColorBrewer)
library(cowplot)
library(png)
library(grid)
library(ggtext)

# ---- Parámetros generales
panel_width_in  <- 6
panel_height_in <- 5
dpi_out         <- 600
gutter_pts      <- 2
base_family     <- "Times New Roman"

organ_levels <- c("Bark","Grape","Leaf","Soil")
organ_cols <- c(Bark="#66C2A5", Grape="#FC8D62", Leaf="#8DA0CB", Soil="#E78AC3")

strip_grid <- function(p) {
  p + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", colour = NA))
}
fmt_p <- function(p) {
  if (is.na(p) || p > 0.05) "NS"
  else if (p < 0.001) "p < 0.001"
  else paste0("p = ", format(round(p, 3), nsmall = 3, trim = TRUE))
}

# ============================================================
# ===== Panel a) Shannon por Treatment (Figure_7a.csv)
# ============================================================
raw_data_a <- read.csv("data/Figure_7a.csv", sep = ";", dec = ",", check.names = FALSE)
names(raw_data_a) <- gsub("\\s+", ".", trimws(names(raw_data_a)))

if (!"Relative.Abundance" %in% names(raw_data_a)) {
  raw_data_a$Abundance <- as.numeric(gsub(",", ".", raw_data_a$Abundance))
  raw_data_a <- raw_data_a %>%
    group_by(Sample) %>%
    mutate(Relative.Abundance = Abundance/sum(Abundance, na.rm = TRUE)) %>%
    ungroup()
}

shannon_a <- raw_data_a %>%
  group_by(Sample, Treatment) %>%
  summarise(Shannon = diversity(Relative.Abundance, "shannon"), .groups="drop")

anova_a <- aov(Shannon ~ Treatment, data = shannon_a)
tukey_a <- TukeyHSD(anova_a)$`Treatment`
comparisons_a <- list(c("Organic","Conventional"), c("Organic","Wild"), c("Conventional","Wild"))
p_labels_a <- sapply(tukey_a[,4], fmt_p)

p_a <- ggplot(shannon_a, aes(Treatment, Shannon, fill = Treatment)) +
  geom_boxplot(width=0.55, outlier.size=1.8, fatten=2) +
  scale_fill_manual(values = c(Organic="#1b9e77",Conventional="#d95f02",Wild="#7570b3")) +
  ggsignif::geom_signif(comparisons = comparisons_a, annotations = p_labels_a,
                        map_signif_level = FALSE, step_increase = 0.35,
                        tip_length=0.01, textsize=3, vjust=-0.2) +
  theme_minimal(base_family = base_family) +
  labs(x="Treatment", y="Shannon Diversity Index") +
  theme(legend.position="bottom")
panel_a <- strip_grid(p_a)

# ============================================================
# ===== Panel b) Shannon por órgano (Figure_7b.csv)
# ============================================================
raw_data_b <- read.csv("data/Figure_7b.csv", sep = ";", dec = ",", check.names = FALSE)
raw_data_b$Abundance <- as.numeric(gsub(",", ".", raw_data_b$Abundance))

df_b <- raw_data_b %>%
  group_by(Sample, Organ.Material, Genera) %>%
  summarise(Abundance=sum(Abundance, na.rm=TRUE), .groups="drop") %>%
  group_by(Sample, Organ.Material) %>%
  mutate(Rel_Abundance=Abundance/sum(Abundance, na.rm=TRUE)) %>%
  ungroup()

shannon_b <- df_b %>%
  group_by(Sample, Organ.Material) %>%
  summarise(Shannon = diversity(Rel_Abundance, "shannon"), .groups="drop")

anova_b <- aov(Shannon ~ Organ.Material, data = shannon_b)
tukey_b <- TukeyHSD(anova_b)$`Organ.Material`
comparisons_b <- strsplit(rownames(tukey_b), "-")
p_labels_b <- sapply(tukey_b[,4], fmt_p)

p_b <- ggplot(shannon_b, aes(Organ.Material, Shannon, fill = Organ.Material)) +
  geom_boxplot(width=0.55, outlier.size=1.8) +
  scale_fill_manual(values=organ_cols) +
  ggsignif::geom_signif(comparisons=comparisons_b, annotations=p_labels_b,
                        map_signif_level=FALSE, step_increase=0.12,
                        tip_length=0.01, textsize=2.7, vjust=-0.2) +
  theme_minimal(base_family = base_family) +
  labs(x="Plant Organ", y="Shannon Diversity Index") +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.position="bottom")
panel_b <- strip_grid(p_b)

# ============================================================
# ===== Panel c) NMDS (Figure_7c.csv)
# ============================================================
raw_data_c <- read.csv("data/Figure_7c.csv", sep = ",", check.names = FALSE)
abund_mat <- as.matrix(raw_data_c[, -1])
rownames(abund_mat) <- raw_data_c$Sample_ID
set.seed(123)
nmds <- metaMDS(abund_mat, distance="bray", k=2, trymax=100)

metadata <- data.frame(do.call(rbind, strsplit(raw_data_c$Sample_ID, "_")))
colnames(metadata) <- c("Rep","Treatment","Organ")

df_c <- data.frame(Sample=raw_data_c$Sample_ID,
                   NMDS1=nmds$points[,1], NMDS2=nmds$points[,2],
                   Treatment=metadata$Treatment,
                   Organ=factor(metadata$Organ, levels=organ_levels))

p_c <- ggplot(df_c, aes(NMDS1, NMDS2, color=Organ, shape=Treatment)) +
  geom_point(size=3) +
  scale_color_manual(values=organ_cols) +
  theme_minimal(base_family = base_family) +
  labs(x="NMDS1", y="NMDS2") +
  theme(legend.position="bottom")
panel_c <- strip_grid(p_c)

# ============================================================
# ===== Panel d) Barras apiladas (Figure_7d.csv)
# ============================================================
raw_data_d <- read.csv("data/Figure_7d.csv", sep = ";", dec = ",", check.names = FALSE)
raw_data_d$Abundance <- as.numeric(gsub(",", ".", raw_data_d$Abundance))

df_d <- raw_data_d %>%
  group_by(System, Medium, Ecosystem, Class) %>%
  summarise(Abundance=sum(Abundance, na.rm=TRUE), .groups="drop") %>%
  group_by(System, Medium, Ecosystem) %>%
  mutate(Rel_Abundance=100*Abundance/sum(Abundance, na.rm=TRUE)) %>%
  ungroup()

p_d <- ggplot(df_d, aes(System, Rel_Abundance, fill=Class)) +
  geom_bar(stat="identity", position="stack", width=0.25, color="white", linewidth=0.2) +
  facet_grid(Ecosystem ~ Medium) +
  theme_minimal(base_family = base_family) +
  labs(y="Relative Abundance (%)", x=NULL, fill="Bacterial Class") +
  theme(axis.text.x=ggtext::element_markdown(angle=45,hjust=1),
        legend.position="right")
panel_d <- strip_grid(p_d)

# ============================================================
# Exportar paneles individuales a TIFF
# ============================================================
tiff("output/panel_a.tiff", width=panel_width_in, height=panel_height_in,
     units="in", res=dpi_out, compression="lzw")
print(panel_a); dev.off()

tiff("output/panel_b.tiff", width=panel_width_in, height=panel_height_in,
     units="in", res=dpi_out, compression="lzw")
print(panel_b); dev.off()

tiff("output/panel_c.tiff", width=panel_width_in, height=panel_height_in,
     units="in", res=dpi_out, compression="lzw")
print(panel_c); dev.off()

tiff("output/panel_d.tiff", width=panel_width_in, height=panel_height_in,
     units="in", res=dpi_out, compression="lzw")
print(panel_d); dev.off()

# ============================================================
# Figura compuesta final a 12x10 in
# ============================================================
fila1 <- plot_grid(panel_a, panel_b, ncol=2, labels=c("a)","b)"),
                   label_size=14, label_fontface="bold",
                   label_x=0.02, label_y=0.98, hjust=0, vjust=1)
fila2 <- plot_grid(panel_c, panel_d, ncol=2, labels=c("c)","d)"),
                   label_size=14, label_fontface="bold",
                   label_x=0.02, label_y=0.98, hjust=0, vjust=1)
fig_final <- plot_grid(fila1, fila2, ncol=1)

tiff("output/Figure_7.tiff", width=12, height=10,
     units="in", res=dpi_out, compression="lzw")
print(fig_final); dev.off()

pdf("output/Figure_7.pdf", width=12, height=10)
print(fig_final); dev.off()
