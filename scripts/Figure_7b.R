# ---- Paquetes ----
library(tidyverse)
library(vegan)
library(ggsignif)
library(Cairo)

# ---- Leer datos ----
# CSV esperado (según tu fichero real): Sample, Treatment, Organ Material, Genera, Abundance
datos <- read.csv("data/Figure_7.csv", sep = ";", stringsAsFactors = FALSE)

# Abundance son recuentos (no aplicar coma->punto para no corromper enteros)
datos$Abundance <- as.numeric(datos$Abundance)
datos$Genera <- trimws(datos$Genera)

# Asegura orden consistente de órganos en el eje X (ajusta si usas otro orden en el paper)
organ_levels <- c("Soil","Leaf","Grape","Bark")
if ("Organ.Material" %in% names(datos)) {
  datos$Organ.Material <- factor(datos$Organ.Material, levels = organ_levels)
} else if ("Organ Material" %in% names(datos)) {
  # Por si la columna viene con espacio
  datos$`Organ Material` <- factor(datos$`Organ Material`, levels = organ_levels)
  names(datos)[names(datos) == "Organ Material"] <- "Organ.Material"
}

# ---- Abundancia relativa por muestra y órgano ----
df_relative <- datos %>%
  group_by(Sample, Organ.Material, Genera) %>%
  summarise(Abundance = sum(Abundance), .groups = "drop") %>%
  group_by(Sample, Organ.Material) %>%
  mutate(Rel_Abundance = Abundance / sum(Abundance)) %>%
  ungroup()

# ---- Shannon por muestra y órgano ----
shannon_by_organ <- df_relative %>%
  group_by(Sample, Organ.Material) %>%
  summarise(Shannon = diversity(Rel_Abundance, index = "shannon"),
            .groups = "drop")

# ---- ANOVA + Tukey entre órganos ----
anova_organ <- aov(Shannon ~ Organ.Material, data = shannon_by_organ)
tukey_tab <- TukeyHSD(anova_organ)$`Organ.Material`

# Comparaciones presentes (geom_signif requiere pares del eje X)
comp_labels <- rownames(tukey_tab)                   # p.ej. "Leaf-Grape"
comparisons_list <- strsplit(comp_labels, "-")       # list(c("Leaf","Grape"), ...)
p_vals <- tukey_tab[, "p adj"]

p_lab <- sapply(p_vals, function(p){
  if (is.na(p)) "NA" else if (p > 0.05) "NS" else if (p < 0.001) "p < 0.001" else paste("p =", format(p, digits = 3))
})

# Posiciones verticales automáticas
y_max <- max(shannon_by_organ$Shannon, na.rm = TRUE)
y_pos <- y_max + seq(0.05, 0.15, length.out = length(comparisons_list))

# ---- Gráfico ----
p <- ggplot(shannon_by_organ, aes(x = Organ.Material, y = Shannon, fill = Organ.Material)) +
  geom_boxplot(width = 0.9) +
  theme_minimal(base_family = "Times New Roman", base_size = 13) +
  labs(x = "Plant Organ", y = "Shannon Diversity Index", fill = "Organs") +
  scale_fill_brewer(palette = "Set2", drop = FALSE) +
  geom_signif(comparisons = comparisons_list,
              annotations = p_lab,
              map_signif_level = FALSE,
              y_position = y_pos,
              tip_length = 0.01,
              textsize = 3, vjust = 0.2) +
  annotate("text", x = 0.55, y = y_max + 0.25, label = "b)",
           size = 5, family = "Times New Roman") +
  coord_cartesian(ylim = c(0, y_max + 0.35)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        text = element_text(family = "Times New Roman"),
        axis.title.y = element_text(margin = margin(r = 8)))

# ---- Exportar ----
CairoPDF("output/Figure_7b.pdf", width = 6, height = 5, family = "Times New Roman")
print(p); dev.off()

tiff("output/Figure_7b.tiff", units = "in", width = 6, height = 5,
     res = 600, compression = "lzw", type = "cairo")
print(p); dev.off()
