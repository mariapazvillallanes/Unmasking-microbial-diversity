library(tidyverse)
library(vegan)
library(ggsignif)
library(Cairo)

# ---- Leer datos ----
# CSV real: Sample, Treatment, Organ Material, Genera, "Relative Abundance" (con espacio y % usando coma)
datos <- read.csv("data/Figure_10.csv", sep = ";", stringsAsFactors = FALSE)

# Normaliza nombre con espacio -> columna usable:
if ("Relative Abundance" %in% names(datos)) {
  datos$Relative.Abundance <- as.numeric(gsub(",", ".", datos[["Relative Abundance"]]))
} else {
  stop("No encuentro la columna 'Relative Abundance' en Figure_10.csv")
}
datos$Genera <- trimws(datos$Genera)

# Factor de órgano con orden consistente
organ_levels <- c("Soil","Leaf","Grape","Bark")
if ("Organ.Material" %in% names(datos)) {
  datos$Organ.Material <- factor(datos$Organ.Material, levels = organ_levels)
} else if ("Organ Material" %in% names(datos)) {
  datos$`Organ Material` <- factor(datos$`Organ Material`, levels = organ_levels)
  names(datos)[names(datos) == "Organ Material"] <- "Organ.Material"
}

# ---- Abundancia relativa por muestra y órgano ----
# Tus valores vienen en %, sumamos por muestra/órgano y luego normalizamos a proporciones 0–1
df_relative <- datos %>%
  group_by(Sample, Organ.Material, Genera) %>%
  summarise(AbPerc = sum(Relative.Abundance, na.rm = TRUE), .groups = "drop") %>%
  group_by(Sample, Organ.Material) %>%
  mutate(Rel_Abundance = (AbPerc / sum(AbPerc, na.rm = TRUE))) %>%  # ya es proporción 0–1
  ungroup()

# ---- Shannon por muestra y órgano ----
shannon_by_organ <- df_relative %>%
  group_by(Sample, Organ.Material) %>%
  summarise(Shannon = diversity(Rel_Abundance, index = "shannon"),
            .groups = "drop")

# ---- ANOVA + Tukey ----
anova_res <- aov(Shannon ~ Organ.Material, data = shannon_by_organ)
tukey_tab <- TukeyHSD(anova_res)$`Organ.Material`

comp_labels <- rownames(tukey_tab)
comparisons <- strsplit(comp_labels, "-")
p_vals <- tukey_tab[, "p adj"]

p_lab <- sapply(p_vals, function(p){
  if (is.na(p)) "NA" else if (p > 0.05) "NS" else if (p < 0.001) "p < 0.001" else paste("p =", format(p, digits = 3))
})

y_max <- max(shannon_by_organ$Shannon, na.rm = TRUE)
y_pos <- y_max + seq(0.05, 0.15, length.out = length(comparisons))

p <- ggplot(shannon_by_organ, aes(x = Organ.Material, y = Shannon, fill = Organ.Material)) +
  geom_boxplot(width = 0.9) +
  theme_minimal(base_family = "Times New Roman", base_size = 13) +
  labs(x = "Plant Organ", y = "Shannon Diversity Index", fill = "Organs") +
  scale_fill_brewer(palette = "Set2", drop = FALSE) +
  geom_signif(comparisons = comparisons,
              annotations = p_lab,
              map_signif_level = FALSE,
              y_position = y_pos, tip_length = 0.01,
              textsize = 3, vjust = 0.2) +
  annotate("text", x = 0.55, y = y_max + 0.25, label = "b)",
           size = 5, family = "Times New Roman") +
  coord_cartesian(ylim = c(0, y_max + 0.35)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        text = element_text(family = "Times New Roman"),
        axis.title.y = element_text(margin = margin(r = 8)))

CairoPDF("output/Figure_10b.pdf", width = 6, height = 5, family = "Times New Roman")
print(p); dev.off()

tiff("output/Figure_10b.tiff", units = "in", width = 6, height = 5,
     res = 600, compression = "lzw", type = "cairo")
print(p); dev.off()
