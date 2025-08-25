# ---- Packages ----
library(tidyverse)
library(vegan)
library(ggsignif)
library(Cairo)

# ---- Read data ----
df <- read.csv("data/Figure_10.csv", sep = ";", stringsAsFactors = FALSE)

# Esperado: Sample, Treatment, Organ Material, Genera, Relative Abundance (con espacio y coma decimal)
df$Relative.Abundance <- as.numeric(gsub(",", ".", df[["Relative Abundance"]]))
df$Genera    <- trimws(df$Genera)
df$Treatment <- factor(df$Treatment, levels = c("Organic", "Conventional", "Wild"))

# ---- Shannon per sample (proportions 0–1) ----
shannon_index <- df %>%
  group_by(Sample, Treatment) %>%
  summarise(Shannon = diversity(Relative.Abundance / 100, index = "shannon"),
            .groups = "drop")

# ---- ANOVA + Tukey ----
anova_res <- aov(Shannon ~ Treatment, data = shannon_index)
tukey_tbl <- TukeyHSD(anova_res)$Treatment

# Comparaciones presentes
all_comp <- list(c("Organic","Conventional"),
                 c("Organic","Wild"),
                 c("Conventional","Wild"))
present_lvls <- levels(droplevels(shannon_index$Treatment))
comparisons <- Filter(function(x) all(x %in% present_lvls), all_comp)

key_map <- c("Conventional-Organic","Wild-Organic","Wild-Conventional")
p_vals <- tukey_tbl[key_map[key_map %in% rownames(tukey_tbl)], "p adj"]

p_labels <- sapply(p_vals, function(p){
  if (is.na(p)) "NA"
  else if (p > 0.05) "NS"
  else if (p < 0.001) "p < 0.001"
  else paste("p =", format(p, digits = 3))
})
if (length(comparisons) != length(p_labels)) {
  p_labels <- p_labels[seq_len(length(comparisons))]
}

y_max <- max(shannon_index$Shannon, na.rm = TRUE)
y_pos <- y_max + seq(0.05, 0.15, length.out = length(comparisons))

# ---- Plot ----
p <- ggplot(shannon_index, aes(x = Treatment, y = Shannon, fill = Treatment)) +
  geom_boxplot(width = 0.9) +
  theme_minimal(base_family = "Times New Roman", base_size = 13) +
  labs(x = "Treatment", y = "Shannon Diversity Index") +
  scale_fill_manual(values = c(Organic = "#1b9e77",
                               Conventional = "#d95f02",
                               Wild = "#7570b3")) +
  { if (length(comparisons) > 0)
    geom_signif(comparisons = comparisons,
                annotations = p_labels,
                map_signif_level = FALSE,
                y_position = y_pos, tip_length = 0.01,
                textsize = 3.2, vjust = 0.2)
    else NULL } +
  annotate("text", x = 0.55, y = y_max + 0.25, label = "a)",
           size = 5, family = "Times New Roman") +
  coord_cartesian(ylim = c(0, y_max + 0.35)) +
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 8)))

# ---- Export ----
CairoPDF("output/Figure_10a.pdf", width = 6, height = 5, family = "Times New Roman")
print(p); dev.off()

tiff("output/Figure_10a.tiff", units = "in", width = 6, height = 5,
     res = 600, compression = "lzw", type = "cairo")
print(p); dev.off()
