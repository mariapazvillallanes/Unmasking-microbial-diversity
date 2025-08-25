# Paquetes
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(Cairo)

# Leer y preparar datos
df <- read.csv("Figure_4.csv", sep = ";")
df$Abundance <- as.numeric(gsub(",", ".", df$Abundance))

df <- df %>%
  mutate(
    System = case_when(
      grepl("Conventional", Treatment) ~ "Conventional Management",
      grepl("Organic", Treatment) ~ "Organic Management",
      grepl("Wild", Treatment) ~ "Wild Management"
    ),
    Medium = case_when(
      grepl("Commercial", Treatment) ~ "Commercial Medium",
      grepl("Natural", Treatment) ~ "Natural Medium"
    )
  )

# Factores
ordered_classes <- sort(unique(df$Class))
df$Class <- factor(df$Class, levels = ordered_classes)
levels_ecosystem <- c("Soil", "Leaf", "Grape", "Bark", "Total")
df$Ecosystem <- factor(df$Ecosystem, levels = levels_ecosystem)

# Calcular abundancia relativa
df <- df %>%
  group_by(Treatment, Ecosystem) %>%
  mutate(Relative_Abundance = 100 * Abundance / sum(Abundance)) %>%
  ungroup()

df <- df %>%
  complete(Class, nesting(Treatment, Ecosystem, System, Medium),
           fill = list(Abundance = 0, Relative_Abundance = 0))

# Paleta de colores
n_classes <- length(ordered_classes)
palette_colors <- if (n_classes <= 8) {
  brewer.pal(n_classes, "Set2")
} else {
  c(brewer.pal(8, "Set2"),
    brewer.pal(min(n_classes - 8, 12), "Set3"))
}
palette <- setNames(palette_colors[1:n_classes], ordered_classes)

# Función para crear un gráfico individual
# Función para crear un gráfico individual con etiquetas %
create_plot <- function(system, medium) {
  df %>%
    filter(System == system, Medium == medium) %>%
    ggplot(aes(x = Ecosystem, y = Relative_Abundance, fill = Class)) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_text(aes(label = ifelse(Relative_Abundance >= 5, 
                                 sprintf("%.1f%%", Relative_Abundance), "")),
              position = position_stack(vjust = 0.5),
              size = 3, family = "Times New Roman", color = "black") +
    scale_fill_manual(values = palette, drop = FALSE) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    theme_minimal(base_family = "Times New Roman", base_size = 13) +
    labs(x = NULL, y = NULL, fill = "Fungal Class") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      strip.text = element_blank(),
      legend.position = "none"
    )
}


# Crear los 6 paneles
plot_a <- create_plot("Conventional Management", "Commercial Medium") + ggtitle("Commercial Medium")
plot_b <- create_plot("Conventional Management", "Natural Medium") + ggtitle("Natural Medium")
plot_c <- create_plot("Organic Management", "Commercial Medium") + ggtitle("Commercial Medium")
plot_d <- create_plot("Organic Management", "Natural Medium") + ggtitle("Natural Medium")
plot_e <- create_plot("Wild Management", "Commercial Medium") + ggtitle("Commercial Medium")
plot_f <- create_plot("Wild Management", "Natural Medium") + ggtitle("Natural Medium")

# Títulos principales por sistema
title_conventional <- ggplot() + 
  annotate("text", x = 1, y = 1, label = "Conventional Management", 
           family = "Times New Roman", size = 5, fontface = "bold") +
  theme_void()

title_organic <- ggplot() + 
  annotate("text", x = 1, y = 1, label = "Organic Management", 
           family = "Times New Roman", size = 5, fontface = "bold") +
  theme_void()

title_wild <- ggplot() + 
  annotate("text", x = 1, y = 1, label = "Wild Management", 
           family = "Times New Roman", size = 5, fontface = "bold") +
  theme_void()

# Composición del gráfico
top_row <- title_conventional + title_organic + title_wild + plot_layout(ncol = 3)

bottom_row <- (plot_a + plot_b) / (plot_c + plot_d) / (plot_e + plot_f) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "", tag_suffix = ")")

final_plot <- top_row / bottom_row +
  plot_layout(heights = c(0.1, 1)) &
  theme(legend.position = "right")

# Mostrar en pantalla
print(final_plot)

# Guardar como PDF
CairoPDF("Figure_4.pdf", width = 15, height = 10, family = "Times New Roman")
print(final_plot)
dev.off()

# Guardar como TIFF
ggsave("Figure_4.tiff", plot = final_plot,
       dpi = 600, width = 15, height = 10, units = "in",
       compression = "lzw", bg = "white")
