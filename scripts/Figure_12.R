library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(Cairo)
library(ggtext)

# ---- Read & clean ----
df <- read.csv("data/Figure_12.csv", sep = ";", check.names = FALSE)
names(df) <- trimws(names(df))
df$Abundance <- as.numeric(gsub(",", ".", df$Abundance))

needed <- c("Treatment","Ecosystem","Class","Abundance")
missing <- setdiff(needed, names(df))
if (length(missing)) stop(paste("Faltan columnas:", paste(missing, collapse=", ")))

df <- df %>%
  mutate(
    System = case_when(
      str_detect(Treatment, regex("Domesticated", ignore_case = TRUE)) ~ "V. vinifera subsp. vinifera",
      str_detect(Treatment, regex("Wild", ignore_case = TRUE))         ~ "V. vinifera subsp. sylvestris",
      TRUE ~ NA_character_
    ),
    Medium = case_when(
      str_detect(Treatment, regex("Commercial", ignore_case = TRUE)) ~ "Commercial Medium",
      str_detect(Treatment, regex("Natural", ignore_case = TRUE))    ~ "Natural Medium",
      TRUE ~ NA_character_
    ),
    Ecosystem = recode(Ecosystem, "Epi"="Epiphytic", "End"="Endophytic", .default = Ecosystem)
  )

df$System    <- factor(df$System, levels = c("V. vinifera subsp. vinifera", "V. vinifera subsp. sylvestris"))
df$Medium    <- factor(df$Medium, levels = c("Commercial Medium","Natural Medium"))
df$Ecosystem <- factor(df$Ecosystem, levels = c("Epiphytic","Endophytic"))

df_sum <- df %>%
  filter(!is.na(System), !is.na(Medium), !is.na(Ecosystem), !is.na(Class)) %>%
  group_by(System, Medium, Ecosystem, Class) %>%
  summarise(Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
  group_by(System, Medium, Ecosystem) %>%
  mutate(Relative_Abundance = 100 * Abundance / sum(Abundance, na.rm = TRUE)) %>%
  ungroup()

df_sum$Relative_Abundance[!is.finite(df_sum$Relative_Abundance)] <- 0

ordered_classes <- sort(unique(df_sum$Class))
df_sum$Class <- factor(df_sum$Class, levels = ordered_classes)
palette_colors <- colorRampPalette(brewer.pal(8, "Set2"))(length(ordered_classes))
palette <- setNames(palette_colors, ordered_classes)

system_labels <- c(
  "V. vinifera subsp. vinifera"   = "*V. vinifera* subsp. *vinifera*",
  "V. vinifera subsp. sylvestris" = "*V. vinifera* subsp. *sylvestris*"
)

label_threshold <- 5 # %
df_lab <- df_sum %>% mutate(label = ifelse(Relative_Abundance >= label_threshold,
                                           sprintf("%.1f%%", Relative_Abundance), ""))

final_plot <- ggplot(df_sum, aes(x = System, y = Relative_Abundance, fill = Class)) +
  geom_bar(stat = "identity", position = "stack", width = 0.28, color = NA) +
  geom_text(
    data = df_lab %>% filter(label != ""),
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3,
    family = "Times New Roman"
  ) +
  facet_grid(Ecosystem ~ Medium) +
  scale_fill_manual(values = palette, drop = FALSE) +
  scale_x_discrete(labels = system_labels) +
  theme_minimal(base_family = "Times New Roman", base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90", color = NA),
    axis.text.x = element_markdown(angle = 30, hjust = 1),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  ) +
  labs(
    title = "Fungal community structure by Domestication, Medium and Lifestyle",
    x = NULL,
    y = "Relative Abundance (%)",
    fill = "Fungal Class"
  )

CairoPDF("output/Figure_12.pdf", width = 14, height = 8, family = "Times New Roman")
print(final_plot); dev.off()

ggsave("output/Figure_12.tiff", plot = final_plot,
       dpi = 600, width = 12, height = 7, units = "in",
       compression = "lzw", bg = "white")
