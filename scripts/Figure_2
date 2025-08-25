# ---- Packages ----
library(ggplot2)
library(ComplexUpset)
library(patchwork)
library(ggtext)
library(cowplot)
library(showtext)

# ---- Font (cross-platform) ----
add_times <- function() {
  sys <- tolower(Sys.info()[["sysname"]])
  if (grepl("windows", sys)) {
    font_add(family = "Times New Roman", regular = "C:/Windows/Fonts/times.ttf")
  } else if (grepl("darwin", sys)) { # macOS
    font_add(family = "Times New Roman", regular = "/System/Library/Fonts/Supplemental/Times New Roman.ttf")
  } else { # Linux (instala msttcorefonts si la tienes)
    cand <- c("/usr/share/fonts/truetype/msttcorefonts/Times_New_Roman.ttf",
              "/usr/share/fonts/truetype/msttcorefonts/Times_New_Roman.tti",
              "/usr/share/fonts/truetype/freefont/FreeSerif.ttf")
    cand <- cand[file.exists(cand)]
    if (length(cand)) font_add(family = "Times New Roman", regular = cand[1])
  }
  showtext_auto()
}
add_times()

# ---- Colors & order ----
colores_azul  <- c("#6baed6", "#4292c6", "#2171b5", "#084594")
colores_verde <- c("#74c476", "#41ab5d", "#238b45", "#005a32")
organ_order   <- c("Grape", "Leaf", "Bark", "Soil")

# ---- Data ----
data <- read.csv("data/Figure_2.csv", sep = ";", check.names = TRUE)
data[is.na(data)] <- 0
data <- data[, c("Treatment", "Genera", organ_order)]

to_logical <- function(df) { df[] <- lapply(df, function(x) as.logical(as.numeric(x))); df }

df_list <- list(
  vinifera_comm   = subset(data, Treatment == "Commercial Conventional")[, organ_order],
  vinifera_nat    = subset(data, Treatment == "Natural Conventional")[, organ_order],
  sylvestris_comm = subset(data, Treatment == "Commercial Organic")[, organ_order],
  sylvestris_nat  = subset(data, Treatment == "Natural Organic")[, organ_order],
  wild_comm       = subset(data, Treatment == "Commercial Wild")[, organ_order],
  wild_nat        = subset(data, Treatment == "Natural Wild")[, organ_order]
)
df_list <- lapply(df_list, to_logical)

# ---- Plot helper ----
crear_upset_simple <- function(df, color) {
  upset(
    df,
    intersect = organ_order,
    name = "",
    sort_sets = FALSE,
    width_ratio = 0.16,
    set_sizes = (
      upset_set_size(geom = geom_bar(fill = "grey40", width = 0.7)) +
        geom_text(stat = "count",
                  aes(label = after_stat(count), y = after_stat(count)),
                  hjust = -0.15, size = 1.3) +
        coord_flip() +
        scale_y_continuous(limits = c(0, 25),
                           breaks = seq(0, 25, 5),
                           minor_breaks = NULL,
                           expand = expansion(mult = c(0, 0.30))) +
        scale_x_discrete(limits = rev(organ_order), drop = FALSE) +
        theme_minimal(base_size = 6, base_family = "Times New Roman") +
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y  = element_blank(),
              axis.text.x  = element_text(size = 5),
              axis.ticks.x = element_line(),
              panel.grid.major.y = element_blank(),
              plot.margin = margin(t = 2, r = 4, b = 2, l = 6))
    ),
    base_annotations = list(
      "Intersection size" = (
        ggplot(mapping = aes(y = after_stat(count))) +
          geom_bar(stat = "count", fill = color[1], width = 0.8) +
          stat_count(geom = "text", aes(label = after_stat(count)), vjust = -0.4, size = 2.5) +
          scale_y_continuous(breaks = seq(0, 14, 2), limits = c(0, 14)) +
          theme_minimal(base_size = 8, base_family = "Times New Roman") +
          ylab("Intersection size") +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = 8),
                axis.text.y  = element_text(size = 7),
                axis.text.x  = element_blank(),
                axis.ticks.x = element_blank(),
                panel.grid.major.y = element_blank())
      )
    )
  ) +
  theme(text = element_text(size = 8, family = "Times New Roman"),
        plot.margin = margin(3, 3, 3, 3))
}

# ---- Build panels ----
plots_upset <- list(
  crear_upset_simple(df_list$vinifera_comm, colores_azul),
  crear_upset_simple(df_list$vinifera_nat,  colores_verde),
  crear_upset_simple(df_list$sylvestris_comm, colores_azul),
  crear_upset_simple(df_list$sylvestris_nat,  colores_verde),
  crear_upset_simple(df_list$wild_comm, colores_azul),
  crear_upset_simple(df_list$wild_nat,  colores_verde)
)

row1 <- plots_upset[[1]] | plots_upset[[3]] | plots_upset[[5]]
row2 <- plots_upset[[2]] | plots_upset[[4]] | plots_upset[[6]]
plot_final <- (row1 / row2) + plot_annotation(tag_levels = "a", tag_prefix = "", tag_suffix = ")")

# ---- Save ----
ggsave("output/Figure_2.pdf", plot_final,
       device = cairo_pdf, width = 18, height = 12.5, units = "cm", dpi = 600, bg = "white")
ggsave("output/Figure_2.tiff", plot_final,
       device = "tiff", width = 18, height = 12.5, units = "cm", dpi = 600,
       compression = "lzw", bg = "white")
