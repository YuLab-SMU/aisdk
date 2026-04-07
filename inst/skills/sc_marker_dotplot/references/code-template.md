# Code Template

Use this as the starting template. Adjust the metadata columns, subset condition, marker vector, and annotation-group boundaries.

```r
library(Seurat)
library(ggplot2)
library(ggh4x)
library(grid)

# Helper: keep panel size fixed while computing full canvas size automatically
save_fixed_plot <- function(plot, filename, panel_w = 5, panel_h = 5, dpi = 300) {
  p_fixed <- plot + ggh4x::force_panelsizes(
    rows = unit(panel_h, "cm"),
    cols = unit(panel_w, "cm")
  )

  gt <- ggplot_gtable(ggplot_build(p_fixed))

  pdf(NULL)
  total_w <- convertWidth(sum(gt$widths), "cm", valueOnly = TRUE)
  total_h <- convertHeight(sum(gt$heights), "cm", valueOnly = TRUE)
  dev.off()

  ggplot2::ggsave(
    filename,
    plot = gt,
    width = total_w,
    height = total_h,
    units = "cm",
    dpi = dpi
  )

  message(
    paste0(
      "Saved: ", filename,
      " (canvas: ", round(total_w, 2), " x ", round(total_h, 2), " cm)"
    )
  )
}

# 1. Start from an existing Seurat object
# sce.all <- readRDS("your_seurat_object.rds")

# 2. Subset to the lineage / cell class of interest
cells_sub <- subset(sce.all, newMainCellTypes == "Keratinocyte")

# Optional: only if preprocessing is still needed
cells_sub <- NormalizeData(cells_sub)
cells_sub <- FindVariableFeatures(cells_sub, nfeatures = 2000)
cells_sub <- ScaleData(cells_sub)
cells_sub <- RunPCA(cells_sub)

# 3. Fix plotting order
fac_levs <- c(
  "Bas-I", "Bas-prolif", "Bas-mig", "Bas-II",
  "Spi-I", "Spi-II", "Spi-mig",
  "Gra-I"
)
cells_sub$plot_group <- factor(cells_sub$newCellTypes, levels = rev(fac_levs))

# 4. Curated marker order
top_repre_markers <- c(
  "KRT15", "KRT31", "COL17A1", "ASS1", "POSTN",
  "NUSAP1", "STMN1", "TOP2A", "MKI67", "CENPF",
  "MMP3", "MMP1", "AREG", "TNFRSF12A", "FGFBP1"
)

# 5. Base dot plot
plot_marker <- DotPlot(
  cells_sub,
  features = top_repre_markers,
  group.by = "plot_group",
  cols = c("white", "#cb181d"),
  dot.scale = 5,
  col.min = 0,
  dot.min = 0.1
) +
  labs(x = "", y = "") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.border = element_rect(colour = "black", fill = NA)
  )

# 6. Group boundary positions for the top annotation strip
xPosition <- list(
  c(1, 6, 11),
  c(5, 10, 15)
)
yPosition <- 9
pCol <- c("#7f7db6", "#9d9ac4", "#d2eac8")

xmin <- xPosition[[1]] - 0.6
xmax <- xPosition[[2]] + 0.2
ymin <- yPosition - 0.1
ymax <- yPosition + 0.4

object <- plot_marker
nPoints <- length(xmin)

for (i in seq_len(nPoints)) {
  object <- object +
    annotation_custom(
      grob = rectGrob(
        gp = gpar(
          col = "black",
          fill = pCol[i],
          lwd = 1.3,
          lty = "solid",
          lineend = "square",
          alpha = 1
        )
      ),
      xmin = xmin[i],
      xmax = xmax[i],
      ymin = ymin,
      ymax = ymax
    ) +
    coord_cartesian(ylim = c(1, length(fac_levs)), clip = "off")
}

final_plot <- object +
  theme(plot.margin = margin(t = 30, unit = "pt"))

final_plot

# 7. Export with fixed per-panel size
save_fixed_plot(
  final_plot,
  filename = "marker_dotplot_fixed.svg",
  panel_w = 4,
  panel_h = 4
)
```

## What To Edit First

1. `subset(...)` condition
2. `fac_levs`
3. `top_repre_markers`
4. `xPosition`
5. `pCol`
6. `panel_w` / `panel_h` in `save_fixed_plot()`

The annotation positions must be updated whenever the marker grouping changes.
The fixed-export helper is most useful when you want the core plotting area to stay constant even if the legend, title, or number of facets changes.
