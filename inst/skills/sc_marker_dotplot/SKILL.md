---
name: sc-marker-dotplot
description: |
  Reproduce publication-style single-cell marker gene dot plots in R/Seurat, especially dot plots with carefully ordered cell subtypes and a colored annotation bar above the plot. Use this skill whenever the user wants a high-quality Seurat `DotPlot`, marker-gene bubble plot, top-journal-style single-cell marker figure, keratinocyte/subcluster marker visualization, or asks how to add top annotation segments above a dot plot in ggplot2.
aliases:
  - "single-cell marker dotplot"
  - "Seurat DotPlot"
  - "marker gene bubble plot"
  - "单细胞气泡图"
  - "marker基因气泡图"
  - "单细胞marker气泡图"
  - "Seurat marker plot"
user-invocable: true
---

# Single-Cell Marker DotPlot

Use this skill when the task is specifically about making a polished single-cell marker-gene dot plot in R, usually from a Seurat object, and especially when the plot needs:
- a fixed subtype order
- a manually curated marker list
- clean `DotPlot()` styling
- a colored annotation strip above the figure
- reproducible export size with fixed panel dimensions
- optional typography or label polish for publication / presentation use

This skill is not a full single-cell analysis workflow. It assumes the user already has a Seurat object or is close to one.

## What This Skill Solves

This pattern is for figures like:
- one cell class extracted from a larger Seurat object
- selected marker genes shown across refined subclusters
- dot size for fraction expressing
- color for expression level
- an extra top band that visually groups genes into blocks

The important idea is:
1. prepare the Seurat subset cleanly
2. define marker order explicitly
3. define cluster order explicitly
4. render `DotPlot()`
5. add the top annotation strip with `annotation_custom()` so it stays locked to the same x coordinates as the gene columns
6. export with fixed panel dimensions so layout stays stable across legend/facet changes
7. optionally polish fonts and labels without disturbing the data mapping

## Inputs You Usually Need

- a Seurat object
- the metadata column used for subcluster labels
- the subset of cells to plot
- a marker vector in the exact desired order
- the display order of subtypes
- optional gene-group boundaries for the top annotation bar

If any of these are missing, ask for them or infer a sensible default and state it.

## Workflow

### 1. Narrow to the target cell population

If the plot is meant for one major lineage, subset first.

Typical pattern:

```r
cells_sub <- subset(sce.all, newMainCellTypes == "Keratinocyte")
cells_sub <- NormalizeData(cells_sub)
cells_sub <- FindVariableFeatures(cells_sub, nfeatures = 2000)
cells_sub <- ScaleData(cells_sub)
cells_sub <- RunPCA(cells_sub)
```

Do not pretend PCA is required just for plotting if the object is already normalized and usable. Reuse existing preprocessing when available.

### 2. Fix cluster order before plotting

Do not trust default alphabetical order.

Convert the grouping column to a factor with explicit levels.

```r
fac_levs <- c("Bas-I", "Bas-prolif", "Bas-mig", "Bas-II",
              "Spi-I", "Spi-II", "Spi-mig",
              "Gra-I")

cells_sub$plot_group <- factor(cells_sub$newCellTypes, levels = rev(fac_levs))
```

Reverse only if the visual arrangement actually needs it.

### 3. Keep the marker vector deliberate

Use a manually curated marker vector in the final display order.

Do not sort it automatically.
Do not silently deduplicate without telling the user.

If the user gives a marker table rather than a vector, extract the final display order explicitly.

### 4. Build the base `DotPlot()`

Start with a plain, readable dot plot.

```r
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
```

Prefer restrained defaults. Do not overdecorate the first pass.

Before moving on, check these quality gates:
- the top row of data is fully visible
- the legend does not crowd the plotting region
- x-axis labels are readable at a glance
- low-expression dots are still visible against the background
- gridlines do not compete with the bubbles
- y-axis labels carry biological meaning rather than anonymous integers

### 5. Add the top annotation strip

This is the distinctive part.

Preferred approach:
- keep the annotation strip in the same plot object
- place it above the panel using `annotation_custom()`
- keep it outside the data region with `clip = "off"` and sufficient top margin

Do not create a second plot panel unless small x-alignment drift is acceptable.

Define start/end x positions for each marker block, plus the colors:

```r
xPosition <- list(
  c(1, 6, 11, 18, 23, 26, 29, 34, 42),
  c(5, 10, 17, 22, 25, 28, 33, 41, 46)
)
yPosition <- 9
pCol <- c("#7f7db6", "#9d9ac4", "#d2eac8", "#f09e45", "#f5c664",
          "#fae49b", "#eb8777", "#bcdd78", "#f5cfe4")

xmin <- xPosition[[1]] - 0.6
xmax <- xPosition[[2]] + 0.2
ymin <- yPosition - 0.1
ymax <- yPosition + 0.4
```

Then add the strip with `annotation_custom()`:

```r
object <- plot_marker

for (i in seq_along(xmin)) {
  object <- object +
    annotation_custom(
      grob = grid::rectGrob(
        gp = grid::gpar(
          col = "black",
          fill = pCol[i],
          lwd = 0.9,
          lty = "solid"
        )
      ),
      xmin = xmin[i],
      xmax = xmax[i],
      ymin = length(fac_levs) + 0.62,
      ymax = length(fac_levs) + 0.84
    )
}

final_plot <- object +
  coord_cartesian(
    ylim = c(0.5, length(fac_levs) + 0.5),
    clip = "off"
  ) +
  theme(plot.margin = margin(t = 34, r = 18, b = 6, l = 6, unit = "pt"))
```

This keeps the strip aligned to the exact same x coordinate system as the bubbles.

If the strip overlaps the first data row, the figure is wrong.
If the strip is misaligned with the marker columns, the figure is wrong.

## Visual QA Rules

Reject the figure and fix it if any of the following happens:

- the top annotation bar covers any bubble or label
- the legend feels pushed into the panel or too close to the plot edge
- x-axis labels are harder to read than they need to be
- the low end of the color scale is so pale that dots disappear into the background
- gridlines dominate the bubble marks
- y-axis labels are only numeric and the biological identity is not explained

Default fixes:
- keep the annotation strip in the same plot and place it beyond the panel top
- add right margin when the legend is on the right
- prefer 45 to 60 degree x-label rotation when the marker count allows it
- use a higher-contrast low-end color than pure white
- lighten or simplify gridlines
- replace cluster indices with subtype labels whenever possible

### 6. Export with fixed panel size when figure size matters

If the user cares about publication consistency, do not rely on manual trial-and-error `ggsave(width=..., height=...)`.

Use a helper that:
- locks the physical panel size
- lets legends/titles/facets expand outside that panel naturally
- computes the final canvas size from the built gtable

This is especially useful when:
- the legend position changes
- facet count changes
- top annotation bars extend outside the panel
- the user wants multiple figures with the same core plotting area

See `references/code-template.md` for a reusable `save_fixed_plot()` helper based on `ggh4x::force_panelsizes()`.

### 7. Add polish only after the figure logic is correct

Do not start with cosmetics.
First make sure:
- subtype order is right
- marker order is right
- top annotation groups align with the marker blocks
- legends and labels still explain the data honestly

Only then consider these polish layers:

- `font harmonization`
  Use when the plot mixes axis text, titles, and text grobs that need a consistent family or face.

- `shadowed labels`
  Use when gene labels or annotations sit on busy backgrounds and need stronger separation. Prefer this for labeled volcano/network/enrichment plots, and use sparingly in dot plots.

- `emoji/icon glyphs`
  Use only for outreach, teaching, summaries, or highly stylized presentations. This is usually not the right default for formal marker-gene figures.

Keep the plot readable. A polished figure is not permission to become noisy.

## Common Pitfalls

- Forgetting to set factor levels, so subtype order is wrong
- Letting ggplot clip the top annotation bar
- Using gene-group boundaries that do not match the marker vector length
- Saving by guessed width/height so the core panel changes size between figures
- Over-styling labels before the ordering and annotation logic is correct
- Using emoji/icon treatments in figures that should remain conventional and publication-first
- Recomputing the whole Seurat workflow unnecessarily when the object is already ready
- Confusing “dot size” and “expression color” semantics in the legend/explanation

## What To Explain To The User

If you produce or modify code, explain:
- which metadata column is used for grouping
- where subtype order is controlled
- where marker order is controlled
- how the annotation strip boundaries correspond to gene blocks
- which numbers to edit when the marker list changes

## When To Escalate Beyond This Skill

Route beyond this skill when the user is really asking for:
- full single-cell QC / clustering / annotation
- ComplexHeatmap instead of DotPlot
- scanpy/Python implementation
- automated marker discovery rather than curated marker display

## Reference Files

- `references/tutorial-summary.md`
  Distilled notes from the source tutorial, including the exact plotting idea and the GEO dataset context.

- `references/code-template.md`
  Reusable code skeleton for building this style of figure from a Seurat object, including a fixed-panel export helper.

- `references/polish-options.md`
  Optional styling moves distilled from additional ggplot-focused tutorials: font unification, shadow text, and emoji/icon labeling.

- `sources/manifest.md`
  Source provenance for every tutorial used to shape this skill, including URLs, fetched titles, and notes.
