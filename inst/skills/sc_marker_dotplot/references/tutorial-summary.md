# Tutorial Summary

Title: 顶刊杂志同款高颜值的单细胞maker基因气泡图
Source: 生信技能树
Date captured: 2026-04-07
Original URL: https://mp.weixin.qq.com/s/wIsjj4jfAeKQlBV_gt3DIg

## Core Goal

Reproduce a polished marker-gene dot plot from a Cell Stem Cell paper:

- target data: human skin acute wound single-cell RNA-seq
- GEO accession: `GSE241132`
- focus lineage in the example: keratinocyte subclusters
- visual target: dot plot of curated markers with a top annotation strip

## Dataset Context

The tutorial uses:
- GEO: `GSE241132`
- metadata file merged back into the Seurat object
- keratinocytes extracted from a larger object

The point of the tutorial is not the biological story itself.
The point is the figure pattern.

## Technical Pattern

1. Read and merge 10X-style folders into a Seurat object
2. Add cell metadata
3. Subset to keratinocytes
4. Normalize and prepare the subset
5. Define a manually curated marker vector
6. Define a fixed cluster order
7. Build a `DotPlot()`
8. Add the top annotation strip with `annotation_custom()` and `grid::rectGrob()`

## Important Implementation Detail

The top annotation strip is hand-built by:
- storing start and end x positions for marker groups
- converting them to `xmin` and `xmax`
- drawing rectangles above the panel
- turning clipping off with `coord_cartesian(..., clip = "off")`
- adding top margin so the strip remains visible

This is the distinctive part of the tutorial.

## Why This Tutorial Is Skill-Worthy

Many users can already call `DotPlot()`.
What they usually lack is:
- publication-style ordering
- clean grouping logic
- the annotation strip trick
- knowing exactly which positions to edit when the marker list changes

So the reusable skill is:
"turn a Seurat object + marker list + subtype order into a polished dot plot with a top annotation bar."
