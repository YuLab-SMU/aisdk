---
title: "单细胞 RNA-seq 分析"
subtitle: "Single-Cell RNA-seq Analysis"
description: "GSE212966 数据集 Seurat 标准流程：QC、降维、聚类与细胞类型注释"
---

## 分析概览

对 GSE212966 数据集（6 PDAC + 6 Adjacent 样本，共 69,105 个细胞）执行标准 Seurat 分析流程。


::::::{.cell layout-align="center"}

```{.default .cell-code}
flowchart LR
    A[12 样本<br/>69,105 细胞] --> B[QC 过滤<br/>nFeature 200-6000<br/>MT% < 20%]
    B --> C[57,623 细胞<br/>标准化 + HVG]
    C --> D[PCA<br/>50 PCs]
    D --> E[UMAP<br/>分辨率 0.8]
    E --> F[37 clusters]
    F --> G[细胞类型注释<br/>11 types]

    style A fill:#ede9fe,stroke:#7c3aed
    style G fill:#ede9fe,stroke:#7c3aed
```

:::::{.cell-output-display}

::::{}
`<figure class=''>`{=html}

:::{}

<pre class="mermaid mermaid-js">flowchart LR
    A[12 样本&lt;br/&gt;69,105 细胞] --&gt; B[QC 过滤&lt;br/&gt;nFeature 200-6000&lt;br/&gt;MT% &lt; 20%]
    B --&gt; C[57,623 细胞&lt;br/&gt;标准化 + HVG]
    C --&gt; D[PCA&lt;br/&gt;50 PCs]
    D --&gt; E[UMAP&lt;br/&gt;分辨率 0.8]
    E --&gt; F[37 clusters]
    F --&gt; G[细胞类型注释&lt;br/&gt;11 types]

    style A fill:#ede9fe,stroke:#7c3aed
    style G fill:#ede9fe,stroke:#7c3aed
</pre>
:::


`<figcaption>`{=html} 单细胞分析流程`</figcaption>`{=html} `</figure>`{=html}
::::
:::::
::::::


## 质控过滤


::: {.cell}

```{.r .cell-code}
library(Seurat)

# 实际分析加载已保存的 `.rds` 文件，直接查看 QC 后的状态
if (file.exists("../03_single_cell_analysis/pdac_seurat_annotated.rds")) {
    cat("正在加载预处理的单细胞数据...\n")
    combined_filtered <- readRDS("../03_single_cell_analysis/pdac_seurat_annotated.rds")
    
    cat(sprintf("质控与过滤后剩余: %d cells, %d genes\n", 
                ncol(combined_filtered), nrow(combined_filtered)))
    
    # 打印细胞来源统计
    if ("condition" %in% colnames(combined_filtered@meta.data)) {
        print(table(combined_filtered$condition))
    }
} else {
    cat("未找到 pdac_seurat_annotated.rds 文件。\n")
}
```

::: {.cell-output .cell-output-stdout}

```
正在加载预处理的单细胞数据...
质控与过滤后剩余: 57623 cells, 27579 genes

Adjacent    Tumor 
   24515    33108 
```


:::
:::


::: {.callout-note}
## 📊 QC 统计

| 指标 | 中位数 | 范围 |
|:-----|-------:|:-----|
| nFeature_RNA | 1,471 | [199, 12,281] |
| nCount_RNA | 4,653 | [491, 369,464] |
| percent.mt | 6.61% | [0.00%, 97.31%] |

过滤前：**69,105** 细胞 → 过滤后：**57,623** 细胞（去除 16.6%）

- Tumor 细胞: **33,108**
- Adjacent 细胞: **24,515**
:::


## 降维聚类


::: {.cell}

```{.r .cell-code}
# 标准化 → 高变基因 → 缩放 → PCA → UMAP (演示流程，耗时较长在此页面仅做展示)
combined_filtered <- NormalizeData(combined_filtered)
combined_filtered <- FindVariableFeatures(combined_filtered, nfeatures = 2000)
combined_filtered <- ScaleData(combined_filtered)
combined_filtered <- RunPCA(combined_filtered, npcs = 50)
combined_filtered <- FindNeighbors(combined_filtered, dims = 1:30)
combined_filtered <- FindClusters(combined_filtered, resolution = 0.8)
combined_filtered <- RunUMAP(combined_filtered, dims = 1:30)
```
:::



## 细胞类型注释

基于 Marker 基因的 `AddModuleScore` 自动注释，识别出 **11 种主要细胞类型**：

::: {.panel-tabset}

### 细胞类型表

| 细胞类型 | 细胞数 | 比例 | 标记基因 | 主要来源 |
|:---------|-------:|-----:|:---------|:---------|
| T cell | 18,128 | 31.5% | CD3D, CD3E, CD2 | 免疫 |
| Fibroblast | 10,057 | 17.5% | COL1A1, ACTA2, DCN | 基质 |
| Acinar | 5,530 | 9.6% | PRSS1, AMY2A, CPA1 | 外分泌 |
| Myeloid | 5,449 | 9.5% | CD68, CD163, CD14 | 免疫 |
| B cell | 4,963 | 8.6% | CD79A, MS4A1, CD19 | 免疫 |
| **Malignant Ductal** | **4,722** | **8.2%** | KRT19, EPCAM, MUC1 | **肿瘤** |
| NK cell | 3,105 | 5.4% | GNLY, NKG7, KLRD1 | 免疫 |
| Endothelial | 3,034 | 5.3% | PECAM1, VWF, CDH5 | 血管 |
| Benign Ductal | 1,437 | 2.5% | KRT19, CFTR | 导管 |
| Mast cell | 994 | 1.7% | KIT, TPSAB1 | 免疫 |
| Endocrine | 204 | 0.4% | INS, GCG, SST | 内分泌 |

: 细胞类型注释结果 {.striped .hover}

:::

![UMAP: 11 种细胞亚群](figures/03_single_cell/umap_celltype_detailed.png){fig-align="center" width="85%"}
![UMAP: Tumor vs Adjacent 细胞来源](figures/03_single_cell/umap_celltype_condition.png){fig-align="center" width="85%"}


## R 可视化代码


::: {.cell}

```{.r .cell-code}
library(Seurat)
library(ggplot2)

# 细胞类型配色方案
celltype_colors <- c(
  "Malignant_Ductal" = "#E64B35",  "Benign_Ductal" = "#F8B4B4",
  "T_cell"           = "#8491B4",  "B_cell"        = "#91D1C2",
  "NK_cell"          = "#7E6148",  "Myeloid"       = "#DC0000",
  "Mast_cell"        = "#B09C85",  "Fibroblast"    = "#3C5488",
  "Endothelial"      = "#F39B7F",  "Acinar"        = "#4DBBD5",
  "Endocrine"        = "#00A087"
)

# 绘制 UMAP图前，检查对象是否存在
if (exists("combined_filtered")) {
    DimPlot(combined_filtered, 
            group.by = "cell_type_detailed",
            cols = celltype_colors,
            pt.size = 0.3, 
            label = TRUE, label.size = 4) +
      ggtitle("PDAC Single-Cell Atlas — Cell Type Annotation") +
      theme(plot.title = element_text(face = "bold", size = 16))
} else {
    # mock a simple plot if data not available
    plot(1, type="n", main="UMAP details not available (Data not loaded)", axes=FALSE, xlab="", ylab="")
}
```

::: {.cell-output-display}
![UMAP — 细胞类型注释](03_single_cell_files/figure-html/fig-umap-celltype-1.png){#fig-umap-celltype}
:::
:::



## 恶性细胞鉴定

基于 CNV 推断，将导管细胞分为**恶性导管细胞** (Tumor来源) 和**良性导管细胞** (Adjacent来源)：

::: {.grid}

::: {.g-col-6}

| 分类 | 细胞数 | 占导管细胞比例 |
|:-----|-------:|:-------------:|
| Malignant Ductal | 4,722 | **76.7%** |
| Benign Ductal | 1,437 | 23.3% |

:::

::: {.g-col-6}


::: {.cell}

```{.python .cell-code}
import pandas as pd

try:
    # 演示通过 metadata 快速统计恶性 vs 良性导管细胞
    meta = pd.read_csv('../03_single_cell_analysis/cell_metadata.csv', index_col=0)
    ductal_cells = meta[meta['cell_type_detailed'].isin(['Malignant_Ductal', 'Benign_Ductal'])]
    
    # 统计数量
    stats = ductal_cells['cell_type_detailed'].value_counts()
    print("导管细胞分类统计:")
    for ct, count in stats.items():
        print(f" - {ct}: {count} ({count/len(ductal_cells)*100:.1f}%)")
except Exception as e:
    print("获取元数据统计失败:", e)
```

::: {.cell-output .cell-output-stdout}

```
导管细胞分类统计:
 - Malignant_Ductal: 4722 (76.7%)
 - Benign_Ductal: 1437 (23.3%)
```


:::
:::


:::
:::


## 小结

::: {.callout-tip}
## ✅ 本节结论

1. 57,623 个高质量细胞通过 QC，识别出 **11 种细胞类型**
2. **恶性导管细胞** (4,722 个) 是后续糖酵解分析的核心
3. 肿瘤微环境包含丰富的免疫细胞（T 细胞 31.5%、B 细胞 8.6%）和基质细胞（成纤维细胞 17.5%）
4. 数据保存为 `pdac_seurat_annotated.rds` 供后续分析使用
:::
