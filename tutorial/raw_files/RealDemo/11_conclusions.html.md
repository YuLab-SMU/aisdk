---
title: "研究结论与展望"
subtitle: "Conclusions & Future Directions"
description: "主要发现汇总、临床转化价值与研究局限性"
---

## 研究总结


::::::{.cell layout-align="center"}

```{.default .cell-code}
mindmap
  root((PDAC 糖酵解<br/>预后模型))
    **Bulk 验证**
      ssGSEA p=1.74e-58
      93.4% 糖酵解基因上调
      Cohen's d = 3.46
    **单细胞发现**
      恶性导管细胞糖酵解最高
      Wilcoxon p=6.79e-63
      63 候选基因
    **预后模型**
      4 基因: MET NT5E LDHA TPI1
      C-index = 0.641
      1yr AUC = 0.731
    **临床转化**
      独立预后因素 HR=2.55
      吉西他滨耐药
      列线图预测
    **分子机制**
      KRAS驱动 OR=6.20
      免疫抑制微环境
      SLPI高表达
```

:::::{.cell-output-display}

::::{}
`<figure class=''>`{=html}

:::{}

<pre class="mermaid mermaid-js">mindmap
  root((PDAC 糖酵解&lt;br/&gt;预后模型))
    **Bulk 验证**
      ssGSEA p=1.74e-58
      93.4% 糖酵解基因上调
      Cohen&#39;s d = 3.46
    **单细胞发现**
      恶性导管细胞糖酵解最高
      Wilcoxon p=6.79e-63
      63 候选基因
    **预后模型**
      4 基因: MET NT5E LDHA TPI1
      C-index = 0.641
      1yr AUC = 0.731
    **临床转化**
      独立预后因素 HR=2.55
      吉西他滨耐药
      列线图预测
    **分子机制**
      KRAS驱动 OR=6.20
      免疫抑制微环境
      SLPI高表达
</pre>
:::


`<figcaption>`{=html} 研究核心发现概览`</figcaption>`{=html} `</figure>`{=html}
::::
:::::
::::::


## 主要发现

::: {.grid}

::: {.g-col-6}

### 1. 糖酵解代谢重编程

::: {.callout-important icon=false}
## 核心发现

PDAC 肿瘤组织中糖酵解通路**广泛激活**：

- Bulk 层面：ssGSEA 评分显著升高 ($p = 1.74 \times 10^{-58}$)
- 单细胞层面：恶性导管细胞糖酵解评分最高
- 93.4% (156/167) 糖酵解基因在肿瘤中上调

**效应量极大** (Cohen's $d = 3.46$)，是 PDAC 的核心代谢特征。
:::

:::

::: {.g-col-6}

### 2. 4 基因预后模型

::: {.callout-note icon=false}
## 模型性能

$$
\text{Risk Score} = 0.638 \cdot \text{MET} + 0.419 \cdot \text{NT5E} + 0.344 \cdot \text{LDHA} + 0.301 \cdot \text{TPI1}
$$

| 指标 | 数值 |
|:-----|-----:|
| C-index | 0.641 |
| 1yr AUC | 0.731 |
| Log-rank p | 0.0083 |
| 多因素 Cox HR | 2.55 |

高风险组中位 OS：14.2 月 vs 低风险组 23.8 月
:::

:::

:::


### 3. 4 个基因的生物学意义

::: {.grid}

::: {.g-col-3}
::: {.stat-card}
[MET]{.stat-number style="font-size: 1.5rem; background: linear-gradient(135deg, #E64B35, #dc2626);"}
[肝细胞生长因子受体<br/>促进侵袭转移<br/>HR = 1.89]{.stat-label}
:::
:::

::: {.g-col-3}
::: {.stat-card}
[NT5E]{.stat-number style="font-size: 1.5rem; background: linear-gradient(135deg, #7c3aed, #a855f7);"}
[CD73<br/>免疫抑制微环境<br/>HR = 1.52]{.stat-label}
:::
:::

::: {.g-col-3}
::: {.stat-card}
[LDHA]{.stat-number style="font-size: 1.5rem; background: linear-gradient(135deg, #2563eb, #3b82f6);"}
[乳酸脱氢酶 A<br/>糖酵解关键酶<br/>HR = 1.41]{.stat-label}
:::
:::

::: {.g-col-3}
::: {.stat-card}
[TPI1]{.stat-number style="font-size: 1.5rem; background: linear-gradient(135deg, #059669, #10b981);"}
[磷酸丙糖异构酶<br/>糖酵解代谢酶<br/>HR = 1.35]{.stat-label}
:::
:::

:::


### 4. 药物敏感性差异

| 分层 | 推荐方案 | 依据 |
|:-----|:---------|:-----|
| **高风险** (高糖酵解) | 紫杉醇/伊立替康为基础 (如 FOLFIRINOX) | 吉西他滨耐药 ($p < 0.0001$) |
| **低风险** (低糖酵解) | 吉西他滨标准方案 | 对吉西他滨更敏感 |
| **联合策略** | 糖酵解抑制剂 + 化疗 | 克服代谢耐药 |

: 基于风险分层的用药建议 {.striped .hover}


### 5. 分子机制关联


::::::{.cell layout-align="center"}

```{.default .cell-code}
flowchart TD
    A[KRAS 突变<br/>95.4% in High-risk] --> B[代谢重编程<br/>Warburg Effect]
    B --> C[糖酵解激活<br/>LDHA↑ TPI1↑]
    B --> D[MET 通路激活<br/>侵袭转移↑]
    B --> E[NT5E/CD73 上调<br/>免疫抑制]
    C --> F[乳酸积累<br/>酸性微环境]
    D --> F
    E --> G[NK/CD8+ T 细胞↓<br/>免疫逃逸]
    F --> H[吉西他滨耐药<br/>CDA↑ RRM1/2↑]
    G --> I[预后不良<br/>高风险组]
    H --> I
    
    style A fill:#ffe4e6,stroke:#f43f5e,stroke-width:2px
    style I fill:#fee2e2,stroke:#dc2626,stroke-width:3px
```

:::::{.cell-output-display}

::::{}
`<figure class=''>`{=html}

:::{}

<pre class="mermaid mermaid-js">flowchart TD
    A[KRAS 突变&lt;br/&gt;95.4% in High-risk] --&gt; B[代谢重编程&lt;br/&gt;Warburg Effect]
    B --&gt; C[糖酵解激活&lt;br/&gt;LDHA↑ TPI1↑]
    B --&gt; D[MET 通路激活&lt;br/&gt;侵袭转移↑]
    B --&gt; E[NT5E/CD73 上调&lt;br/&gt;免疫抑制]
    C --&gt; F[乳酸积累&lt;br/&gt;酸性微环境]
    D --&gt; F
    E --&gt; G[NK/CD8+ T 细胞↓&lt;br/&gt;免疫逃逸]
    F --&gt; H[吉西他滨耐药&lt;br/&gt;CDA↑ RRM1/2↑]
    G --&gt; I[预后不良&lt;br/&gt;高风险组]
    H --&gt; I
    
    style A fill:#ffe4e6,stroke:#f43f5e,stroke-width:2px
    style I fill:#fee2e2,stroke:#dc2626,stroke-width:3px
</pre>
:::


`<figcaption>`{=html} 糖酵解驱动 PDAC 进展的分子机制`</figcaption>`{=html} `</figure>`{=html}
::::
:::::
::::::


## 临床转化价值

::: {.grid}

::: {.g-col-4}

::: {.callout-tip icon=false}
## 🎯 预后分层

4 基因风险评分可用于 PDAC 患者的预后分层，识别高风险人群，指导随访策略。

**技术要求**：标准 RNA-seq 或 qPCR 检测 4 个基因表达
:::

:::

::: {.g-col-4}

::: {.callout-tip icon=false}
## 💊 治疗决策

基于糖酵解风险评分的药物选择：

- 高风险 → FOLFIRINOX
- 低风险 → 吉西他滨方案
- 联合 → 糖酵解抑制剂
:::

:::

::: {.g-col-4}

::: {.callout-tip icon=false}
## 🔬 靶向治疗潜力

- [MET]{.gene-name} 抑制剂 (Capmatinib, Tepotinib)
- [NT5E]{.gene-name}/CD73 抗体 (Oleclumab)
- [LDHA]{.gene-name} 抑制剂 (FX11, Gossypol)
:::

:::

:::


## 研究局限性

::: {.callout-warning}
## ⚠️ 需要注意的局限

| 局限 | 说明 | 改进方向 |
|:-----|:-----|:---------|
| 回顾性分析 | 基于公开数据库回顾性分析 | 前瞻性队列验证 |
| 单一训练集 | 仅 TCGA-PAAD 作为训练集 | 独立外部验证 (ICGC-AU) |
| 药敏预测 | 基于基因表达的计算预测 | 体外/体内药敏实验验证 |
| 机制研究 | 相关性分析为主 | 功能实验 (CRISPR, 类器官) |
| 单细胞限制 | GSE212966 1 个数据集 | 多中心 scRNA-seq 验证 |

: 研究局限与改进方向 {.striped .hover}
:::


## 未来方向

1. **外部验证**：在 ICGC-AU、GEO 独立 PDAC 队列中验证 4 基因模型
2. **功能实验**：CRISPR-Cas9 敲除/过表达实验验证基因功能
3. **类器官模型**：PDAC 类器官验证药物敏感性预测
4. **临床试验**：基于风险分层的前瞻性临床试验设计
5. **多组学整合**：蛋白质组学、代谢组学验证糖酵解代谢重编程


---

::: {.callout-note icon=false}
## 📋 项目信息

| 项目 | 信息 |
|:-----|:-----|
| **数据来源** | TCGA-PAAD, GTEx, GEO GSE212966, cBioPortal |
| **分析平台** | R (Seurat, survival, rms) + Python (pandas, gseapy, scipy) |
| **文档系统** | [Quarto](https://quarto.org) |
| **更新日期** | 2026-02-13 |
:::
