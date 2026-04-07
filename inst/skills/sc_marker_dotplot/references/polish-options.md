# Polish Options

These are optional finishing layers. They are not the core of the skill.

Apply them only after the figure is already correct.

## 1. Font Harmonization

Use when the plot has:
- axis text
- titles/subtitles/captions
- text geoms or custom text grobs

Problem:
`theme()` does not automatically cover every text-containing grob in a plot object.

Source lesson:
The Y叔 article on “一次解决ggplot2所有字体” argues that full post-hoc font harmonization is possible if you work at the lower `grid` level rather than only through `theme()`.

Practical guidance:
- keep one primary family for the figure
- use a second family only if there is a strong reason
- if you need a global font pass, document clearly which text elements it touches
- do not let typography fixes silently distort size hierarchy

Use this when:
- exporting a final figure set
- trying to unify titles, axis labels, and annotation text
- preparing a figure for slides or a poster where typography inconsistency becomes obvious

## 2. Shadowed Labels

Use when labels sit on visually busy backgrounds and need separation.

Source lesson:
The “拥有立体字效果的一大波火山图正在向你走来” article highlights shadow/outline text as a readability device, not just decoration. The implementation lineage points to `shadowtext`, and the article notes similar capability is integrated into modern `ggrepel`.

Practical guidance:
- prefer for volcano plots, network plots, enrichment plots, and dense scatter plots
- use minimally in dot plots, because dot plots are grid-like and can become visually heavy very quickly
- treat it as a contrast aid, not a stylistic default

Use this when:
- labels overlap visually with points or complex backgrounds
- you need selected gene labels to remain legible without adding opaque boxes

## 3. Emoji/Icon Glyphs

Use only for:
- teaching demos
- social or outreach graphics
- internal summaries
- playful annual-review or storytelling plots

Source lesson:
The “点的形状太少？快用emoji来画图！” article uses `emojifont` to replace ordinary point markers or text labels with semantic glyphs.

Practical guidance:
- this is usually not appropriate for publication-grade single-cell marker figures
- if used, make the semantic mapping explicit
- prefer as an optional storytelling layer, not for the main scientific panel

Use this when:
- the figure is explanatory rather than formal
- icon semantics genuinely help the audience

Avoid when:
- the figure will be judged on conventional scientific presentation norms
- font portability is uncertain

## Rule of Thumb

Order of operations:
1. data logic
2. ordering logic
3. annotation logic
4. export consistency
5. polish

If polish comes earlier than step 4, the work usually degenerates into repair.
