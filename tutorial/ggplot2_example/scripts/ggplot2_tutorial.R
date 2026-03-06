# ggplot2 教学实例
# 作者：R AI SDK Terminal Assistant
# 日期：2025-06-17

# 1. 准备工作 ============================================

# 安装并加载必要的包
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")

library(ggplot2)
library(dplyr)
library(tidyr)

# 设置主题样式
theme_set(theme_minimal(base_size = 12))

# 2. 使用内置数据集 =======================================

# 查看内置数据集
data(mpg)  # 汽车燃油效率数据
data(diamonds)  # 钻石数据集
data(Titanic)  # 泰坦尼克号数据

# 3. 基础图表类型 =========================================

# 散点图 - 基础
p1 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  labs(
    title = "汽车发动机排量与高速公路燃油效率",
    x = "发动机排量 (升)",
    y = "高速公路燃油效率 (mpg)"
  )
print(p1)

# 保存散点图
ggsave("plots/scatter_basic.png", p1, width = 8, height = 6, dpi = 300)

# 散点图 - 添加颜色映射
p2 <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "不同车型的燃油效率",
    x = "发动机排量 (升)",
    y = "高速公路燃油效率 (mpg)",
    color = "车型类别"
  ) +
  theme(legend.position = "right")
print(p2)
ggsave("plots/scatter_by_class.png", p2, width = 8, height = 6, dpi = 300)

# 折线图 - 时间序列示例
# 创建示例时间序列数据
set.seed(123)
time_data <- data.frame(
  date = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "month"),
  value = cumsum(rnorm(12, mean = 5, sd = 2)) + 100
)

p3 <- ggplot(time_data, aes(x = date, y = value)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 3) +
  labs(
    title = "2024年月度趋势",
    x = "月份",
    y = "累计值"
  ) +
  scale_x_date(date_labels = "%Y-%m")
print(p3)
ggsave("plots/line_time_series.png", p3, width = 10, height = 6, dpi = 300)

# 柱状图 - 统计汇总
mpg_summary <- mpg %>%
  group_by(class) %>%
  summarise(
    avg_hwy = mean(hwy, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_hwy))

p4 <- ggplot(mpg_summary, aes(x = reorder(class, avg_hwy), y = avg_hwy)) +
  geom_col(fill = "skyblue", width = 0.7) +
  coord_flip() +
  labs(
    title = "不同车型的平均高速公路燃油效率",
    x = "车型类别",
    y = "平均燃油效率 (mpg)"
  ) +
  geom_text(aes(label = round(avg_hwy, 1)), 
            hjust = -0.1, size = 3.5)
print(p4)
ggsave("plots/bar_class_avg.png", p4, width = 9, height = 6, dpi = 300)

# 直方图 - 分布分析
p5 <- ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2, fill = "lightgreen", color = "black", alpha = 0.8) +
  labs(
    title = "高速公路燃油效率分布",
    x = "燃油效率 (mpg)",
    y = "频数"
  ) +
  geom_vline(aes(xintercept = mean(hwy)), 
             color = "red", linetype = "dashed", linewidth = 1)
print(p5)
ggsave("plots/hist_hwy.png", p5, width = 8, height = 6, dpi = 300)

# 箱线图 - 多组比较
p6 <- ggplot(mpg, aes(x = class, y = hwy, fill = class)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  labs(
    title = "不同车型的燃油效率分布",
    x = "车型类别",
    y = "燃油效率 (mpg)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "C", guide = "none")
print(p6)
ggsave("plots/boxplot_class.png", p6, width = 10, height = 6, dpi = 300)

# 密度图
p7 <- ggplot(mpg, aes(x = hwy, fill = drv)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "不同驱动方式的燃油效率分布",
    x = "燃油效率 (mpg)",
    y = "密度",
    fill = "驱动方式"
  ) +
  scale_fill_brewer(palette = "Set1")
print(p7)
ggsave("plots/density_drv.png", p7, width = 8, height = 6, dpi = 300)

# 4. 进阶图表 =============================================

# 分面图 (Facet)
p8 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "darkorange", alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_grid(drv ~ cyl, scales = "free") +
  labs(
    title = "不同驱动方式和气缸数的排量与燃油效率关系",
    x = "发动机排量 (升)",
    y = "高速公路燃油效率 (mpg)"
  )
print(p8)
ggsave("plots/facet_drv_cyl.png", p8, width = 12, height = 8, dpi = 300)

# 热力图 - 相关性矩阵
# 计算数值变量的相关系数
numeric_vars <- mpg %>% 
  select(where(is.numeric)) %>%
  select(-year)

cor_matrix <- cor(numeric_vars, use = "complete.obs")

cor_data <- as.data.frame(as.table(cor_matrix)) %>%
  rename(var1 = Var1, var2 = Var2, correlation = Freq)

p9 <- ggplot(cor_data, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1)) +
  labs(
    title = "MPG数据集数值变量相关性热力图",
    x = "",
    y = "",
    fill = "相关系数"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p9)
ggsave("plots/heatmap_correlation.png", p9, width = 10, height = 8, dpi = 300)

# 5. 钻石数据集示例 =======================================

# 钻石切工与价格关系
diamond_summary <- diamonds %>%
  group_by(cut) %>%
  summarise(
    avg_price = mean(price),
    median_price = median(price),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(cut = factor(cut, levels = c("Fair", "Good", "Very Good", "Premium", "Ideal")))

p10 <- ggplot(diamond_summary, aes(x = cut, y = avg_price, fill = cut)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("$", round(avg_price/1000, 1), "K")),
            vjust = -0.5, size = 4) +
  labs(
    title = "不同切工级别的平均钻石价格",
    x = "切工等级",
    y = "平均价格 (美元)"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_brewer(palette = "Blues", direction = -1, guide = "none")
print(p10)
ggsave("plots/diamond_cut_price.png", p10, width = 9, height = 6, dpi = 300)

# 钻石克拉与价格散点图（采样显示）
set.seed(123)
diamond_sample <- diamonds %>% sample_n(5000)

p11 <- ggplot(diamond_sample, aes(x = carat, y = price, color = cut)) +
  geom_point(alpha = 0.6, size = 1.5) +
  labs(
    title = "钻石克拉与价格关系（采样5000颗）",
    x = "克拉 (Carat)",
    y = "价格 (美元)",
    color = "切工等级"
  ) +
  scale_color_brewer(palette = "Set2")
print(p11)
ggsave("plots/diamond_carat_price.png", p11, width = 10, height = 7, dpi = 300)

# 6. 组合图表 - 多图层 ====================================

# 散点图 + 回归线 + 置信区间
p12 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "darkgray", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", fill = "pink", se = TRUE) +
  labs(
    title = "发动机排量与燃油效率关系",
    subtitle = "红色线为线性回归拟合，阴影为95%置信区间",
    x = "发动机排量 (升)",
    y = "高速公路燃油效率 (mpg)"
  )
print(p12)
ggsave("plots/scatter_regression.png", p12, width = 9, height = 6, dpi = 300)

# 堆叠面积图 - 模拟数据
area_data <- data.frame(
  year = rep(2019:2023, 3),
  category = rep(c("产品A", "产品B", "产品C"), each = 5),
  value = c(120, 135, 150, 165, 180,  # 产品A
            80, 85, 90, 95, 100,       # 产品B
            50, 55, 60, 65, 70)        # 产品C
)

p13 <- ggplot(area_data, aes(x = year, y = value, fill = category)) +
  geom_area(alpha = 0.8) +
  labs(
    title = "各产品年度销售趋势（堆叠面积图）",
    x = "年份",
    y = "销售额（百万）",
    fill = "产品类别"
  ) +
  scale_fill_brewer(palette = "Set3")
print(p13)
ggsave("plots/area_stacked.png", p13, width = 9, height = 6, dpi = 300)

# 7. 自定义主题和样式 =====================================

# 自定义主题示例
custom_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2C3E50"),
  plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#7F8C8D"),
  axis.title = element_text(size = 12, face = "bold"),
  axis.text = element_text(size = 10),
  legend.title = element_text(face = "bold"),
  panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "white"),
  panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
)

p14 <- ggplot(mpg, aes(x = class, y = hwy, fill = class)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "燃油效率分布 - 自定义主题",
    subtitle = "使用自定义颜色和排版",
    x = "车型类别",
    y = "燃油效率 (mpg)"
  ) +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1", 
                                "#96CEB4", "#FFEAA7", "#DDA0DD", "#98D8C8")) +
  custom_theme +
  theme(legend.position = "none")
print(p14)
ggsave("plots/custom_theme.png", p14, width = 10, height = 6, dpi = 300)

# 8. 保存所有图表的汇总 ====================================

# 创建一个包含所有图表文件名的列表
plot_files <- list.files("plots", pattern = "*.png", full.names = TRUE)

cat("✅ ggplot2 教学实例完成！\n")
cat("==============================\n")
cat("生成了以下图表文件：\n")
for (file in plot_files) {
  cat(sprintf("  • %s\n", basename(file)))
}
cat("\n总共生成图表数量:", length(plot_files), "\n")
cat("图表保存在目录: tutorial/ggplot2_example/plots/\n")
cat("R脚本保存在: tutorial/ggplot2_example/scripts/ggplot2_tutorial.R\n")

