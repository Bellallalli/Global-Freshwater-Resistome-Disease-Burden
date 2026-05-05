# ============================================================
# 1. 加载必要的包
# ============================================================
library(readxl)
library(psych)
library(tidyverse)
library(reshape2)
library(scales)

# ============================================================
# 2. 读取数据
# ============================================================
# 请确保路径正确，注意反斜杠方向
file_path <- "F:/Global Samples/paper data/2-2.Pathogen-ARG Pearson/00.sample-ARG type-pathogen-1281-Plot.xlsx"
data <- read_excel(file_path)

# ============================================================
# 3. 数据预处理
# ============================================================
# 提取 ARG (4-32列) 并计算 Total_ARG
# 注意：保留 Total_ARG 作为环境耐药负荷的指示指标
arg_data <- data[, 4:32]
arg_data$Total_ARG <- rowSums(arg_data)

# 提取 Pathogen (33列到最后)
pathogen_data <- data[, 33:ncol(data)]

# ============================================================
# 4. 计算 Spearman 相关性
# ============================================================
# method="spearman": 适用于非正态分布的微生物数据
# adjust="BH": 控制错误发现率(FDR)，这对大样本多重比较至关重要
cor_result <- corr.test(arg_data, pathogen_data, method = "spearman", adjust = "BH")

# 提取相关系数 r 和校正后的 p 值
r_val <- cor_result$r
p_val <- cor_result$p

# ============================================================
# 5. 格式转换：从矩阵转为长表格
# ============================================================
r_long <- melt(r_val, varnames = c("ARG", "Pathogen"), value.name = "Correlation")
p_long <- melt(p_val, varnames = c("ARG", "Pathogen"), value.name = "Pvalue")
plot_data <- left_join(r_long, p_long, by = c("ARG", "Pathogen"))

# ============================================================
# 6. 核心过滤步骤：双重阈值筛选
# ============================================================
# 规则：P < 0.05 (显著性) 且 |r| >= 0.4 (生物学效应量)
significant_data <- plot_data %>% 
  filter(Pvalue < 0.05) %>%
  filter(abs(Correlation) >= 0.4) %>% 
  mutate(Significance = case_when(
    Pvalue < 0.001 ~ "***",
    Pvalue < 0.01 ~ "**",
    Pvalue < 0.05 ~ "*",
    TRUE ~ ""
  ))

# 统计并打印过滤后的结果，确认数据量
cat("过滤噪音(|r|>=0.4)后，剩下的强相关组合数量为：", nrow(significant_data), "\n")

# ============================================================
# 7. (新增) 打印 Top 20 强相关组合的详细数据
# ============================================================
# 按相关系数绝对值排序，把最强的关联打印出来供论文引用
sorted_stats <- significant_data %>%
  arrange(desc(abs(Correlation))) %>% 
  select(ARG, Pathogen, Correlation, Pvalue, Significance)

cat("\n>>> Top 20 强相关组合 (Correlation r) 数据预览：\n")
cat("------------------------------------------------------\n")
for(i in 1:min(20, nrow(sorted_stats))) {
  row <- sorted_stats[i, ]
  # 格式输出：ARG - Pathogen : r = 0.xx
  cat(sprintf("%s - %s : r = %.3f (P = %.2e %s)\n", 
              row$ARG, row$Pathogen, row$Correlation, row$Pvalue, row$Significance))
}
cat("------------------------------------------------------\n")

# 可选：将所有数据保存以便查阅
# write.csv(sorted_stats, "Significant_Correlations_List.csv", row.names = FALSE)

# ============================================================
# 8. 准备绘图数据：选取 Top 30 病原体
# ============================================================
top_pathogens_list <- significant_data %>%
  group_by(Pathogen) %>%
  summarise(max_cor = max(abs(Correlation))) %>%
  arrange(desc(max_cor)) %>%
  slice(1:30) %>%
  pull(Pathogen)

final_plot_data <- significant_data %>%
  filter(Pathogen %in% top_pathogens_list)

# ============================================================
# 9. 绘制气泡图 (Bubble Plot)
# ============================================================
p <- ggplot(final_plot_data, aes(x = ARG, y = Pathogen)) +
  # 气泡大小对应相关强度，颜色对应正负方向
  geom_point(aes(size = abs(Correlation), color = Correlation)) +
  # 添加显著性星号
  geom_text(aes(label = Significance), vjust = 0.75, color = "black", size = 3.5) +
  # 设置红蓝双色渐变，中间为白色
  scale_color_gradient2(
    low = "#0571b0", mid = "white", high = "#ca0020", 
    midpoint = 0, limit = c(-0.8, 0.8), # 限制颜色范围提升对比度
    oob = squish 
  ) +
  # 学术化主题设置
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, face = "italic", color = "black"), # 细菌学名斜体
    panel.grid.major = element_line(color = "gray95"),
    panel.border = element_rect(colour = "black", fill=NA, size=1.2),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  labs(
    title = "Spearman Correlation: ARGs vs Pathogens",
    subtitle = "Dual-threshold: |r| >= 0.4 and P < 0.05 (BH adjusted)",
    x = "ARG Types",
    y = "Selected Pathogen Species",
    size = "Effect Size |r|",
    color = "Spearman rho"
  )

# ============================================================
# 10. 显示图像
# ============================================================
print(p)
ggsave("F:/Global Samples/paper data/2-2.Pathogen-ARG Pearson/fig3b_0503.jpg", p, width = 10, height = 5, dpi = 300)