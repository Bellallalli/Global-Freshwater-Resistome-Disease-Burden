# 加载必要的包
library(vegan)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

# 1. 设置工作路径并读取数据
setwd("F:/Global Samples/paper data/1-4.PCOA")
data_file <- "00.13-sample_normalized_cell.type.xlsx"

# 读取数据
library(readxl)
data <- read_excel(data_file)

# 2. 数据预处理
sample_names <- data$Sample
wdi_class <- data$`WDI Class`
arg_data <- data[, 3:ncol(data)]  # 从第3列开始是ARG type数据

# 转换为数值矩阵
arg_matrix <- as.matrix(arg_data)

# 检查数据并处理缺失值
if(any(is.na(arg_matrix))) {
  na_count <- sum(is.na(arg_matrix))
  cat("警告: 数据中有", na_count, "个缺失值，用0填充\n")
  arg_matrix[is.na(arg_matrix)] <- 0
}

# 检查数据是否全为0
row_sums <- rowSums(arg_matrix)
if(any(row_sums == 0)) {
  zero_rows <- sum(row_sums == 0)
  cat("警告: 有", zero_rows, "个样本的ARG type值全为0\n")
}

# 3. 计算Bray-Curtis距离矩阵
cat("\n计算Bray-Curtis距离矩阵...\n")
bc_dist <- vegdist(arg_matrix, method = "bray")

# 检查距离矩阵是否有NaN值
if(any(is.na(bc_dist))) {
  cat("警告: 距离矩阵中存在NaN值，检查数据\n")
}

# 4. 执行PCOA（主坐标分析）
cat("执行PCOA分析...\n")
pcoa_result <- cmdscale(bc_dist, k = 3, eig = TRUE, add = TRUE)

# 提取主坐标
pcoa_scores <- as.data.frame(pcoa_result$points)
colnames(pcoa_scores) <- c("PCoA1", "PCoA2", "PCoA3")

# 计算各轴解释的变异比例
eigenvalues <- pcoa_result$eig
# 只使用正的特征值
positive_eigenvalues <- eigenvalues[eigenvalues > 0]
total_variance <- sum(positive_eigenvalues)
variance_explained <- eigenvalues[1:3] / total_variance * 100

cat("PCoA1解释变异:", round(variance_explained[1], 2), "%\n")
cat("PCoA2解释变异:", round(variance_explained[2], 2), "%\n")
if(length(variance_explained) >= 3) {
  cat("PCoA3解释变异:", round(variance_explained[3], 2), "%\n")
}

# 5. 准备绘图数据
plot_data <- data.frame(
  Sample = sample_names,
  WDI_Class = wdi_class,
  PCoA1 = pcoa_scores$PCoA1,
  PCoA2 = pcoa_scores$PCoA2
)

if(ncol(pcoa_scores) >= 3) {
  plot_data$PCoA3 <- pcoa_scores$PCoA3
}

# 6. 为每个WDI Class分配颜色
unique_classes <- unique(plot_data$WDI_Class)
n_classes <- length(unique_classes)

# 自定义颜色方案（学术期刊常用配色）
custom_colors <- c(
    "SUB-SAHARAN AFRICA" = "#E41A1C",        # 红色
    "EAST ASIA & PACIFIC" = "#377EB8",       # 蓝色
    "EUROPE & CENTRAL ASIA" = "#4DAF4A",     # 绿色
    "LATIN AMERICA & CARIBBEAN" = "#984EA3", # 紫色
    "MIDDLE EAST & NORTH AFRICA" = "#FF7F00",# 橙色
    "NORTH AMERICA" = "#FFFF33",             # 黄色
    "SOUTH ASIA" = "#A65628"                 # 棕色
)

# 检查实际存在的类别并创建颜色映射
# 只保留数据中实际存在的类别
existing_colors <- custom_colors[names(custom_colors) %in% unique_classes]
color_mapping <- existing_colors

# 如果有自定义颜色中不包含的类别，使用备用颜色
if(length(color_mapping) < n_classes) {
  missing_classes <- setdiff(unique_classes, names(color_mapping))
  cat("以下类别不在自定义颜色中，将使用备用颜色:", paste(missing_classes, collapse = ", "), "\n")
  
  # 为缺失的类别添加备用颜色
  backup_colors <- c("#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")
  for(i in seq_along(missing_classes)) {
    color_mapping[missing_classes[i]] <- backup_colors[(i-1) %% length(backup_colors) + 1]
  }
}

# 显示颜色分配
cat("\n颜色分配:\n")
for(class in unique_classes) {
  cat(sprintf("%-30s: %s\n", class, color_mapping[class]))
}

# 7. 绘制基础PCOA图（圆点形式）
cat("\n绘制PCOA图...\n")

# 创建基础图形
pcoa_basic <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2, color = WDI_Class)) +
  
  # 使用圆点，所有点形状相同（shape = 16是实心圆点）
  geom_point(size = 3.0, alpha = 0.8, shape = 16) +
  
  # 设置颜色
  scale_color_manual(values = color_mapping, name = "Region") +
  
  # 坐标轴标签（包含解释的变异百分比）
  xlab(paste0("PCoA1 (", round(variance_explained[1], 2), "%)")) +
  ylab(paste0("PCoA2 (", round(variance_explained[2], 2), "%)")) +
  
  # 图形标题
  ggtitle("Principal Coordinates Analysis of ARG Types") +
  
  # 使用经典主题
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 10)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.7, "cm"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

# 显示图形
print(pcoa_basic)

# ---------------------------------------------------------
# 8. 执行 PERMANOVA 检验 (置换多元方差分析)
# ---------------------------------------------------------
cat("\n正在执行 PERMANOVA 检验 (Adonis)... \n")
cat("这可能需要一点时间，具体取决于置换次数 (permutations)...\n")

# 使用 adonis2 函数进行检验
# 公式为：距离矩阵 ~ 分组变量
permanova_result <- adonis2(bc_dist ~ WDI_Class, 
                            data = plot_data, 
                            permutations = 999, 
                            method = "bray")

# 打印 PERMANOVA 结果表格
cat("\n--- PERMANOVA 检验结果 ---\n")
print(permanova_result)

# 提取关键指标用于后续论文描述
r2_value <- permanova_result$R2[1]
p_value <- permanova_result$`Pr(>F)`[1]
f_statistic <- permanova_result$F[1]

cat("\n--- 关键统计指标摘要 ---\n")
cat(sprintf("解释度 (R2): %.4f (即地理区域解释了数据 %.2f%% 的差异)\n", r2_value, r2_value * 100))
cat(sprintf("F 统计量: %.2f\n", f_statistic))
cat(sprintf("P 值: %s\n", ifelse(p_value < 0.001, "< 0.001", round(p_value, 4))))