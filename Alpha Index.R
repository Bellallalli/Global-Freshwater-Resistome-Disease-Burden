# 加载必要的包
library(readxl)
library(vegan)
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. 读取数据（请根据实际路径修改）
file_path <- "F:/Global Samples/paper data/1-7.ARGs Alpha/1280Sample-Group-2261ARG Subtype.xlsx"
data <- read_excel(file_path)

# 2. 提取丰度矩阵（从第3列到最后一列，第一列为样本名，第二列为分组）
arg_matrix <- as.data.frame(data[, 3:ncol(data)])
rownames(arg_matrix) <- data[[1]]

# 3. 计算 Alpha 多样性指数
alpha_results <- data.frame(
  Sample = data[[1]],
  Group = data[[2]],
  Shannon = diversity(arg_matrix, index = "shannon"),
  Simpson = diversity(arg_matrix, index = "simpson"),
  Richness = specnumber(arg_matrix)   # 观测到的ARG亚型数
)

# 4. 添加 Pielou 均匀度 (Shannon / ln(Richness))
#    安全处理：当 Richness <= 1 时，Pielou 设为 NA（避免除零错误）
alpha_results <- alpha_results %>%
  mutate(Pielou = ifelse(Richness > 1, Shannon / log(Richness), NA))

# 5. 转换数据为长格式
plot_data <- alpha_results %>%
  pivot_longer(cols = c(Shannon, Simpson, Richness, Pielou),
               names_to = "Index", 
               values_to = "Value")

# 6. 设置指数显示顺序（丰富度 → 香农 → 辛普森 → 均匀度）
plot_data$Index <- factor(plot_data$Index, 
                          levels = c("Richness", "Shannon", "Simpson", "Pielou"))

# 7. 绘制箱线图 + 抖动点（分两列展示）
p <- ggplot(plot_data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8, size = 0.5) + 
  geom_jitter(aes(color = Group), width = 0.2, size = 1.2, alpha = 0.6, show.legend = FALSE) +
  facet_wrap(~Index, scales = "free_y", ncol = 2) +   # 两列布局
  theme_bw() +
  labs(title = "Alpha Diversity of ARGs", 
       x = NULL, 
       y = "Index Value", 
       fill = "Region") +
  theme(
    axis.text.x = element_blank(),        # 隐藏x轴标签
    axis.ticks.x = element_blank(),       # 隐藏x轴刻度线
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.background = element_rect(fill = "gray96"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()  # 去掉纵向网格线
  )

# 8. 显示图形
print(p)

# 9. 保存图片（可根据需要调整格式和尺寸）
ggsave("Alpha_Diversity_Four_Indices.pdf", plot = p, width = 8, height = 6, dpi = 300)
# 若需要 TIFF 格式：
# ggsave("Alpha_Diversity_Four_Indices.tiff", plot = p, width = 8, height = 6, dpi = 300, compression = "lzw")

ggsave("F:/Global Samples/paper data/1-7.ARGs Alpha/Alpha_4Index.png", plot = p, width = 9, height = 7, dpi = 600)