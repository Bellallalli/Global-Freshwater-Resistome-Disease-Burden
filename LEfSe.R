library(microeco)
library(readxl)
library(magrittr)
library(ggplot2)

# ==========================================
# 1. 数据准备与对象构建
# ==========================================
df <- read_excel("F:/Global Samples/paper data/2-7.LEfSE/1281Sample-7Region-352Pathogen-Plot.xlsx")
group_var <- "WDI Class"
pathogen_names <- colnames(df)[3:ncol(df)]

# 构建丰度表 (otu_table)
otu_table <- df[, pathogen_names] %>% t() %>% as.data.frame()
colnames(otu_table) <- paste0("Sample_", 1:nrow(df))

# 构建样本信息表 (sample_table)
sample_info <- data.frame(
  SampleID = colnames(otu_table),
  Group = df[[group_var]],
  row.names = colnames(otu_table),
  stringsAsFactors = FALSE
)

# 构建分类表 (tax_table)
tax_table <- data.frame(
  Kingdom = "Pathogen",
  Species = pathogen_names,
  row.names = pathogen_names,
  stringsAsFactors = FALSE
)

# 创建 microtable 对象
dataset <- microtable$new(
  otu_table = otu_table, 
  sample_table = sample_info, 
  tax_table = tax_table
)

# 预处理
dataset$tidy_dataset()
dataset$cal_abund() 

# ==========================================
# 2. 执行 LEfSe 分析
# ==========================================
lefse <- trans_diff$new(
  dataset = dataset,
  method = "lefse",
  group = "Group",
  alpha = 0.05,
  p_adjust_method = "fdr",
  taxa_level = "Species", 
  lda_cutoff = 2.0  
)

# ==========================================
# 3. 结果筛选与最终绘图 (仅保留优化版)
# ==========================================

# 严格筛选：只保留绝对值 LDA >= 3.5 的物种
lefse$res_diff <- lefse$res_diff[abs(lefse$res_diff$LDA) >= 3.5, ]
final_count <- nrow(lefse$res_diff)
message("最终绘制物种数量: ", final_count)

# 自定义配色方案
my_colors <- c("#9ecae1", "#f1948a", "#a5d6a7", "#bb8fce", "#f5b7b1", "#85c1e9")

# 绘图逻辑
g <- lefse$plot_diff_bar(
    use_number = 1:final_count, 
    width = 0.7,
    color_values = my_colors 
) +
    theme_bw() +
    labs(x = "", y = "LDA score") +
    theme(
        axis.text.y = element_text(size = 9, face = "italic", color = "black"), 
        axis.text.x = element_text(size = 10, color = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank() 
    )

# 显示图表
print(g)

# ==========================================
# 4. 结果保存 (TIFF 高清格式)
# ==========================================
ggsave(
  filename = "F:/Global Samples/paper data/2-7.LEfSE/lefse.tif", 
  plot = g,
  device = "tiff",
  width = 8, 
  height = 6,               # 建议根据物种数量调整高度，如果物种多就设大一点
  units = "in",
  dpi = 600, 
  compression = "lzw"
)