library(readxl)
library(tibble)
library(pheatmap)
library(dplyr)
library(RColorBrewer)

# ==========================================
# 1. 读取数据
# ==========================================
# 注意：R语言中路径使用正斜杠 "/"
file_path <- "F:/Global Samples/paper data/1-5.ARG subtype Heatmap/02-4.all-Country-Code_Class.subtype-Richness-Average-Filter0.xlsx"
df_avg <- read_excel(file_path)

# ==========================================
# 2. 数据清理与矩阵准备
# ==========================================
if("WDI Location" %in% colnames(df_avg)) df_avg$`WDI Location` <- trimws(df_avg$`WDI Location`)
if("WDI Class" %in% colnames(df_avg)) df_avg$`WDI Class` <- trimws(df_avg$`WDI Class`)

# 确保 Country Code 列存在并清理空格
if("Country Code" %in% colnames(df_avg)) {
  df_avg$`Country Code` <- trimws(df_avg$`Country Code`) 
} else {
  stop("错误：在表格中未找到 'Country Code' 列，请检查新表格的表头名称。")
}

# 2.1 准备注释条 (用于显示顶部的颜色条)
# 使用 Country Code 作为行名
annotation_col <- df_avg %>% 
  select(`Country Code`, `WDI Class`) %>%    
  column_to_rownames("Country Code")         

# 2.2 准备数值矩阵 (用于绘图)
# 剔除 WDI Location, WDI Class, Country Code 之外的所有非数值列
# 注意：这里我们select剔除掉文字描述列，保留数值列
data_mat <- df_avg %>% 
  select(-`WDI Location`, -`WDI Class`) %>%  
  column_to_rownames("Country Code")         # 设为行名(横坐标)

# ==========================================
# 3. 筛选差异最大的前 200 个基因
# ==========================================
gene_vars <- apply(data_mat, 2, var, na.rm = TRUE)
top_200_genes <- names(sort(gene_vars, decreasing = TRUE)[1:min(200, length(gene_vars))])
selected_data <- data_mat[, top_200_genes]

# ==========================================
# 4. 标准化与聚类
# ==========================================
# Scale (Z-score) 并转置：行=基因，列=国家代码
plot_data_raw <- t(scale(selected_data)) 

# 计算基因聚类
row_dist <- dist(plot_data_raw) 
row_cluster <- hclust(row_dist, method = "ward.D2")

# 计算国家聚类
col_dist <- dist(t(plot_data_raw))
col_cluster <- hclust(col_dist, method = "ward.D2")

# ==========================================
# 5. 视觉截断 (Clip values at +/- 2)
# ==========================================
plot_data_visual <- plot_data_raw
cap_value <- 2  
plot_data_visual[plot_data_visual > cap_value] <- cap_value
plot_data_visual[plot_data_visual < -cap_value] <- -cap_value

# ==========================================
# 6. 配置颜色
# ==========================================
unique_classes <- unique(annotation_col$`WDI Class`)
n_classes <- length(unique_classes)

ann_colors_list <- list(
  `WDI Class` = setNames(colorRampPalette(brewer.pal(min(n_classes, 12), "Paired"))(n_classes), unique_classes)
)

# ==========================================
# 7. 绘图展示
# ==========================================
pheatmap(plot_data_visual,             
         cluster_rows = row_cluster,   
         cluster_cols = col_cluster,   
         annotation_col = annotation_col,      
         annotation_colors = ann_colors_list,
         annotation_names_col = FALSE, 
         
         # --- 坐标轴设置 ---
         show_colnames = TRUE,                # 显示国家代码
         show_rownames = FALSE,               # 不显示基因名
         fontsize_col = 8,                    # 字号
         angle_col = 90,                      # 竖直显示
         
         # --- 样式设置 ---
         scale = "none",                      
         clustering_method = "ward.D2",       
         color = colorRampPalette(c("#3C5488FF", "white", "#DC0000FF"))(100), 
         border_color = NA,                   
         main = "Global ARG Profile (Top 200 Subtypes)")