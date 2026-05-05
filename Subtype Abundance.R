# ==============================================================================
# 1. 环境准备与数据加载
# ==============================================================================
library(tidyverse)
library(ggraph)
library(igraph)
library(readxl)
library(scales)

# 修改为你自己的文件路径
file_path <- "F:/Global Samples/paper data/1-6.ARG Subtype/1280Sample-2261ARG Subtype-Total.xlsx"
df_raw <- read_excel(file_path)

# ==============================================================================
# 2. 数据清洗与结构构建
# ==============================================================================
df_clean <- df_raw %>%
    separate(Samples, into = c("Type", "Subtype"), sep = "__") %>%
    rename(Abundance = Total) %>%
    filter(!is.na(Abundance), Abundance > 0) %>%
    # 给一个微小的数值保底，确保坐标计算稳定
    mutate(Plot_Size = Abundance + 1e-7) %>% 
    mutate(Unique_ID = paste(Type, Subtype, sep = "_ID_"))

# 构建层级结构边表
edges <- rbind(
    data.frame(from = "Origin", to = unique(df_clean$Type)),
    df_clean %>% select(from = Type, to = Unique_ID)
)

# 构建节点属性表
vertices <- data.frame(name = unique(c(edges$from, edges$to))) %>%
    left_join(df_clean %>% select(Unique_ID, Type_Ref = Type, Plot_Size, Abundance, SubName = Subtype), 
              by = c("name" = "Unique_ID")) %>%
    mutate(
        # 核心逻辑：Subtype 继承其父类 Type 的名称作为颜色分组
        Color_Group = ifelse(is.na(Type_Ref), name, Type_Ref),
        Plot_Size = replace_na(Plot_Size, 0),
        SubName = ifelse(is.na(SubName), name, SubName)
    )

# 创建图形对象
graph_obj <- graph_from_data_frame(edges, vertices = vertices)

# ==============================================================================
# 3. 配色方案定制 (莫兰迪风格)
# ==============================================================================
unique_types <- unique(df_clean$Type)
type_count <- length(unique_types)

# 生成低饱和度、高辨识度的多色系 (莫兰迪调色盘)
# h: 色相范围, c: 饱和度(低), l: 亮度(中等偏高)
my_pal <- hue_pal(h = c(0, 360) + 20, c = 40, l = 65)(type_count)
names(my_pal) <- unique_types

# ==============================================================================
# 4. 绘图：Circle Packing
# ==============================================================================
arg_plot <- ggraph(graph_obj, layout = 'circlepack', weight = Plot_Size) + 
    # 绘制圆圈：映射颜色和透明度
    geom_node_circle(aes(
        fill = Color_Group, 
        alpha = as.factor(depth)
    ), color = "white", linewidth = 0.2) +
    
    # 透明度设置：大圆(1)极浅 0.1，小圆(2)稍深 0.65，使同色系产生层次感
    scale_alpha_manual(values = c("0" = 0, "1" = 0.1, "2" = 0.65), guide = "none") +
    
    # 颜色设置：应用自定义色板并剔除 Origin
    scale_fill_manual(
        values = my_pal, 
        breaks = unique_types, # 关键：只显示真实的 Type，不显示 Origin
        name = "ARG Types",
        na.value = "transparent", 
        guide = guide_legend(
            override.aes = list(alpha = 0.8, size = 1),
            ncol = 1
        )
    ) + 
    
    # Subtype 标签：仅显示丰度较高的子类，避免重叠
    geom_node_text(aes(
        label = ifelse(depth == 2 & Abundance > 2, SubName, NA)
    ), size = 1.6, color = "grey20", check_overlap = TRUE, fontface = "plain") +
    
    # 图形整体布局美化
    theme_void() +
    coord_fixed() +
    theme(
        legend.position = "right",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        plot.margin = margin(30, 30, 30, 30)
    )

# ==============================================================================
# 5. 高清输出渲染
# ==============================================================================
# 建议使用 TIFF 或 PNG 格式用于论文投稿
output_path <- "F:/Global Samples/paper data/1-6.ARG Subtype/ARG_Subtype_CirclePack_Final.png"

png(output_path, width = 2800, height = 2400, res = 300, bg = "white")
print(arg_plot)
dev.off()
       