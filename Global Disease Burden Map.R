# 加载必要的包
library(tidyverse)
library(sf)
library(rnaturalearth)
library(readxl)

# 1. 读取数据
file_path <- "F:/Global Samples/paper data/3-1.social Model/0316/03.code_risk.xlsx"
risk_df <- read_excel(file_path)

# 强制重命名列名，确保代码稳健
colnames(risk_df)[1] <- "Code"
colnames(risk_df)[2] <- "pred"

# 2. 获取世界地图数据
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# 3. 合并数据
map_data <- world_sf %>%
  left_join(risk_df, by = c("iso_a3" = "Code"))

# 4. 绘制地图
p <- ggplot(data = map_data) +
  # 背景底色
  geom_sf(fill = "#f0f0f0", color = "white", size = 0.1) +
  # 风险值上色
  geom_sf(aes(fill = pred), color = "white", size = 0.1) +
  # 投影切换
  coord_sf(crs = "+proj=robin") +
  # --- 颜色调整 ---
  # 使用 gradientn 自定义多色阶过渡
  # "lightblue" 确保蓝色不深，逐渐过渡到中间的浅色，再到深红色
  scale_fill_gradientn(
    colors = c("#DEEBF7", "#9ECAE1", "#FC9272", "#DE2D26", "#A50F15"), 
    name = "Risk Value",
    trans = "log10", # 依然保留对数变换，应对 0.03-173 的跨度
    na.value = "#f0f0f0",
    breaks = c(0.1, 1, 10, 100),
    labels = c("0.1", "1", "10", "100")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right",      # 图注放在右侧
    legend.title = element_text(size = 10, face = "bold"),
    legend.key.height = unit(1.5, "cm"), # 调高侧边图注条
    panel.grid = element_blank()
  ) +
  labs(
    title = "Global Distribution of Predicted Risk",
    subtitle = "Color: Light Blue (Low) to Deep Red (High)",
    caption = "Source: 1281Sample-15factors-DALYs.xlsx"
  )

# 5. 显示并保存
print(p)