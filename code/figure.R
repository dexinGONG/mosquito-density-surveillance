
# 加载相关包 -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("readxl", "dplyr", "sp", "sf", "tidyr","formattable", "tmap",
         "ggplot2", "knitr", "purrr", "stringr", "kableExtra") # package names
pacman::p_load(pkgs, character.only = T)

puning <- read_xlsx("data/2024年3月上半月至5月下半月登革热BI汇总(模拟).xlsx") %>%
  dplyr::rename(调查户数 =  `调查户数（户）`,
                阳性户数 = `阳性户数（户）`,
                Bi = `Bi（布雷图指数）`)
##所有的"普侨镇"更改为"普宁华侨管理区"

month_data <- puning %>%
  filter(时间 == window_report) %>%
  mutate(乡镇 = str_replace(乡镇, "普侨镇", "普宁华侨管理区"))

guangdong <- st_read("data/map/puning_map.gpkg")%>%
  # dplyr::filter(district=="普宁市")%>%
  dplyr::rename("乡镇"="stree")%>%
  inner_join(month_data, by = "乡镇")%>%
  mutate(Bi分类 = case_when(
  Bi <= 5 ~ "符合防控要求",
  Bi > 5 & Bi <= 10 ~ "低密度传播风险",
  Bi > 10 & Bi <= 20 ~ "中密度传播风险",
  Bi > 20 ~ "高密度传播风险"))

#查看乡镇名称
unique(guangdong$乡镇)

bi_colors <- c(
  "符合防控要求" = "white",
  "低密度传播风险" = "green",
  "中密度传播风险" = "yellow",
  "高密度传播风险" = "red"
)

# 查看高中低风险个数
table(guangdong$Bi分类)

# 设定排列顺序
guangdong$Bi分类 <- factor(guangdong$Bi分类, levels = c("符合防控要求", "低密度传播风险", "中密度传播风险", "高密度传播风险"))

# 绘制地图
bi_map <- ggplot() +
  geom_sf(data = guangdong, aes(fill = Bi分类)) +
  scale_fill_manual(name = "蚊媒密度状态",
                    values = bi_colors,  # 确保这里的颜色和图例中的标签匹配
                    labels = c("符合防控要求", "低密度传播风险", "中密度传播风险", "高密度传播风险"),
                    drop = FALSE) +  # 确保所有图例项都显示
  geom_sf_text(data = guangdong, aes(label = 乡镇), 
               size = 5.5, color = "black", vjust = 1, nudge_x = 0, nudge_y = 0) +
  theme_minimal() +  # 使用简洁主题
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, "cm"),
    legend.position = "top"
  ) +
  labs(title = "2024年BI指数分布图")  # 确保标题正确

bi_map

ggsave("figs/bi_map.png",width = 18,height =15)
