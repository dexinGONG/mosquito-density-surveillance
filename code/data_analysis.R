
# 加载相关包 -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("readxl", "dplyr", "sp", "sf", "tidyr","formattable", "tmap",
         "ggplot2", "knitr", "purrr", "stringr", "kableExtra") # package names
pacman::p_load(pkgs, character.only = T)

# 读取数据
puning <- read_xlsx("data/2024年3月上半月至5月下半月登革热BI汇总(模拟).xlsx") %>%
  dplyr::rename(调查户数 =  `调查户数（户）`,
                阳性户数 = `阳性户数（户）`) %>%
  dplyr::mutate(Bi = round((阳性积水数/调查户数*100),2))

## 表1 普宁市2024年##月##月蚊媒密度状态情况表####
# 使用str_match来捕获“月”字前面的数字

# window_report <- "6月下半月" #报告窗口；与标题YAML中的title一致
matches <- stringr::str_match(window_report, "(\\d+)月")

# matches是一个列表，我们只需要第一个元素
month_extracted <- matches[[2]] %>% as.numeric()
last_month_extracted <- month_extracted - 1

# 提取报告窗口所在月的上一个月的BI数据
last_month_data <- puning %>% 
  dplyr::mutate(month_extracted = as.numeric(stringr::str_match(时间, "(\\d+)月")[,2])) %>%
  filter(month_extracted == last_month_extracted)  ##根据月份不同选择输入
Bi_last_month <- round(mean(last_month_data$Bi), 2)

# 提取报告窗口月的数据
month_data <- puning %>% 
  filter(时间 == window_report)  ##根据月份不同选择输入
Bi_month <- round(mean(month_data$Bi), 2)

# 使用mutate和case_when根据条件创建新变量
Bi_high_low_5 <-  ifelse(Bi_month > 5, "高", "低")
Bi_high_low_last_month <-  ifelse(Bi_month > Bi_last_month, "高", "低")


# 计算此前各月的BI
Bi_all_months <- puning %>% 
  dplyr::mutate(month_extracted = as.numeric(stringr::str_match(时间, "(\\d+)月")[,2])) %>%
  dplyr::group_by(month_extracted) %>% summarize(Bi = round(mean(Bi),2))

# 计算此前各半月的BI
Bi_half_months <- puning %>% 
  dplyr::mutate(month_extracted = as.numeric(stringr::str_match(时间, "(\\d+)月")[,2]),
                时间 = factor(时间)) %>%
  dplyr::group_by(时间) %>% summarize(Bi = round(mean(Bi),2))

# 使用ggplot2绘制折线图
plot_bi_half <- ggplot(Bi_half_months, aes(x = 时间, y = Bi, group = 1)) +
  geom_line(aes(color = "BI指数"), size = 3) +
  geom_point(size = 4, color = "red") +
  scale_color_manual(values = c("BI指数" = "lightblue")) +
  labs(title = "普宁市2024年各半月BI指数",
       x = "监测时间",
       y = "BI指数",
       color = "",
       hjust = 0.5, size = 20) +
  geom_hline(yintercept = 5, linetype = "dotted", color = "blue") + # 添加Y轴值为5的线
  geom_hline(yintercept = 10, linetype = "dotted", color = "blue") + # 添加Y轴值为10的线
  geom_hline(yintercept = 20, linetype = "dotted", color = "blue") + # 添加Y轴值为15的线
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 18),  # 增大X轴和Y轴标签的字体
        axis.title = element_text(size = 18),  # 增大X轴和Y轴标题的字体
        axis.ticks = element_blank(),  # 隐藏轴上的刻度
        panel.grid = element_blank(),  # 隐藏图表的网格线
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18),
        legend.key.size = unit(2.5, "cm"),
        legend.position = "none")

# 保存图表
ggsave("figs/bi_line_graph.png", plot = plot_bi_half, width = 18,height =15)


# 按照传播风险不同划分计算每个状态的风险点数和总风险点数
total_at_risk <- month_data %>%
  summarise(总风险点数 = n())
# 按照“蚊媒密度状态”分组，并计算每个状态的风险点数和百分比
status_counts <- month_data %>%
  group_by(蚊媒密度状态) %>%
  summarise(
    风险点数 = n(),
    百分比 = round((风险点数 / total_at_risk$总风险点数) * 100,2)
  ) %>%
  ungroup()
## markdown table
# kable(status_counts,format = "markdown",escape = FALSE)

# 创建风险类别
risk_levels <- c("高密度传播风险", "中密度传播风险", "低密度传播风险", "符合防控要求")

##年，月赋值
time_info <- month_data$时间
month <- head(time_info, 1)


## 表2 普宁市2024年##月##月布雷图指数监测情况汇总表####
# 计算BI指数，并新增蚊媒密度状态列
summary_data <- month_data %>%
  summarise(
    调查户数 = sum(调查户数, na.rm = TRUE),
    阳性户数 = sum(阳性户数, na.rm = TRUE),
    阳性积水数 = sum(阳性积水数, na.rm = TRUE),
    BI = round((sum(阳性积水数, na.rm = TRUE) / sum(调查户数, na.rm = TRUE)) * 100 ,2),  # 计算平均BI指数
    蚊媒密度状态 = case_when(
      BI < 5 ~ "符合防控要求",
      BI >= 5 & BI <= 10 ~ "低密度传播风险",
      BI >= 10 & BI <= 20 ~ "中密度传播风险",
      BI >= 20 ~ "高密度传播风险"
      # 如果有更高的BI值，可以继续添加条件
    )) %>% ungroup()

##赋值
total_households<-summary_data$调查户数
positive_households<-summary_data$阳性户数
positive_stagnant_water<-summary_data$阳性积水数
bi_index<-round(summary_data$BI,2)
mosquito_density_status<-summary_data$蚊媒密度状态
kable(summary_data,format = "markdown",escape = FALSE)



#### 风险等级点数、百分比、名称
# 创建一个空的数据框
risk_levels <- c("高密度传播风险", "中密度传播风险", "低密度传播风险", "符合防控要求")
risk_df <- tibble(蚊媒密度状态 = risk_levels, risk_sites = c()) %>%
  dplyr::mutate(蚊媒密度状态 = factor(蚊媒密度状态, levels = risk_levels))

#基于month_data，求出各风险等级的风险点数
# 总监测点数
total_bi_sites <- nrow(month_data) 

risk_sites <- month_data %>% group_by(蚊媒密度状态) %>%
  summarise(risk_sites = n()) %>% right_join(risk_df, by = "蚊媒密度状态") %>%
  dplyr::mutate(risk_sites = ifelse(is.na(risk_sites), 0, risk_sites),
                risk_percentage = round(risk_sites/sum(risk_sites),2) * 100)
# 各风险等级的测点数
high_risk_sites <- risk_sites[risk_sites$蚊媒密度状态 == "高密度传播风险", "risk_sites"]
medium_risk_sites <- risk_sites[risk_sites$蚊媒密度状态 == "中密度传播风险", "risk_sites"]
low_risk_sites <- risk_sites[risk_sites$蚊媒密度状态 == "低密度传播风险", "risk_sites"]
compliant_sites <-risk_sites[risk_sites$蚊媒密度状态 == "符合防控要求", "risk_sites"]

# 各风险等级的风险点数百分比
high_risk_percentage <- risk_sites[risk_sites$蚊媒密度状态 == "高密度传播风险", "risk_percentage"]
medium_risk_percentage <- risk_sites[risk_sites$蚊媒密度状态 == "中密度传播风险", "risk_percentage"]
low_risk_percentage <- risk_sites[risk_sites$蚊媒密度状态 == "低密度传播风险", "risk_percentage"]
compliant_percentage <- risk_sites[risk_sites$蚊媒密度状态 == "符合防控要求", "risk_percentage"]



# 列出高中低风险点名字

risk_name <- month_data %>% select(蚊媒密度状态,地区) %>% 
    dplyr::mutate(蚊媒密度状态 = factor(蚊媒密度状态, levels = risk_levels)) %>%
  dplyr::arrange(蚊媒密度状态)

# 创建一个函数来提取并格式化风险点名称
get_risk_names <- function(data, status_col, level, region_col) {
  data %>%
    filter(!!sym(status_col) %in% level) %>%
    select(!!sym(region_col)) %>%
    mutate(across(everything(), as.character)) %>%
    reduce(`[`) %>%
    (function(x) {
      if (length(x) > 1) {
        paste0(paste0(x[-length(x)], collapse = "、"), " 和 ", x[length(x)])
      } else {
        x
      }
    })
}
# 这个get_risk_names函数首先使用filter根据风险级别筛选数据，然后使用select选择地区
# 列，并确保所有列都是字符类型。接着，使用reduce将地区名称合并为一个向量，并通过一
# 个匿名函数来格式化这个向量，使其成为一个按中文习惯连接的字符串。最后，你可以使用
# 这个函数来为不同的风险级别生成地区名称字符串。

# 定义风险级别
risk_levels <- c("高密度传播风险", "中密度传播风险", "低密度传播风险", "符合防控要求")

# 应用函数来获取不同风险级别的地区名称
high_risk_name <- get_risk_names(month_data, "蚊媒密度状态", risk_levels[1], "地区")
medium_risk_name <- get_risk_names(month_data, "蚊媒密度状态", risk_levels[2], "地区")
low_risk_name <- get_risk_names(month_data, "蚊媒密度状态", risk_levels[3], "地区")
no_risk_name <- get_risk_names(month_data, "蚊媒密度状态", risk_levels[4], "地区")

# # 打印结果
high_risk_name <- ifelse(high_risk_sites == 0, "",paste0("，为：", high_risk_name))
medium_risk_name <- ifelse(medium_risk_sites == 0, "",paste0("，为：", medium_risk_name))
low_risk_name <- ifelse(low_risk_sites == 0, "",paste0("，分别为：", low_risk_name))


## 表3.1 普宁市疾控中心3月下半月BI监测点监测情况一览表####
# 筛选时间在“3月上半月”的记录
month_data_cdc <- month_data %>% 
  filter(乡镇或者普宁CDC=="普宁CDC") %>%##根据月份不同选择输入
  select(地区,调查户数,阳性户数,阳性积水数,Bi,蚊媒密度状态)
kable(month_data_cdc,format = "markdown", escape = FALSE)


## 表3.2 普宁市各乡（镇、街道、场）3月上半月常规监测情况表####
month_data_other<- month_data %>% 
  filter( !乡镇或者普宁CDC %in% "普宁CDC") %>% ##根据月份不同选择输入
  select(地区,调查户数,阳性户数,阳性积水数,Bi,蚊媒密度状态)%>%
  arrange(蚊媒密度状态)
knitr::kable(month_data_other,format ="markdown")


## 表4 普宁市疾控中心2024年3月下半月MOI监测情况统计表####
# 读取特定工作表

data_moi <- read_xlsx("data/MOI汇总(模拟).xlsx",sheet = "6月下半月") %>%
  dplyr::mutate(诱蚊诱卵指数 = round(诱蚊诱卵指数,2))
nmoi <- nrow(data_moi)
Ovitrap_index<-round(data_moi$诱蚊诱卵指数,2)
risk<-data_moi$蚊媒密度状态

# 应用函数来获取MOI不同风险级别的地区名称
moi_all_risk_name <- get_risk_names(data_moi, "蚊媒密度状态", risk_levels[1:4], "地区")
moi_high_risk_name <- get_risk_names(data_moi, "蚊媒密度状态", risk_levels[1], "地区")
moi_medium_risk_name <- get_risk_names(data_moi, "蚊媒密度状态", risk_levels[2], "地区")
moi_low_risk_name <- get_risk_names(data_moi, "蚊媒密度状态", risk_levels[3], "地区")
moi_no_risk_name <- get_risk_names(data_moi, "蚊媒密度状态", risk_levels[4], "地区")

# 应用函数来获取MOI不同风险级别的诱蚊诱卵指数MOI
moi_all_risk_moi <- get_risk_names(data_moi, "蚊媒密度状态", risk_levels[1:4], "诱蚊诱卵指数")
moi_high_risk_moi <- get_risk_names(data_moi, "蚊媒密度状态", risk_levels[1], "诱蚊诱卵指数")
moi_medium_risk_moi <- get_risk_names(data_moi, "蚊媒密度状态", risk_levels[2], "诱蚊诱卵指数")
moi_low_risk_moi <- get_risk_names(data_moi, "蚊媒密度状态", risk_levels[3], "诱蚊诱卵指数")
moi_no_risk_moi <- get_risk_names(data_moi, "蚊媒密度状态", risk_levels[4], "诱蚊诱卵指数")

# 描述
moi_risk_summary <- unique(data_moi$蚊媒密度状态)

knitr::kable(data_moi,format ="markdown")


