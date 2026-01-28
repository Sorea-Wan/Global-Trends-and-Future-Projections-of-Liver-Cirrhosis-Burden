library(ggplot2)
library(dplyr)

# 假设您的数据框是bapc，并且已经加载了数据
bapc <- read.csv("C:\\Users\\86177\\Desktop\\1.csv")
bapc <- bapc %>%
  mutate(
    mean = mean * 100000,
    sd = sd * 100000
  )
bapc <- bapc %>%
  mutate(causes = factor(causes, levels = c("Cirrhosis","Chronic hepatitis B", "Chronic hepatitis C", "Alcohol use", "NAFLD", "Other causes")))

# 定义填充颜色
fill_colors <- c(
  "Cirrhosis" = "#AFAFAF",
  "Chronic hepatitis B" = "#AFAFAF",  # both 用灰色
  "Chronic hepatitis C" = "#AFAFAF",  # male 用灰色
  "Alcohol use" = "#AFAFAF",
  "NAFLD" = "#AFAFAF",
  "Other causes" = "#AFAFAF"# female 用灰色
)

# 定义线条颜色
line_colors <- c(
  "Cirrhosis" = "#FFD0E9",
  "Chronic hepatitis B" = "#8ccdbf",  # both 用浅红
  "Chronic hepatitis C" = "#cde0a5",  # male 用浅蓝
  "Alcohol use" = "#f9db95",
  "NAFLD" = "#ef8476",
  "Other causes" = "#c5a8ce"# female 用浅紫
)

# 创建图表
pbapc <- ggplot(bapc) +
  aes(x = Year, y = mean, colour = causes) +
  # 误差线置于底层，按 causes 填充不同颜色
  geom_ribbon(
    aes(ymin = mean - sd, ymax = mean + sd, fill = causes),
    alpha = 0.3,  # 设置透明度
    show.legend = FALSE  # 不显示填充颜色的图例
  ) +
  scale_fill_manual(values = fill_colors) +  # 应用填充颜色
  # 2021年以前用实线图（observed），并使其更光滑
  geom_line(
    data = filter(bapc, Year <= 2021),
    size = 1,  # 加粗线条
    alpha = 0.8,  # 设置透明度
    lineend = "round",  # 使线条更光滑
    linejoin = "round"  # 使线条连接处更光滑
  ) +
  # 2021年以后用虚线图（predicted）
  geom_line(
    data = filter(bapc, Year > 2021),
    size = 1,  # 加粗线条
    linetype = "dashed",  # 虚线
    alpha = 0.8  # 设置透明度
  ) +
  # 2021年处加黑色虚线
  geom_vline(
    xintercept = 2021,
    linetype = "dashed",
    color = "black",
    size = 1  # 加粗虚线
  ) +
  scale_color_manual(values = line_colors) +  # 应用线条颜色
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    text = element_text(family = "Arial", size = 22),  # 全局字体设置
    axis.title.x = element_text(size = 24, face = "bold"),  # X 轴标题字体大小和加粗
    axis.title.y = element_text(size = 24, face = "bold"),  # Y 轴标题字体大小和加粗
    axis.text.x = element_text(size = 22),  # X 轴刻度字体大小
    axis.text.y = element_text(size = 22),  # Y 轴刻度字体大小
    strip.text = element_text(size = 22, face = "bold"),  # 分面图标题字体大小和加粗
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    legend.position = "none"  # 不显示图例
  ) +
  labs(
    x = "Year",  # X 轴标题
    y = "Age-standardized Incidence Rate"  # Y 轴标题
  ) +
  facet_wrap(~ causes, ncol = 2, scales = "free_y")  # 按 causes 分面，不共用纵坐标

# 显示图表
print(pbapc)
# 保存图表
ggsave(pbapc, file = 'C:\\Users\\86177\\Desktop\\bapccauses.tiff', units = 'cm', height = 24, width = 48)
