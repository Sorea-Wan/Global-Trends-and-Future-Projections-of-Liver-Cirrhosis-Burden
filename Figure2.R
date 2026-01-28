#1.安装并加载R包
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("scales")
library(tidyverse)
library(ggplot2)
library(scales)

library(ggplot2)
regionrisk <- read.csv('C:\\Users\\86177\\Desktop\\肝硬化\\regionsrisk\\regionsrisk.csv') 
regionrisk$cause_name <- factor(regionrisk$cause_name,
                            c('Other causes','Alcohol use','Chronic hepatitis C','Chronic hepatitis B','NAFLD'))

p <- ggplot(regionrisk, aes(x = location_name, y = val, fill = cause_name)) +
  geom_col() +
  scale_y_continuous(labels = function(x) ifelse(x < 0, abs(x), x)) +
  xlab('Location') +
  ylab('Age-standardized Incidence Rate') +
  coord_flip() + 
  scale_fill_manual(values = c("NAFLD" = "#0D47A1", 
                               "Chronic hepatitis B" = "#1565C0",   # 绿色和橙色
                               "Chronic hepatitis C" = "#42A5F5", 
                               "Alcohol use" = "#64B5F6", 
                               "Other causes" = "#90CAF9")) +  # 使用自定义的蓝色调
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),  # 去掉主要网格线
        panel.grid.minor = element_blank(),  # 去掉次要网格线
        axis.text.x = element_text(angle = 30, hjust = 1, size = 6),  # 设置x轴标签旋转角度和字号
        axis.text.y = element_text(size = 6),  # 设置y轴标签字号
        axis.title.x = element_text(size = 8),  # 设置x轴标题字号
        axis.title.y = element_text(size = 8),  # 设置y轴标题字号
        legend.text = element_text(size = 5),  # 设置图例文本大小
        legend.title = element_text(size = 6))   # 设置图例标题大小
#2.加载数据
p



ggsave(p, file = 'C:\\Users\\86177\\Desktop\\肝硬化\\pic\\regionsrisk.tiff',units = 'cm', height = 6, width = 12)


