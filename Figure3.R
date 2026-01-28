library(tidyverse)
library(ggplot2)
library(scales)


#2.加载数据

a <- read.csv('C:\\Users\\86177\\Desktop\\肝硬化\\yesrrisk\\yearrisk.csv') 


#3.数据清洗（3.1字符串；3.2筛选行与列；3.3确定用于作图的变量内部的顺序）

##3.1 字符串（如1-4 year）预处理


# 数据准备
anumber <- a %>%
  select(location_name, year, sex_name, age_name, measure_name, cause_name, metric_name, val, upper, lower) %>%
  filter(location_name == "Global",
         age_name == "All ages",
         sex_name == "Both",
         measure_name == 'Incidence',
         cause_name != "Cirrhosis and other chronic liver diseases",
         metric_name == 'Number') %>%
  mutate(val = val / 1000000)
anumber$cause_name <- factor(anumber$cause_name, levels = c('NAFLD','Chronic hepatitis B','Chronic hepatitis C','Alcohol use','Other causes'))
arate <- a %>%
  select(location_name, year, sex_name, age_name, measure_name, cause_name, metric_name, val, upper, lower) %>%
  filter(location_name == "Global",
         age_name == "Age-standardized",
         sex_name == "Both",
         measure_name == 'Incidence',
         cause_name != "Cirrhosis and other chronic liver diseases",
         metric_name == 'Rate') %>%
  mutate(val = val / 12.5)
arate$cause_name <- factor(arate$cause_name, levels = c('NAFLD','Chronic hepatitis B','Chronic hepatitis C','Alcohol use','Other causes'))
plot <- ggplot() +
  scale_y_continuous(
    name = "Total Incidence Numbers（00,000s） ", 
    sec.axis = sec_axis(~ .*1/8, name = "Age-standardized Incidence rate(00s)"),
    breaks = seq(0, 64, by = 8), 
    limits = c(0, 64), expand = c(0,0))  +
  geom_col(data = subset(anumber, metric_name == "Number"), aes(x = year, y = val, fill = cause_name),  width = 0.6, position = "stack") + 
  geom_line(data = subset(arate, metric_name == "Rate"), aes(x = year, y = val, group = cause_name, color = cause_name), linetype = "solid", size = 1) +
  scale_fill_manual(name = "Number", values = c("NAFLD" = "#0D47A1", 
                                                "Chronic hepatitis B" = "#1565C0", 
                                                "Chronic hepatitis C" = "#42A5F5", 
                                                "Alcohol use" = "#64B5F6", 
                                                "Other causes" = "#90CAF9")) +
  scale_color_manual(name = "Rate", values =  c("NAFLD" = "#93445e", 
                                                "Chronic hepatitis B" = "#a5796b", 
                                                "Chronic hepatitis C" = "#527865", 
                                                "Alcohol use" = "#262626", 
                                                "Other causes" = "#B71C1C")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # 去掉主要网格线
        panel.grid.minor = element_blank(),  # 去掉次要网格线
        legend.position = 'right',
        legend.title = element_blank(),
        legend.key.size = unit(0.3, "cm"),
        axis.text = element_text(size=6),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1),
        axis.text.y.left = element_text(margin = margin(r = 0)),
        axis.text.y.right = element_text(margin = margin(r = 0)),
        axis.title = element_text(size = 6),
        axis.ticks = element_line()) +
  xlab('Age group (years)')
# 显示图表
print(plot)

#5.文件保存

ggsave(plot, file = 'C:\\Users\\86177\\Desktop\\肝硬化\\pic\\yearrisk.tiff',units = 'cm', height = 12, width = 24)


