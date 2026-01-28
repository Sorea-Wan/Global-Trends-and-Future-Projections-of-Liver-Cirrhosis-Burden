# 健康不公平的可视化
# 2022-12-08

# 加载需要的包 ------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(car)# 异方差诊断
library(MASS)# 稳健回归
# install.packages("devtools")
# devtools::install_github("nicolash2/ggbrace")
library(ggbrace)# 画括号
library(mgcv)# 提供洛伦兹曲线拟合 (样条函数等)
library(splines)# 拟合样条函数
library(broom)
library(grid)
# 数据准备--------------------------------------------------------------------
# 读取并合并三个数据
# 疾病负担数据
burden <- read.csv("C:\\Users\\86177\\Desktop\\不平等\\不平等.csv",header = T)
burden <- burden %>%
  filter(age_name!="Age-standardized",
         measure_name == "Incidence") %>%
  rename(location = location_name)
# sdi 数据
sdi <- read.csv('C:\\Users\\86177\\Desktop\\不平等\\ids.csv')
list <- read.csv('C:\\Users\\86177\\Desktop\\不平等\\countrylist.csv')  
sdi <- sdi %>%
  filter(location %in% list$Country) %>%
  rename(sdi = mean_value) %>%
  dplyr::select(location, year, sdi)

# 将年份列转换为整数类型
sdi$year <- as.integer(sdi$year)

# 匹配 sdi 与疾病负担, 生成 data
daa <- left_join(burden,sdi,by=c("location","year"))
names(daa)[4]<-"location"
names(daa)[6]<-"sex"

# 注意一个国家 Georgia
# 读取人口数据
path = "C:\\Users\\86177\\Desktop\\不平等\\pop"
fileName = dir(path)
fileName
population <- data.frame()

#population<-data.frame()
for(k in 1:length(fileName)){
  data = read.csv(file = paste(path,fileName[k],sep = "\\"),
                  header = T,stringsAsFactors = F)
  population=rbind(population,data)
}


pop1 <- population %>%
  dplyr::select(location_name,sex_name,age_name,year,metric_name,val)

unique(pop1$age)
pop1 <- pop1 %>%
  filter(age_name=="All ages") %>%
  dplyr::select("location_name","sex_name","year","val") %>%
  rename(pop=val)
names(pop1)[1]<-"location"
names(pop1)[2]<-"sex"
# 合并人口数据, 生成 mydata
mydata <- left_join(daa,pop1,
                    by=c("location","sex","year"))


# 斜度指数的可视化 ----------------------------------------------------------------

## 1.绘图数据的准备 -----------------------------------------------------------------
# 计算总人口
a <- mydata %>%
  filter(metric_name == "Number") %>%
  group_by(year) %>%
  summarise(sum = sum(pop, na.rm = TRUE))
pop1990 <- a$sum[1]
pop2019 <- a$sum[2]
# 计算加权次序
rank <- mydata %>%
  mutate(pop_global=ifelse(year==1990,pop1990,pop2019)) %>%
  group_by(year,metric_name) %>%
  arrange(sdi) %>%
  mutate(cummu=cumsum(pop)) %>% # 计算累积人口
  mutate(half=pop/2) %>% # 计算该国家人口的一半
  mutate(midpoint=cummu-half) %>% # 累积人口减去该国家人口一半即为人口中点
  mutate(weighted_order=midpoint/pop_global) # 人口中点与总人口相比即为改国的相对位置
# 把年份设置为 factor
rank$year <- factor(rank$year)
# 选择数据
temp1 <- rank %>%
  filter(metric_name=="Rate") %>%
  filter(year==1990)
temp2 <- rank %>%
  filter(metric_name=="Rate") %>%
  filter(year==2021)
# 建模计算斜度指数
fit1 <- lm(data = temp1,val~weighted_order)
fit2 <- lm(data = temp2,val~weighted_order)
# 查看是否存在异方差（存在异方差）
ncvTest(fit1)
ncvTest(fit2)
# 使用稳健（robust）回归：重复迭代加权
r.huber1 <- rlm(data = temp1,val~weighted_order)
r.huber2 <- rlm(data = temp2,val~weighted_order)
# 获得系数与截距
coef(r.huber1)
coef(r.huber2)
# 计算稳健回归的 95% 可信区间
confint.default(r.huber1)
confint.default(r.huber2)

# 2.绘图 ----------------------------------------------------------------------
color <- c("#6699FF","#990000")
colnames(rank)
p1 <- rank %>%
  filter(metric_name=="Rate") %>%
  ggplot(aes(x=weighted_order,y=val,fill=year,group=year,color=year))+
  geom_point(aes(color=year,size=pop/1e6),alpha=0.8,shape=21)+
  scale_size_area("Population\n(million)",breaks=c(300,600,900,1200,1500,1800))+
  geom_smooth(method = "rlm",size=1,alpha=0.1)+
  scale_fill_manual(values = color)+
  scale_color_manual(values = color)+
 
  # 增加中括号：中括号的位置需要提前确定，根据截距与斜率
  #stat_brace(aes(x = c(1.003,1.103), y = c(87548,87548-70136)), size = 0.6, rotate = 90, color = "#6699FF") +
  #stat_brace(aes(x = c(1,1.1), y = c(40734,40734-13805)),  size = 0.6, rotate = 90, color = "#990000")+
  #geom_brace(aes(x=c(1.003,1.103),y=c(87548,87548-70136)),
             #inherit.data = F,size=0.6,
             #rotate=90,color="#6699FF")+
  #geom_brace(aes(c(1,1.1),c(40734,40734-13805)),
             #inherit.data = F,size=0.6,
             #rotate=90,color="#990000")+
  # 增加水平虚线
  #geom_segment(x=0.02,xend=0.99,
               #y=1494.3539 ,yend=1494.3539 , # 截距的位置
               #color="#6699FF",linetype=2,size=0.4,alpha=0.4)+
  #geom_segment(x=0.02,xend=0.99,
               #y=1672.792,yend=1672.792, # 截距的位置
               #color="#990000",linetype=2,size=0.4,alpha=0.4)+
  # 增加某些国家的标签: 比如中国与印度
  geom_text(aes(label=ifelse(location=="China"|location=="India",as.character(location),""),
                color=year),
            hjust=0,vjust=1.7,# 避免点和文字重合
            size=5)+
  # 增加斜度指数标签
  
  #annotate("text",label="Slope Index of Inequality",x=1.22,y=40670+6751/2,size=4,angle=90)+
  annotate("text",label="-305.71",x=1.15,y=400,size=6,color="#6699FF")+ # 位置
  annotate("text",label="-187.67",x=1.15,y=600,size=6,color="#990000")+
  scale_x_continuous(limits = c(0,1.22),labels = c("0","0.25","0.50","0.75","1.00",""))+
  xlab("Relative rank by SDI")+
  ylab("Crude Incidence rate (per 100,000)")+
  theme_bw()

p1 <- p1 +
  theme_bw(base_size = 16) +   # 整体字体基准放大（推荐 16–18）
  theme(
    # 去掉背景网格线
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 坐标轴
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    
    # 图例
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 14),
    
    # 图例位置（可选）
    legend.position = "right"
  )
p1

ggsave(p1, file = 'C:\\Users\\86177\\Desktop\\不平等\\incidence1.tiff',units = 'cm', height = 15, width = 22)

# 集中指数的可视化 ----------------------------------------------------------------

# 1.绘图数据准备 ------------------------------------------------------------------
a <- mydata %>%
  filter(metric_name=="Number") %>%
  group_by(year) %>%
  summarise(sum=sum(val))
daly1990 <- a$sum[1]
daly2019 <- a$sum[2]

ci <- rank %>%
  filter(metric_name=="Number") %>%
  mutate(total_daly=ifelse(year==1990,daly1990,daly2019)) %>%
  group_by(year) %>%
  arrange(sdi) %>%
  mutate(cummu_daly=cumsum(val)) %>% # 计算累积 daly
  mutate(frac_daly=cummu_daly/total_daly) %>% # 计算累积 daly 所占总体的比例
  mutate(frac_population=cummu/pop_global) # 计算累积人口所占总体人口的比例
#####计算ci
# 选择数据
temp3 <- ci %>%
  filter(metric_name=="Number") %>%
  filter(year==1990)
temp4 <- ci %>%
  filter(metric_name=="Number") %>%
  filter(year==2021)
##计算集中指数
CI_1990 <- 2 * (sum(temp3$frac_daly) / nrow(temp3)) - 1

CI_2021 <- 2 * (sum(temp4$frac_daly) / nrow(temp4)) - 1

theme_pub <- theme_bw(base_size = 16) +
  theme(
    # 去掉背景网格线
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 坐标轴
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    
    # 图例
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 14),
    legend.position = "right",
    
    # 边框稍微加粗（期刊友好，可选）
    panel.border = element_rect(size = 0.8)
  )
p2 <- ci %>%
  ggplot(aes(x=frac_population,y=frac_daly,
             fill=year,color=year,group=year))+
  
  geom_segment(x=0,xend=1,y=0,yend=0,
               size=1,color="gray")+
  geom_segment(x=1,xend=1,y=0,yend=1,
               size=1,color="gray")+
  
  geom_segment(x=0,xend=1,y=0,yend=1,
               color="#CD853F",size=0.7)+
  
  geom_point(aes(fill=year,size=pop/1e6),
             alpha=0.75,shape=21)+
  
  scale_fill_manual(values = color)+
  scale_color_manual(values = color)+
  scale_size_area("Population\n(million)",
                  breaks=c(300,600,900,1200,1500,1800))+
  
  geom_smooth(
    method = "gam",
    formula = y ~ ns(x,
                     knots = c(1e-10,0.25,0.5,0.75,0.9999999),
                     Boundary.knots = c(0,1)),
    size=0.8, alpha=0.6, se=TRUE
  )+
  
  annotate("text",label="Concentration Index",
           x=0.75,y=0.35,size=6)+
  annotate("text",label="1990: -0.14",
           x=0.75,y=0.25,size=5,color="#6699FF")+
  annotate("text",label="2021: -0.10",
           x=0.75,y=0.15,size=5,color="#990000")+
  
  geom_text(aes(label=ifelse(location %in% c("China","India") & year=="1990",
                             location,"")),
            hjust=-0.6,vjust=0.8,size=5)+
  
  geom_text(aes(label=ifelse(location %in% c("China","India") & year=="2021",
                             location,"")),
            hjust=1.8,vjust=0,size=5)+
  
  xlab("Cumulative fraction of population ranked by SDI")+
  ylab("Cumulative fraction of Incidence")+
  
  theme_pub
p2
ggsave(p2, file = 'C:\\Users\\86177\\Desktop\\不平等\\incidence2.tiff',units = 'cm', height = 15, width = 22)
