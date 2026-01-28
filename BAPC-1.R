
#包的安装
install.packages("remotes")
install.packages("foreach")
##### 下载后安装nordpred、INLA包，在本地安装


install.packages("cmprsk")
install.packages("fanplot")
install.packages("Epi")
install.packages("caTools")
install.packages("BAPC", repos="http://R-Forge.R-project.org")
install.packages("sp")

library(BAPC)
library(openxlsx)
library(INLA)
library(nordpred)
library(reshape)
library(data.table)
library(tidyr)
library(tidyverse)
library(epitools)
library(ggplot2)

inla.upgrade() # for the stable version

# 1990-2019年人口学数据

# 发病数据需要的年龄分层
age1 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")  


#### 调取标准人口百分比用
ages_2 <- c("<1 year","1 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
            "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
            "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
            "90 to 94", "95 plus")

####  预测的年龄结构
ages_3 <- c("0 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
            "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
            "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
            "90 to 94", "95 plus")

# 标准年龄结构数据age_stand
age_stand <- read.csv("C:\\Users\\86177\\Desktop\\age_stand.csv")
#标准构成比
wstand<-age_stand$std_population %>% as.numeric()/sum(age_stand$std_population) 
wstand <- c(age_stand$std_population[1:2] %>% as.numeric() %>% sum(),
            age_stand$std_population[3:21] %>% as.numeric())/sum(age_stand$std_population[1:21])
# 修改 age 列的格式
# 修改列名

sum(wstand)





IBD_china <- read.csv('C:\\Users\\86177\\Desktop\\bapccauses.csv')
########Both#######
####Incidence####
#####先处理发生人数数据#####
IBD_in_both<- subset(IBD_china,
                     (IBD_china$age_name %in% age1 ) &
                       IBD_china$sex_name=="Both"&
                       IBD_china$location_name=='Global'&
                       IBD_china$cause_name=='NAFLD'&
                       IBD_china$metric_name== 'Number' &
                       IBD_china$measure_name=='Incidence') #这些指标可以改

unique(IBD_in_both$age_name)
IBD_in_both$age_name<-gsub(" years","",IBD_in_both$age_name)
IBD_in_both$age_name <- factor(IBD_in_both$age_name, levels = c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                                                "90-94", "95+"))

# 提取数据Measure_name,age_name,year,val
IBD_in_both <- IBD_in_both[,c("measure_name", "age_name","year","val")]

#长转宽
IBD_in_both_n <- reshape2::dcast(data=IBD_in_both, year ~ age_name, value.var="val")
rownames(IBD_in_both_n) <- IBD_in_both_n$year
IBD_in_both_n <- IBD_in_both_n[,-1]


#IBD_in_both_n <- apply(IBD_in_both_n,c(1,2),as.integer) %>% as.data.frame
####含义####
IBD_in_both_n <- apply(IBD_in_both_n,c(1,2),round) %>% as.data.frame



####人口数据#####
###人口数据
age2 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")   ###20个年龄

var_name <- c("location_name", "sex_name", "year", "age_name", "val") 

GBD_population  <-  data.frame()
path = "C:\\Users\\86177\\Desktop\\Population\\"
fileName = dir(path)
fileName
GBD_population <- data.frame()

#population<-data.frame()
for(k in 1:length(fileName)){
  data = read.csv(file = paste(path,fileName[k],sep = "\\"),
                  header = T,stringsAsFactors = F)
  GBD_population=rbind(GBD_population,data)
}
GBD_population<-GBD_population%>% dplyr::select(var_name) %>% 
  filter(location_name %in% 'Global' & age_name %in% age2 )

# write.csv(GBD_population,file = "GBD_population.csv")
# GBD_population<-read.csv("GBD_population.csv")
GBD_population$age_name<-gsub(" years","",GBD_population$age_name)

#GBD_population <- GBD_population[!duplicated(GBD_population),]
#GBD_Both_population<- subset(GBD_population,GBD_population$sex_name =="Both")
#GBD_Female_population<- subset(GBD_population,GBD_population$sex_name =="Female")
GBD_Male_population<- subset(GBD_population,GBD_population$sex_name =="Both")





GBD_population_prediction <- fread("C:\\Users\\86177\\Desktop\\population_both.csv") 
GBD_5year <- GBD_population_prediction %>% 
  filter(age_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","1 to 4")) %>%
  group_by(location_name,sex_name,year) %>% 
  summarise(val=sum(val)) %>%
  mutate(age_name="<5")

## 再将 "Early Neonatal","Late Neonatal", "Post Neonatal","1 to 4"数据去除,加上<5 year的人口学数据
GBD_population_prediction <- GBD_population_prediction %>% filter(!(age_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","All Ages","1 to 4"))) %>%
  rbind(GBD_5year)
GBD_population_prediction$age_name<-gsub(" to ","-",GBD_population_prediction$age_name)
GBD_population_prediction$age_name<-gsub(" plus","+",GBD_population_prediction$age_name)

unique(GBD_population_prediction$age_name)

#colnames(GBD_population_prediction)<-var_name
#####合并人口学数据1990-2036#####
GBD <- rbind(GBD_Male_population, GBD_population_prediction)
GBD$age_name<-factor(GBD$age_name, levels = c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                              "90-94", "95+"))
unique(GBD$age_name)





# 整理人口学数据变成BAPC能够识别的数据形式

GBD_China_Male <- subset(GBD,location_name=="Global" & sex_name=="Both")
GBD_China_Male$age_name<-factor(GBD_China_Male$age_name, levels = c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                                                    "90-94", "95+"))
GBD_China_Male_n <- dcast(data=GBD_China_Male, year~age_name, value.var=c("val")) %>% as.data.frame()

#改行名
rownames(GBD_China_Male_n) <- GBD_China_Male_n$year

GBD_China_Male_n <- GBD_China_Male_n[,-1]

GBD_China_Male_n <- apply(GBD_China_Male_n, c(1,2), as.numeric) %>% as.data.frame()
GBD_China_Male_n <- apply(GBD_China_Male_n, c(1,2), round) %>% as.data.frame()



# 补充没有发病人数数据的年份
IBD_pro <- matrix(data=NA, nrow=2050-2021, ncol=ncol(GBD_China_Male_n)) %>% as.data.frame()
rownames(IBD_pro) <- seq(2022, 2050, 1)
colnames(IBD_pro) <- names(IBD_in_both_n)

IBD_pro_n <- rbind(IBD_in_both_n, IBD_pro)
IBD_pro_n <- apply(IBD_pro_n, c(1,2), as.numeric) %>% as.data.frame()
IBD_pro_n <- apply(IBD_pro_n, c(1,2), round) %>% as.data.frame()

require(INLA)
#sum(wstand)
# 模型预测
diabetes3_input <- APCList(IBD_pro_n, GBD_China_Male_n, gf=5)##gf为年份间隔
diabetes3_bapc_result <- BAPC(diabetes3_input, predict=list(npredict=29, retro=T), secondDiff=FALSE, stdweight=wstand, verbose=T)

p1<-plotBAPC(diabetes3_bapc_result, scale=10^5, type = 'ageStdRate', showdata = TRUE)



#提取数据
Male_de =data.frame(diabetes3_bapc_result@agestd.rate)


write.csv(Male_de,'C:\\Users\\86177\\Desktop\\NAFLD.csv')



