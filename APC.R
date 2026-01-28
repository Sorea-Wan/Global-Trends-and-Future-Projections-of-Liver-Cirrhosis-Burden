

setwd("D:/work/云象/GBD课程介绍/方法/06_2 年龄周期队列模型")

install.packages("Epi")
library(ggsci)
library(ggplot2)
library(Epi)
library(readxl)
library(RColorBrewer)
####data 由APC跑出后与人数相除


#####Deaths
data <- read.csv("IBD_in_both_population.csv",header = FALSE)
data1<-data[,-c(1,2)]
data1$a1992<-data1$V3/data1$V4
data1$a1997<-data1$V5/data1$V6
data1$a2002<-data1$V7/data1$V8
data1$a2007<-data1$V9/data1$V10
data1$a2012<-data1$V11/data1$V12
data1$a2017<-data1$V13/data1$V14

data2<-data1[,c("a1992","a1997","a2002","a2007","a2012","a2017")]

age_group<-c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)
period<-c(1992,1997,2002,2007,2012,2017)

rownames(data2) <- age_group
colnames(data2) <- period
array_data2 <- as.matrix(data2)
class(array_data2)

mycolor1 <-rainbow(20)
mycolor2<-brewer.pal(20, "Spectral")#可选择的颜色数量区间在此配色最大数和绘图所需数之间


## The four classical plots:
par( mfrow=c(2,2) )
rateplot( array_data2*10^6,lwd = 4,c.lab ="Birth cohort",a.lab ="Age group", col =mycolor2,ann = T
          ,ylab= "Rate of Deaths (per 10,0000 people)",legend=rownames(array_data2),p.lim=range(1992,2012))

dev.off()

## The labels on the vertical axis could be nicer:
rateplot(array_data1*10^6, at=10^(-1:3), labels=c(0.1,1,10,100,1000) ) 



ggsave("save.png", width = 60, height = 30, units = c("cm"), dpi = 400)








# Aplot( rates, age = as.numeric( dimnames( rates )[[1]] ),
#        per = as.numeric( dimnames( rates )[[2]] ), grid = FALSE,
#        a.grid = grid, ygrid = grid, col.grid = gray( 0.9 ),
#        a.lim = range( age, na.rm=TRUE ), ylim = range( rates[rates>0], na.rm=TRUE ),
#        at = NULL, labels = paste( at ), a.lab = names( dimnames( rates ) )[1],
#        ylab = deparse( substitute( rates ) ), type = "l", lwd = 2, lty = 1,
#        col = par( "fg" ), log.ax = "y", las = 1, c.col = col, p.col = col,
#        c.ann = FALSE, p.ann = FALSE, xannx = 1/20, cex.ann = 0.8,
#        c.thin = seq( 2, length( age ) + length( per ) - 1, 2 ),
#        p.thin = seq( 1, length( per ), 2 ), p.lines = TRUE,
#        c.lines = !p.lines, ... )

# Cplot( rates, age = as.numeric( rownames( rates ) ),
#        per = as.numeric( colnames( rates ) ), grid = FALSE,
#        c.grid = grid, ygrid = grid, col.grid = gray( 0.9 ),
#        c.lim = NULL, ylim = range( rates[rates>0], na.rm=TRUE ),
#        at = NULL, labels = paste( at ), c.lab = names( dimnames( rates ) )[2],
#        ylab = deparse( substitute( rates ) ), type = "l", lwd = 2, lty = 1,
#        col = par( "fg" ), log.ax = "y", las = 1, xannx = 1/20, ann = FALSE,
#        cex.ann = 0.8, a.thin = seq( 1, length( age ), 2 ), ...  )
###单独跑图
Aplot( array_data1*10^6,a.lab ="Age", col =mycolor2,p.ann = TRUE,lwd=3
       ,ylab= "Rate of Deaths(per 10,000 prople)",legend=rownames(array_data1) )


Cplot( array_data1*10^6,lwd = 4,c.lab ="Birth cohort", col =mycolor2,ann = TRUE
       ,ylab= "Rate of Deaths(per 10,000 prople)",legend=rownames(array_data1))

