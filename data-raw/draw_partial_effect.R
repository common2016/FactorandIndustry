# 绘图4，图5偏效应图
rm(list = ls())
library(tidyverse)
library(ggplot2)
library(cowplot)
library(randomForest)
library(signal)
devtools::load_all()
load('data-raw/myrlt.RData')

# 通用设备
DrawIndus <- 'C34'
PicName <- 'data-raw/partial_effect1.png'
EndwImptn <- c('CityEmp','LandMining','RD','K')
EndwImptn_CHN <- c('行业就业人员年末数','城市工业用地','研究与试验发展人员全时当量','物质资本存量')
p <- DrawFun(DrawIndus,PicName,EndwImptn,EndwImptn_CHN,RltFinal,FALSE)
# 绘制箭头等
pos <- data.frame(x = c(2000, 300000, 200000), xend = c(50000,250000,155000),
                  y = c(1500, 2500,1600), yend = c(1100, 2500, 1800),
                  xtext = c(2500,300000,200000),
                  ytext = c(1580,2400,1500)) # 设定箭头和文本的坐标
p1 <- draw_arrow(p$p1, pos)

pos <- data.frame(x = c(500, 750, 500), xend = c(300,600,750),
                  y = c(1200, 1600,2000), yend = c(1200, 1600, 2000),
                  xtext = c(550,500,1000),
                  ytext = c(1200,2100,1600)) # 设定箭头和文本的坐标
p2 <- draw_arrow(p$p2, pos)

pos <- data.frame(x = c(30000, 350000, 300000), xend = c(30000,300000,260000),
                  y = c(1500, 2000,1700), yend = c(1300, 2000, 1700),
                  xtext = c(30000,359000,330000),
                  ytext = c(1580,2000,1750)) # 设定箭头和文本的坐标
p3 <- draw_arrow(p$p3, pos)

pos <- data.frame(x = c(5000, 20000, 40000), xend = c(7500,28000,38000),
                  y = c(1500, 1650,1760), yend = c(1350, 1650, 1760),
                  xtext = c(5000,43000,10000),
                  ytext = c(1530,1750,1650)) # 设定箭头和文本的坐标
p4 <- draw_arrow(p$p4, pos)
plot_grid(p1,p2,p3,p4,ncol = 2)


# 计算机
DrawIndus <- 'C39'
PicName <- 'data-raw/partial_effect1.png'
EndwImptn <- c('CityEmp','LandMining','RD','patent')
EndwImptn_CHN <- c('行业就业人员年末数','城市工业用地','研究与试验发展人员全时当量',
                   '国内专利申请授权数')
p_computer <- DrawFun(DrawIndus,PicName,EndwImptn,EndwImptn_CHN,RltFinal,FALSE)
# 绘制箭头
pos <- data.frame(x = c(40000, 2000000, 800000), xend = c(40000,2000000,650000),
                  y = c(3000, 8000,6000), yend = c(1700, 9000, 6000),
                  xtext = c(49000,2000000,1500000),
                  ytext = c(3500,7500,6000)) # 设定箭头和文本的坐标
p1 <- draw_arrow(p_computer$p1, pos)

pos <- data.frame(x = c(500, 1250, 1000), xend = c(500,1250,900),
                  y = c(3500, 4500,3800), yend = c(2600, 5500, 3800),
                  xtext = c(500,1200,1200),
                  ytext = c(3600,4500,3800)) # 设定箭头和文本的坐标
p2 <- draw_arrow(p_computer$p2, pos)

pos <- data.frame(x = c(50000, 470000, 350000), xend = c(50000,470000,380000),
                  y = c(3500, 4800,5500), yend = c(2500, 5800, 5500),
                  xtext = c(52000,470000,250000),
                  ytext = c(3500,4500,5500)) # 设定箭头和文本的坐标
p3 <- draw_arrow(p_computer$p3, pos)

pos <- data.frame(x = c(50000, 200000, 80000), xend = c(10000,200000,110000),
                  y = c(2500, 3250,3200), yend = c(2500, 3550, 3200),
                  xtext = c(51000,195000,40000),
                  ytext = c(2550,3250,3200)) # 设定箭头和文本的坐标
p4 <- draw_arrow(p_computer$p4, pos)
plot_grid(p1,p2,p3,p4,ncol = 2)
