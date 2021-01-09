rm(list = ls())
library(tidyverse)
library(ggplot2)
library(cowplot)
library(randomForest)
library(signal)
library(purrr)
library(parallel)
devtools::load_all()

# 若设置TRUE, 则在CPU是i9,8核16线程下大约跑22分钟左右。因为是随机抽样，每次跑完结果略有差异。
# 若设置FALSE，则调用以前跑完存储的数据集。
if (FALSE){
  ans <- Sys.time()
  RltFinal <- Prelnd(FactorEndw,Ind0111,IndCode,CityEmp,CityWage, n = 500)
  Sys.time() - ans
  Since2009 <- RltFinal[['RltPredict']]
  Since2009 <- bind_rows(Since2009)
}else {
  load('data-raw/myrlt.RData')
}

# 取最后三年的平均
PicData <- group_by(Since2009[Since2009$year == 2016 |
                                Since2009$year == 2015|
                                Since2009$year == 2014,],province,code_name) %>%
  summarise(poten_y_rate_down = mean(poten_y_rate_down),
            poten_y_rate_up = mean(poten_y_rate_up),
            poten_y_rate = mean(poten_y_rate))

# 只绘显著的
PicData <- PicData[PicData$poten_y_rate_down * PicData$poten_y_rate_up > 0,]

PicData$PosNeg <- 0
PicData$PosNeg[PicData$poten_y_rate < 0] <- 1
PicData$PosNeg <- as.factor(PicData$PosNeg)

# 避免部分异常值太大或太小，从而图形大小比例失衡而进行的截尾处理
PicData[PicData$poten_y_rate > quantile(PicData$poten_y_rate,0.97,na.rm = T),'poten_y_rate'] <- 0.4
PicData <- PicData[abs(PicData$poten_y_rate) > 0.05,]

# 定义要素约束、非充分发展、非重要行业
library(reshape2)
leading <- openxlsx::read.xlsx('data-raw/主导产业.xlsx',1) %>% melt(id.vars = 'province')
names(leading)[2:3] <- c('leading','code_name')
leading$leading <- 'LeadingIndus'
PicData <- merge(PicData, leading[,], by = c('province','code_name'),all.x = T)
PicData$IndusCls <- NA
PicData$IndusCls[PicData$PosNeg == 0] <- 'const'
PicData$IndusCls[PicData$PosNeg == 1 & is.na(PicData$leading)] <- 'NotImp'
PicData$IndusCls[is.na(PicData$IndusCls)] <- 'NotSuffi'

# 绘制图2气泡图
p <- ggplot(data = PicData,aes(x=province,y=code_name,size = abs(poten_y_rate),fill=PosNeg,shape = IndusCls)) + geom_point()+
  labs(x=NULL,y=NULL) + scale_shape_manual(values = c(23,21,22))
p + scale_size_area(max_size = 10,guide=FALSE)+guides(fill = FALSE,shape = FALSE)+
  scale_fill_grey(start = 0.4,end = 0.8) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = rel(1.4)),
        axis.text.y = element_text(size = rel(1.4)))

