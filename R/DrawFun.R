#' 绘制偏效应图
#'
#' @param DrawIndus
#' @param PicName
#' @param EndwImptn
#' @param EndwImptn_CHN
#' @param RltFinal
#' @param is.save logical value, 是否保存图片

DrawFun <- function(DrawIndus,PicName,EndwImptn,EndwImptn_CHN,RltFinal,is.save = FALSE){
  bag.Fac <- RltFinal$RltForest[[DrawIndus]]
  x <- RltFinal$RltPredict[[DrawIndus]]
  x <- x[,!(names(x)  %in% c("employment","solo","poten_y","poten_y_rate", "code_name"))]

  endow <- data.frame(code = EndwImptn,name = EndwImptn_CHN)
  endow_2011 <- x[x$year == 2016,c('province',EndwImptn)]

  for(VarList in EndwImptn){
    PicData <- eval(parse(text=paste('partialPlot(bag.Fac,x[,-c(1,2)],',VarList,',plot=FALSE)',sep = '')))
    PicData <- data.frame(x=PicData$x,y=PicData$y)
    PicData_2 <- interp1(PicData$x,PicData$y,endow_2011[,VarList]) %>%
      data.frame(prv = endow_2011$province,x = endow_2011[,VarList],y = .)

    p <- ggplot(data = PicData,aes(x=x,y=y))+geom_line()+ theme_bw()+
      labs(x=VarList,y='',title = paste(endow$name[endow$code==VarList],'的偏效应',sep = ''))+
      theme(plot.title = element_text(size = '16',hjust = 0.5),
            axis.title =element_text(size = '14'),
            axis.text =element_text(size = '12')) +
      geom_point(data = PicData_2,aes(x=x,y=y),shape = 21, size = 4) # 经济研究的画圈圈
      # geom_text(data = PicData_2,aes(x=x,y=y,label = prv)) # 结题报告的写名字

    eval(parse(text = paste('p',as.character(which(EndwImptn %in% VarList)),'<- p',sep = '')))
  }
  if (is.save) ggplot2::ggsave(PicName,scale = 1.2)

  return(list(p1 = p1,p2 = p2, p3 = p3, p4 = p4))
}
