#' 单行业随机森林建模函数
#'
#'
#' @param ind_code 字符串，单行业名字
#' @param Ind0111 行业主营业务收入
#' @param CityEmp 行业从业人员数
#' @param CityWage 行业工资
#' @param code_name 全部行业名
#' @param FactorEndw 要素禀赋
#' @return a list, 一个是包含上下界的预测值，一个是randomForest函数返回的对象。
each_indus <- function(ind_code, Ind0111,CityEmp, CityWage,code_name,FactorEndw,
                       n = 10){

  y <- Ind0111[Ind0111$IndCode == ind_code,]

  # input CityEmp, CityWage, 无数据返回空值
  if (purrr::is_empty(which(names(CityEmp) %in%
                     code_name$industry[code_name$IndCode %in% ind_code]))) {
    return(list(RltData = NULL, bag.Fac = NULL))
  }

  # 不同行业要对应不同行业的工资和就业
  colpos <- which(names(CityEmp) %in% code_name$industry[code_name$IndCode %in% ind_code])
  FactorEndw_new <- merge(FactorEndw,CityEmp[,c(1,2,colpos)],by = c('province','year'),all.x = T)
  names(FactorEndw_new)[ncol(FactorEndw_new)] <- 'CityEmp'

  colpos <- which(names(CityWage) %in% code_name$industry[code_name$IndCode %in% ind_code])
  FactorEndw_new <- merge(FactorEndw_new,CityWage[,c(1,2,colpos)],by = c('province','year'),all.x = T)
  names(FactorEndw_new)[ncol(FactorEndw_new)] <- 'CityWage'

  # 形成新的回归数据
  RltData <- merge(FactorEndw_new,y[,c(1,2,4)],all.x = TRUE)

  # 删除缺失值,观测值过少，返回空值
  RltData <- na.omit(RltData)
  if (nrow(RltData) <= 200) return(list(RltData = NULL, bag.Fac = NULL))

  # 随机森林分析
  ## 反复计算比如500次，获得预测值的抽样分布
  YQuantile <- NULL
  for (j in 1:n) {
    print(j)
    bag.Fac <- randomForest::randomForest(employment~.,data=RltData[,-c(1,2)], ntree = 1000)
    YQuantile <- rbind(YQuantile,t(as.data.frame(bag.Fac$predicted)))
  }
  YQuantile <- t(YQuantile)
  ## 获得0.025和0.975分位点
  RltData$poten_y_down <- apply(YQuantile, 1, quantile,0.025)
  RltData$poten_y_up <- apply(YQuantile, 1, quantile,0.975)
  RltData$poten_y <- apply(YQuantile, 1, mean)

  # 产出缺口
  RltData$poten_y_rate_down <- (RltData$employment -RltData$poten_y_down)/RltData$poten_y_down
  RltData$poten_y_rate_up <- (RltData$employment -RltData$poten_y_up)/RltData$poten_y_up
  RltData$poten_y_rate <- (RltData$employment -RltData$poten_y)/RltData$poten_y
  RltData$code_name <- unique(y$industry)

  return(list(RltData = RltData, bag.Fac = bag.Fac))
}
