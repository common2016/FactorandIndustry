#' 行业产出预测
#'
#' @param FactorEndw
#' @param Ind0111
#' @param IndCode
#' @param CityEmp
#' @param CityWage
#' @param n 计算预测值上下界时的抽样次数

Prelnd <- function(FactorEndw,Ind0111,IndCode,CityEmp,CityWage, n = 20){
  # 提取行业名
  code_name <-unique(Ind0111[,c('IndCode','industry')])

  # 并行运算
  cl <- parallel::makeCluster(parallel::detectCores())
  parallel::clusterEvalQ(cl,{
    library(randomForest)
  }) %>% invisible()
  ans <- parallel::parLapply(cl, X = IndCode, fun = each_indus, Ind0111 = Ind0111,
                      CityEmp = CityEmp, CityWage = CityWage,code_name = code_name,
                      FactorEndw = FactorEndw, n = n)
  parallel::stopCluster(cl)

  # 分类存储
  RltPredict <- lapply(1:length(ans), function(x, obj=ans) obj[[x]]$RltData)
  RltForest <- lapply(1:length(ans), function(x, obj=ans) obj[[x]]$bag.Fac)
  names(RltForest) <- names(RltPredict) <- code_name$IndCode

  return(list(RltPredict = RltPredict,RltForest = RltForest))
}

