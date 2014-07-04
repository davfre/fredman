#' bootMean
#' 
#' Calculate standard error of the mean via bootstrap
  
library(boot)

meanFunc = function(x,i){
  mean(x[i])
}

bootMean = function(x,n=100){
  boot(x,statistic=meanFunc,R=n)
}
