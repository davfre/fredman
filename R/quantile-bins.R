#' quantile_bins
#' 
#' vector (numeric) -> vector (bin labels)
#' 
quantile_bins = function(values, echo=FALSE){
  cut(x,quantile(values, na.rm=TRUE), include.lowest=TRUE, labels=FALSE)
}
