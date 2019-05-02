##functions.R

##Example with stat_range()
install.packages("testthat")
library("testthat")

#' @title Range
#' @description computes the range of a numeric vector (i.e. max - min)
#' @param x a numeric vector
#' @return the range value (max - min)
stat_range <- function(x) {
  max(x) - min(x)
}


#' @title Measures of center
#' @description computes the measures of center such as Median and mean
#' @param x a numeric vector
#' @return a numeric vector with median and mean
stat_centers <- function(x){
  return(c(mean(x), median(x))) 
}


#' @title Measures of spread
#' @description computes measures of spread such as Range, IQR, Standard Deviation
#' @param x a numeric vector
#' @return a numeric vector with range, iqr, and stdev 
stat_spreads <- function(x){
  return(c((max(x)-min(x)), (quantile(x, 0.75)-quantile(x,0.25)), sd(x)))
  
}



last_3 <- function(states){
  for (i in 1:length(states)){
    return(substr(states, (nchar(states[i])-3), (nchar(states[i]))))
  }
}
