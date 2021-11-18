library(tidyverse)
# Project members: Zachary Hauck, Michella Kopti, Joseph Lunt
CLT = function(x,n,b,lgcl=FALSE){
  set.seed(13)
  data = sample(x,n*b, replace=TRUE)
  "xbar1" = matrix(data,nrow=n, ncol=b) #create xbar1 to make the data into a matrix
  "xbar" = apply(xbar1, 2, mean, na.rm=TRUE) #this is to put the matrix in with correct dimensions
  if(lgcl==TRUE)hist(xbar) #to return a histogram is lcgl=TRUE
  return(list(xbar, shapiro.test(xbar)))
}


cauchy = rcauchy(1:120,location = 100, scale=3)
CLT(cauchy,15,103,TRUE)
