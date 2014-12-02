rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R/titanic')
getwd()
dir()

library(compiler)
x <- rnorm(10000000)
y <- rnorm(10000000)
z <- vector(mode="numeric",length=10000000)
system.time(z <- x * y)

test1 <- function(){
  for(i in 1:10000000)
    z[i] <<- x[i] * y[i]
}

system.time(test1())
#    user  system elapsed 
#  39.551   0.094  39.651

comp_test1 <- cmpfun(test1)
system.time(comp_test1())
#    user  system elapsed 
#  11.831   0.066  11.898 

system.time(z <- x * y)
#    user  system elapsed 
#   0.034   0.022   0.056