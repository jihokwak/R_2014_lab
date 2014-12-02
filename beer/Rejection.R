rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R')
getwd()
dir()

set.seed(1000)
# The Sampling-Importance-Resampling algorithm

p <- function(x) {
  0.3 * exp(-(x - 0.3)^2) + 0.7 * exp(-(x - 2)^2/0.3)
}

q <- function(x) {
  4
}

sir <- function(n) {
  w <- vector(n, mode = "numeric")
  # sample2 <- vector(n, mode='numeric')
  
  # sample from q
  sample1 <- runif(n, 0, 4)
  
  # 특정 점에서 나올 가중치를계산
  w <- p(sample1)/q(sample1)
  # 가중치 정규화 값
  w <- w/sum(w)
  
  # 가중치에 따른 재 샘플링 cumw <<- cumsum(w)
  
  # 정규화 되었으니 0,4까지 구할 필요가 없다.  u <- runif(n) for(i in
  # seq(n)){ indices <- which(u < cumw[i]) sample2[indices] <- sample1[i]
  # u[indices] <- 100 }
  sample2 <- sample(sample1, n, prob = w, replace = T)
  return(sample2)
}



library(ggplot2)

# real data

real <- data.frame(real_y = p(seq(-1, 5, by = 1e-04)), real_x = seq(-1, 5, by = 1e-04))
sir_sampling <- data.frame(sir = sir(10000))

ggplot(sir_sampling, aes(sir)) + geom_histogram(aes(y = ..density.., fill = ..density..)) + 
  geom_line(aes(x = real_x, y = real_y), data = real, colour = "red2") + scale_fill_gradient2(guide = guide_legend(reverse = TRUE))