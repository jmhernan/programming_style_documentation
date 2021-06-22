library(stats); library(ggplot2)
fUncTion1 <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}
fUnctioN2=function(x,conf=0.95) {
  se=sd(x)/sqrt(length(x))
  alpha=1-conf
  mean(x)+se*qnorm(c(alpha/2,1-alpha/2))
}
x <- runif(100)
w <- 1:100
print(paste0('The weighted mean is: ', fUncTion1(x,w)))
print(paste0('The mean confidence interval is: ',fUnctioN2(x)[1], ' and ', fUnctioN2(x)[2]))
