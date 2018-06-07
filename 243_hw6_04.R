#Samantha Maillie 243 Homework 6 Problem 4e

library(boot)
library(invgamma)
B <- 5000
n <- 50
lam <- numeric(B)
lamCL <- numeric(B)
lamCU <- numeric(B)
lamDL <- numeric(B)
lamDU <- numeric(B)
lambda = 1
x = rexp(n, lambda)
lambda = 1/mean(x)


for(b in 1:B){
  i <-sample(1:n, size = n, replace = TRUE)
  lam[b] <- 1/mean(x[i])
  lamCL[b]<-1/mean(x[i])*exp((-1.96/sqrt(n)))
  lamCU[b]<-1/mean(x[i])*exp((1.96/sqrt(n)))
  gL <- pinvgamma(.025, n, rate = 1, scale = 1/rate, lower.tail = TRUE,
                 log.p = FALSE)
  lamDL[b]<- 1/mean(x[i])*gL
  gU <- pinvgamma(.975, n, rate = 1, scale = 1/rate, lower.tail = TRUE,
                  log.p = FALSE)
  lamDU[b]<- 1/mean(x[i])*gU
}

partC_CI <-cbind(lamCL, lamCU)
print(partC_CI)

partD_CI <-cbind(lamDL, lamDU)
print(partD_CI)
hist(lam, prob = TRUE, main = 'Histogram for lambda')




boot.bca<- function( x, th0, th, stat, conf = .95 ){
  x <- as.matrix(x)
  n <- nrow(x)
  N <- 1:n
  alpha <- (1 + c(-conf, conf))/2
  zalpha <- qnorm(alpha)
  
  z0 <- qnorm(sum(th<th0)/length(th))
  
  th.jack<-numeric(n)
  for (i in 1:n){
    #J <-N[1:(n-1)]
    #th.jack[i]<-stat(x[-i,], J)
    th.jack[i]<-stat(x[-i,])
  }
  L <- mean(th.jack) - th.jack
  a <- sum(L^3)/(6*sum(L^2)^1.5)
  
  adj.alpha <- pnorm(z0 + (z0+zalpha)/(1-a*(z0 + zalpha)))
  limits <- quantile(th, adj.alpha, type =6)
  return (list("est = th0", "BCa"=limits))
  }


theta.b <- numeric(B)
theta.hat <- mean(lam)

boot.bca(x, th0 = theta.hat, th = theta.b, stat = stat)