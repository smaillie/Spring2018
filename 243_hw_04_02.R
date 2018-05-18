#Samantha Maillie and Ariel Sim
#Homework 4 Problem 2

par(mfrow=c(1,3))
f <- function(x){
  return (exp(-(x^2)/2))
}
theta.hat<-se <-numeric(3)
#approximately .1359
n = 100
v = .1
x <- rnorm(n, 1.5, (v^2))
g <-f(x)*(1/sqrt(2*pi))
theta.hat[1] <-mean(g)
se[1] <-sd(g)
hist(g, main = "v = .1")

n = 100
v = 1
x <- rnorm(n, 1.5, (v^2))
g <-f(x)*(1/sqrt(2*pi))
theta.hat[2] <-mean(g)
se[2] <-sd(g)
hist(g, main = "v = 1")

n = 100
v = 10
x <- rnorm(n, 1.5, (v^2))
g <-f(x)*(1/sqrt(2*pi))
theta.hat[3] <-mean(g)
se[3] <-sd(g)
hist(g, main = "v = 10")
rbind(theta.hat, se)
par(mfrow=c(1,1))