#Samantha Maillie and Ariel Sim
#Homework 4 Problem 5

par(mfrow=c(1,3))
#a
n <- 100
p <- .3
lambda <- 2

y <- rpois(n, lambda)
r <- rbinom(n, 1, p)
x = y*r
hist(x, main = "Histogram of X", col = "blue")


#c

a=b=1
T=10^4
lamb=pe=rep(.5,T)
for (t in 2:T){
  r=(x==0)*(runif(n)<1/(1+(1-pe[t-1])/(pe[t-1]*exp(-lamb[t-1]))))+(x>0)
  lamb[t]=rgamma(1,a+sum(x),b+sum(r))
  pe[t]=rbeta(1,1+sum(r),n-sum(r)+1)}


hist(pe, main = "Histogram of P", col = "purple")
hist(lamb, main = "Histogram of Lambda", col = "springgreen")
par(mfrow=c(1,1))

#95% confidence interval is (2.5% quantile, 97.5%qunatile)
print("P qunatiles")
quantile(pe, prob = seq(0, 1, length = 41), type = 5)
print("Lambda qunatiles")
quantile(lamb, prob = seq(0, 1, length = 41), type = 5)


#Source for assistance in setting up sample generation in part c
#“Implementing Gibbs Sampler in R from Posterior Distribution.” 
#   Cross Validated, stats.stackexchange.com/questions/143468
#   /implementing-gibbs-sampler-in-r-from-posterior-distribution.