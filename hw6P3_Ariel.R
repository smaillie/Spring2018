# STA 243 Homework 6
# Problem 3 - Bootstrap

source('hw6functions.R')

# Generate simulated data
truefunction1 = function(x) {
  t = c(0.1, 0.13, 0.15, 0.23, 0.25,
        0.4, 0.44, 0.65, 0.76, 0.78, 0.81)
  h = c(4, -5, 3, -4, 5, -4.2, 2.1, 4.3, -3.1, 2.1, -4.2)
  temp = 0
  for(i in 1:11) {
    temp = temp + h[i]/2 * (1 + sign(x - t[i]))
  }
  return(temp)
}

truefunction2 = function(x) {
  temp = (4*x-2)+2*exp(-16*(4*x-2)^2)
  return(temp)
}

# Setup
n = 512
x = (0:(n-1))/n
f1 = truefunction1(x)
f2 = truefunction2(x)

set.seed(10)
y1 = f1 + rnorm(f1)/3
y2 = f2 + rnorm(n, mean=0, sd=sqrt(integrate(function(z) {truefunction2(z)^2},lower=0,upper=1)$value)/5 )

# plot(x,y)
# lines(x,f)
# plot(x,f1,type='l')
# plot(x,f2,type='l')
# points(x,y2)


# a
# Parameters in Homework 2
S = 400 # 300 for AIC, 400 for MDL
p1 = 0.95 # both 0.90 and 0.95 work
p2 = 0.08
N = 20

result1 = GenAlgo(x,y1,S,p1,p2,N,MDL)
result2 = GenAlgo(x,y2,S,p1,p2,N,MDL)

jpeg('hw6_3a_test1.jpg', width=4.25, height=3.25, units="in", res=1000, pointsize=4)
y1_hat = plot_estimate_piecewise(x,y1,unlist(result1[[1]]),plot_estimate_piecewiseimation=TRUE)

dev.off()

jpeg('hw6_3a_test2.jpg', width=4.25, height=3.25, units="in", res=1000, pointsize=4)
y2_hat = plot_estimate_piecewise(x,y2,unlist(result2[[1]]),plot_estimate_piecewiseimation=TRUE)

dev.off()

# b

t = 2000 # of simulations
nc = 20 # cores

# Test function 1
# Bootstrap Residuals
res1_hat = y1 - y1_hat
seed1 = 11
registerDoParallel(nc)

res1_B = foreach(k=1:t,.combine="rbind",.multicombine=TRUE,.export=c('bootstrap_resid')) %dopar% bootstrap_resid(x,y1_hat,res1_hat,seed1+k,S,p1,p2,N,MDL)

plotCB(x,f1,res1_B,conf=0.95,'bootstrap residuals for test function 1')

# Bootstrap Pairs
seed2 = 12
registerDoParallel(nc)
tic('bootstrap pairs for test1')
pair1_B = foreach(k=1:t,.combine="rbind",.multicombine=TRUE,.export=c('bootstrap_pair')) %dopar% bootstrap_pair(x,y1,seed2+k,S,p1,p2,N,MDL)
toc()
plotCB(x,f1,pair1_B,conf=0.95,'bootstrap pairs for test function 1')

# Test function 2
# Bootstrap Residuals
res2_hat = y2 - y2_hat

seed3 = 13
registerDoParallel(nc)
tic('bootstrap residuals for test1')
res2_B = foreach(k=1:t,.combine="rbind",.multicombine=TRUE,.export=c('bootstrap_resid')) %dopar% bootstrap_resid(x,y2_hat,res2_hat,seed3+k,S,p1,p2,N,MDL)
toc()
plotCB(x,f2,res2_B,conf=0.95,'bootstrap residuals for test function 2')

# Bootstrap Pairs
seed4 = 14
registerDoParallel(nc)
tic('bootstrap pairs for test1')
pair2_B = foreach(k=1:t,.combine="rbind",.multicombine=TRUE,.export=c('bootstrap_pair')) %dopar% bootstrap_pair(x,y2,seed4+k,S,p1,p2,N,MDL)
toc()
plotCB(x,f2,pair2_B,conf=0.95,'bootstrap pairs for test function 2')


## save
save(x,y1,y2,f1,f2,res1_B,res2_B,pair1_B,pair2_B, file='hw6P3.RData')
