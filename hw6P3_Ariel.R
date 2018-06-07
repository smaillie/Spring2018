# STA 243 Homework 6
# Problem 3 - Bootstrap

source('hw6functions.R')

# Generate simulated data
# Setup
n = 512 ;x = (0:(n-1))/n ;f1 = truefunction1(x) ;f2 = truefunction2(x)

set.seed(10)
y1 = f1 + rnorm(f1)/3
y2 = f2 + rnorm(n, mean=0, sd=sqrt(integrate(function(z) {truefunction2(z)^2},lower=0,upper=1)$value)/5 )

# a
# From HW2 setup:
S = 400 ;p1 = 0.95; p2 = 0.08 ;N = 20

result1 = GenAlgo(x,y1,S,p1,p2,N,MDL)
result2 = GenAlgo(x,y2,S,p1,p2,N,MDL)

# Save the image to pdf
jpeg('hw6_3a_test1.jpg', width=4.25, height=3.25, units="in", res=1000, pointsize=4)
y1_hat = plot_estimate_piecewise(x,y1,unlist(result1[[1]]),plot_estimate_piecewiseimation=TRUE)
dev.off()
jpeg('hw6_3a_test2.jpg', width=4.25, height=3.25, units="in", res=1000, pointsize=4)
y2_hat = plot_estimate_piecewise(x,y2,unlist(result2[[1]]),plot_estimate_piecewiseimation=TRUE)
dev.off()

# b


# set up parallel computing; load packages
library(tictoc)
library('foreach')
library('parallel')
library('iterators')
library('doParallel')

t = 2000 # of simulations
nc = 20 # cores for parallel computing 

# Test function 1
# Bootstrap Residuals
resid1_hat = y1 - y1_hat
registerDoParallel(nc)
tic('bootstrap resid test1')
resid1 = foreach(k=1:t,.combine="rbind",.multicombine=TRUE,.export=c('bootstrap_resid')) %dopar% bootstrap_resid(x,y1_hat,resid1_hat,11+k,S,p1,p2,N,MDL)
toc()
plotCB(x,f1,resid1,conf=0.95,'Test function 1 bootstrap residuals')

# Bootstrap Pairs
registerDoParallel(nc)
tic('bootstrap pairs test1')
pair1 = foreach(k=1:t,.combine="rbind",.multicombine=TRUE,.export=c('bootstrap_pair')) %dopar% bootstrap_pair(x,y1,12+k,S,p1,p2,N,MDL)
toc()
plotCB(x,f1,pair1,conf=0.95,'Test function 1 bootstrap pairs')

# Test function 2
# Bootstrap Residuals
resid2_hat = y2 - y2_hat

registerDoParallel(nc)
tic('bootstrap resid test2')
resid2 = foreach(k=1:t,.combine="rbind",.multicombine=TRUE,.export=c('bootstrap_resid')) %dopar% bootstrap_resid(x,y2_hat,resid2_hat,13+k,S,p1,p2,N,MDL)
toc()
plotCB(x,f2,resid2,conf=0.95,'Test function 2 bootstrap residuals')

# Bootstrap Pairs
registerDoParallel(nc)
tic('bootstrap pairs test2')
pair2 = foreach(k=1:t,.combine="rbind",.multicombine=TRUE,.export=c('bootstrap_pair')) %dopar% bootstrap_pair(x,y2,14+k,S,p1,p2,N,MDL)
toc()
plotCB(x,f2,pair2,conf=0.95,'Test function 2 bootstrap pairs')

## save
save(x,y1,y2,f1,f2,resid1,resid2,pair1,pair2, file='hw6_3.RData')
