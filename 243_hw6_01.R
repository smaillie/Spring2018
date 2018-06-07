#Samantha Maillie 243 Homework 6 Problem 1

lsat <-c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 
          545, 572, 594)
gpa <- c(3.39, 3.3, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43, 3.36, 3.13, 3.12,
         2.74, 2.76, 2.88, 3.96)

#a
cor(lsat, gpa)

  #statistical computing with R provides majority of code p 186, 193
   #everything but my variable names I pulled directly from book. 
   #Source is properly cited in corresponding report
#b
#Bootstrap
B <- 200
n <-length(lsat)
R <- numeric(B)

for(b in 1:B){
  i <-sample(1:n, size = n, replace = TRUE)
  LSAT <- lsat[i]
  GPA <- gpa[i]
  R[b]<- cor(LSAT, GPA)
}

print(se.R <- sd(R))
hist(R, prob = TRUE)

#Jackknife
theta.hat <- mean(lsat)/ mean(gpa)
print(theta.hat)

theta.jack<-numeric(n)
for( i in 1:n){
  theta.jack[i] <-mean(lsat[-i])/mean(gpa[-i])
}

se <- sqrt( (n-1)* mean((theta.jack - mean(theta.jack))^2))
print(se)

# again statistical computing with R provides majority of code p 199, 202
#c
library(boot)
theta.boot <-function(dat, ind){
  y <- dat[ind, 1]
  z <- dat[ind, 2]
  cor(y, z)
}
y <- lsat
z <-gpa
dat <- cbind(y,z)
boot.obj <- boot(dat, statistic = theta.boot, R = 2000)
print(boot.ci(boot.obj, type = c("basic", "norm", "perc")))

boot.t.ci <-function(x, B = 500, R = 100, level = .95, statistic){
  x <- as.matrix(x); n <-nrow(x)
  stat <-numeric(B); se <- numeric(B)
  
  boot.se <- function(x, R, f){
    x <- as.matrix(x); m <- nrow(x)
    th <- replicate(R, expr = {
       i <- sample(1:m, size = m, replace = TRUE)
       f(x[i,])
    })
    return(sd(th))
  }
  for(b in 1:B){
    j <- sample(1:n, size = n, replace = TRUE)
    y <- x[j,]
    stat[b] <-statistic(y)
    se[b]<- boot.se(y, R = R, f = statistic)
  }
  stat0 <-statistic(x)
  t.stats <- (stat - stat0) / se
  se0 <- sd(stat)
  alpha <- 1-level
  Qt <- quantile(t.stats, c(alpha/2, 1 - alpha/2), type = 1)
  names(Qt) <- rev(names(Qt))
  CI <- rev(stat0 - Qt * se0)
}





stat <- function(dat){
  cor(dat[,1], dat[,2])
}
#note this confidence interval should stop at -1, the code is just
# a general script from the book. I adjust it so that the correlation
#coefficient is bounded between -1 and 1
ci <- boot.t.ci(dat, statistic = stat, B= 2000, R = 200)
print(ci)
print("Corrected CI:")
if(ci[1] <-1){ci[1]= -1}
if(ci[1] >1){ci[1]= 1}
print(ci)
