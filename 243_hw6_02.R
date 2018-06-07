#Samantha Maillie 243 Homework 6 Problem 2
f <-function(x, n, theta){
  return ( (n/(theta^n))* x^(n-1))
}

#a see write up 
#b
#.003327 this can be found from the distribution calculated in a

#c we see that this estimates the variance very close to the true value
#we calculated
B <- 5000
n <- 50
theta <- 3
maxi <- numeric(B)

x <- runif(n, 0, theta)

for(b in 1:B){
  i <-sample(1:n, size = n, replace = TRUE)
  maxi[b] <- max(x[i])
}

print(var.maxi <- var(maxi))
hist(maxi, prob = TRUE, main = 'Histogram from Part c')


#d This fails. The estimator is always biased and will always be lower 
#than the true parameter. If you lower B you can see that the theta
#value quickly tends to zero.

B <- 5000
n <- 50
maxi <- numeric(B)
theta <- 3 # operating as a starting guess for theta
x <- runif(n, 0, theta)
theta <- max(x)
for(b in 1:B){
  i <-sample(1:n, size = n, replace = TRUE)
  maxi[b] <- max(x[i])
  theta <-maxi[b]
  x <- runif(n, 0, theta)
}

print(se.maxi <- var(maxi))
hist(maxi, prob = TRUE, main = 'Histogram from Part d')


#e Part d failed but part c did very well and displays a distribution
#very similar to the true function 

n = 50
theta = 3
x <- runif(n, 0, theta)

plot(x, f(x, n, theta), main = 'True function for MLE')
