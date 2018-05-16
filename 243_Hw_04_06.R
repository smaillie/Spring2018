#Samantha Maillie
#Homework 4 Problem 6

f <- function (z){
  return((z^(-3/2)) * exp(-1.5*z - (2/z) + 2*sqrt(3) + log(2) ))
}

plot(f, main = "Plot of f(z)")

#E[z] = 1.3333 E[1/z] = 1.116025

rate = 1
shape = 2
x <- numeric(1000)
x[1] <- rgamma(1, shape = shape, rate = rate)


n = 1000

for (j in 2:n){
  y = rgamma(1, shape = shape, rate = rate)
  u = runif(1, 0 , 1)
  num = f(y)*dgamma(x[j-1], shape = shape, rate = rate)
  den = f(x[j-1])* dgamma(y, shape = shape, rate = rate)
  if(u <= num/den){
    x[j] = y
  }
  else{
    x[j] = x[j-1]
  }
}

mean(x)
mean(1/x)

h <- seq(0, 4, by = .05)
plot(h, pgamma(h, shape = 2, rate = 1), 
     main = "Gamma shape = 2, rate = 1")