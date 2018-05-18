#Samantha Maillie and Ariel Sim 
#Homework 4 Problem 6
par(mfrow=c(1,2))
f <- function (z){
  return((z^(-3/2)) * exp(-1.5*z - (2/z) + 2*sqrt(3) + log(2) ))
}

plot(f, main = "Plot of f(z)")

#E[z] = 1.3333 E[1/z] = 1.116025

for (k in 1:10){
rate = k
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

print(k)
print(mean(x))
print(mean(1/x))
}

#shapes 2 and 3 seem decent

shape = 2


for (k in 1:10){
  rate = shape
  shape = 1/k
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
  
  print(k)
  print(mean(x))
  print(mean(1/x))
}
# rate = 1 is best here it seems

shape = 3
for (k in 1:10){
  rate = shape
  shape = 1/k
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
  
  print(k)
  print(mean(x))
  print(mean(1/x))
}

#decide to stick with rate = 1, shape = 2 
print("Final Estimates: ")
rate = 2
shape = 1
x <- numeric(1000)
x[1] <- rgamma(1, shape = shape, rate = rate)


n = 10000

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

print(mean(x))
print(mean(1/x))
h <- seq(0, 1, by = .01)
plot(h, pgamma(h, shape = 2, rate = 1), 
     main = "Gamma shape = 2, rate = 1")
par(mfrow=c(1,1))