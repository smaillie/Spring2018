#Samantha Maillie and Ariel Sim 
#Homework 4 Problem 1


# Dart Board Method
#a true answer = 1/3

fa <-function(x){
  return (x^2)
}

plot(fa, main = "Function A Plot")

count = 0
n = 999
for (i in 1:n){
  x = runif(1,0,1)
  y = runif(1,0,1)
  if(y < fa(x)){
    count = count + 1
  }
}
estimate = count / n
estimate

#b true answer = 3.48318

fb <-function(x, y){
  return ((x^2)*cos(x*y))
}

x <- seq(-2, 2, length = 30)  ## already in increasing order
y <- seq(0, 1, length = 30)  ## already in increasing order
f <- fb
z <- outer(x, y, f)  ## evaluation on grid; obtain a matrix `z`
par(mfrow=c(1,2))
persp(x, y, z, col = "orchid", ticktype = "detailed", 
      xlim = c(-2,2), ylim = c(0,1), theta = 90, phi = 15,
      main = "Y by Z View")
persp(x, y, z, col = "cyan", ticktype = "detailed", 
      xlim = c(-2,2), ylim = c(0,1), theta = 180, phi = 0,
      main = "X by Z View")
par(mfrow=c(1,1))



count = 0
n = 100000
for (i in 1:n){
  x = runif(1,-2,2)
  y = runif(1,0,1)
  z = runif(1,-1.7,4)
  if(z >= 0 && z <= fb(x,y)){
    count = count + 1
  }
  if(z <0 && z <= fb(x,y)){
    count = count - 1
  }
}
boxVol = 5.7*4*1
prob = abs(count) / n
estimate =boxVol*prob
estimate


#c approximated as 2.27477 via integral-calculator.com
fc <- function(x){
  return ( (3/4)* x^4 * exp(-(x^3)/4))
}

plot(fc, main = "Function C Plot", 
     xlim = c(0,5), 
     ylim = c(0,2))

count = 0
n = 999
for (i in 1:n){
  x = runif(1,0,3)
  y = runif(1,0,2)
  if(y < fc(x)){
    count = count + 1
  }
}
prob = count / n
estimate = prob* (3*2)
estimate
