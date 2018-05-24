#Samantha Maillie
#Homework 4 Problem 3

h <- function(x){
  return (1/(1+x))
}

plot(h, main = "Function Plot", 
     xlim = c(0,1), ylim = c(0,1), col = "blue")

#approx log(2) = .6931472

count = 0
n = 10000
for (j in 1:n){
  x = runif(1,0,1)
  y = runif(1,0,1)
  if(y < h(x)){
    count = count + 1
  }
}
estimate  = count / n
estimate


total = 0
n = 1500
for (i in 1:n){
  u = runif(1, 0, 1)
  total = total + h(u)
}
i = total/n
i

# a
n = 1500
U = runif(n,0,1)
h = 1/(1+U)
I_mc = sum(h)/n
I_mc
log(2) # comparable

# b
U2 = runif(n,0,1)
c = 1 + U2
theta_mc = sum(c)/n
theta_mc

# Calculating the optimal b:
num = sum((c - theta_mc)*(h - I_mc))
denom = sum((c - theta_mc)^2)
b = num/denom
b

# Therefore
I_cv = I_mc - b*(theta_mc-0.5)
I_cv

