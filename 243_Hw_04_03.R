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