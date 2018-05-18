#Samantha Maillie and Ariel Sim
#Homework 4 Problem 5


#a
n <- 100
p <- .3
lambda <- 2

y <- rpois(n, lambda)
r <- rbinom(n, 1, p)
x = y*r
hist(x, main = "Histogram of X", col = "coral")

#b

#c