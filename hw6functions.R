# Functions for Homework 6 (and from hw 2)

# (MDL) Minimum Description Length Principle 
MDL = function(x,y,break_points) {
  #x, y: data set
  #break_points: the break points as a vector
  n = length(x)
  B = sum(break_points)+1
  ind = which(break_points==1)
  ind = c(1,ind,length(x)+1)
  y_hat = c()
  
  # compute estimated y values
  for(k in seq(1,B,1)) {
    y_hat = c(y_hat,rep(mean(y[seq(ind[k],ind[k+1]-1,1)]),ind[k+1]-ind[k]))
  }
  
  # compute MDL value
  z = B*log(n) + sum(log(tail(ind,-1)-head(ind,-1)))/2 + n/2*log(mean((y - y_hat)^2))
  
  return(z)
}

AIC = function(x,y,break_points) {
  # x, y: data set 
  # break_points: the break points as a vector
  n = length(x)
  B = sum(break_points)+1
  ind = which(break_points==1)
  ind = c(1,ind,length(x)+1)
  y_hat = c()
  
  # compute estimated y values
  for(k in seq(1,B,1)) {
    y_hat = c(y_hat,rep(mean(y[seq(ind[k],ind[k+1]-1,1)]),ind[k+1]-ind[k]))
  }
  
  z = n*log(mean((y_hat - y)^2)) + log(n)*2*B
  
  return(z)
}

# plot the estimated piecewise constant function and return the piecewise constant function and estimated y values.
plot_estimate_piecewise = function(x,y,break_points,plot_est=FALSE) {
  # plot_est: takes values of TRUE or FALSE
  B = sum(break_points)+1
  ind = which(break_points==1)
  ind = c(1,ind,length(x)+1)
  y_hat = c()
  
  # compute estimated y values
  for(k in seq(1,B,1)) {
    y_hat = c(y_hat,rep(mean(y[seq(ind[k],ind[k+1]-1,1)]),ind[k+1]-ind[k]))
  }
  
  if (plot_est) {
    plot(x,y,col='black')
    lines(x,y_hat,col='red')
  }
  
  return(y_hat)
}

# Genetic Algorithm borrowed from Yao since my own function did not work well in Homework 2, you can use your own.
GenAlgo = function(x,y,size,p1,p2,N,func) {
  # invPerm function is from package Matrix
  library(Matrix)
  # x, y: data set
  # size: generation size
  # p1: crossover probability
  # p2: mutation probability
  # N: stop criterion
  # func: function of objective
  obj_FUN = match.fun(func)
  
  n = length(x)
  
  generation0 = matrix(0,nrow=size,ncol=n)
  # generate initial generation
  # use a vector of bernoulli rv with success prob. = 0.05
  
  for(k in seq(1,size,1)) {
    # end points are not break points
    generation0[k,] = c(0,rbinom(n-2,1,0.05),0)
  }
  
  same_chromosome_count = 1
  
  count = 0
  generation_last = generation0
  fitness = apply(generation_last,1,function(z) obj_FUN(x,y,z))
  rk = invPerm(unlist(order(fitness,decreasing=T),use.names=F))
  p = rk/sum(rk)
  best0 = generation_last[which.max(rk),]
  while(same_chromosome_count < N) {
    
    next_generation = matrix(0,nrow=size,ncol=n)
    # always keep the best chromosome
    next_generation[1,] = best0
    # number of crossover in this new generation
    num_co = rbinom(1,size-1,p1)
    # compute crossover offsprings
    for(k in seq(2,num_co+1,1)) {
      parents = sample(seq(1,size,1),size=2,replace=F,prob=p)
      ind1 = rbinom(n-2,1,0.5)
      ind2 = c(0,ind1==0,0)
      ind1 = c(0,ind1,0)
      next_generation[k,] = ind1*generation_last[parents[1],]+ind2*generation_last[parents[2],]
    }
    # compute mutation offsprings
    mutation_start = num_co+2
    if(mutation_start <= size) {
      for(k in seq(mutation_start,size,1)) {
        mutation_loc = which(c(0,rbinom(n-2,1,p2),0)==1)
        parent = sample(seq(1,size,1),size=1,replace=F,prob=p)
        temp = generation_last[parent,]
        temp[mutation_loc] = abs(temp[mutation_loc]-1)
        next_generation[k,] = temp
      }
    } else{ # do nothing 
    }
    
    fitness = apply(next_generation,1,function(z) obj_FUN(x,y,z))
    rk = invPerm(unlist(order(fitness,decreasing=T),use.names=F))
    p = rk/sum(rk)
    
    best = next_generation[which.max(rk),]
    if (sum(abs(best-best0))==0) {
      same_chromosome_count = same_chromosome_count + 1
    } else {
      same_chromosome_count = 1
    }
    best0 = best
    generation_last = next_generation
    count = count + 1
    #  Print score for testing  
    #  print(count)
    #  print(obj_FUN(x,y,best0))
  }
  # plot the estimated piecewise constant function
  plot_est(x,y,best0)
  return(list(best0,count,obj_FUN(x,y,best0)))
}



# Perform bootstrap residual estimation once
bootstrap_resid = function(x,y_hat,res_hat,seed,S,p1,p2,N,scoreFUN) {
  
  # x: x vector
  # y_hat: estimated y
  # res_hat: estimated residuals
  # seed: any seed number
  # size, p1, p2, N, scoreFUN from GenAlgo parameters
  # Returns y_hat_b - estimated bootstrap
  
  n = length(x)
  res_b_ind = sample(1:n,size=n,replace=TRUE)
  y_b = y_hat + res_hat[res_b_ind]
  result = GenAlgo(x,y_b,S,p1,p2,N,scoreFUN)
  y_hat_b = plot_estimate_piecewise(x,y_b,unlist(result[[1]]),plot_estimate_piecewiseimation=FALSE)
  return(y_hat_b)
}


# Perform bootstrap pairs once
bootstrap_pair = function(x,y,seed,S,p1,p2,N,scoreFUN) {
  set.seed(seed)
  n = length(x)
  pair_b_ind_temp = sample(1:n,size=n,replace=TRUE)
  pair_b_ind = sort(unique(pair_b_ind_temp))
  x_b = x[pair_b_ind]
  y_b = y[pair_b_ind]
  result = GenAlgo(x_b,y_b,S,p1,p2,N,scoreFUN)
  y_hat_b_temp = plot_estimate_piecewise(x_b,y_b,unlist(result[[1]]),plot_estimate_piecewiseimation=FALSE)
  y_hat_b = vector(mode='numeric',length=n)
  y_hat_b[pair_b_ind] = y_hat_b_temp
  
  for(k in 1:pair_b_ind[1]) {
    y_hat_b[k] = y_hat_b[pair_b_ind[1]]
  }
  
  to_fill = sort(setdiff(1:n,c(1:pair_b_ind[1],pair_b_ind[-1])))
  for(k in to_fill) {
    y_hat_b[k] = y_hat_b[k-1]
  }
  
  return(y_hat_b)
}

# Plot true function and bootstrap confidence band
plotCB = function(x,f,B_matrix,conf=0.95,plot_name) {
  n = length(x)
  T = nrow(B_matrix)
  cl = as.integer(((1-conf)/2*T))
  cu = as.integer(((1+conf)/2*T))
  y_upper = vector(mode='numeric',length=n)
  y_lower = vector(mode='numeric',length=n)
  for(k in 1:n) {
    temp = sort(B_matrix[,k],decreasing=FALSE)
    y_upper[k] = temp[cu]
    y_lower[k] = temp[cl]
  }
  jpeg(paste(plot_name,'.jpeg',sep=''), width=4.25, height=3.25, units="in", res=1000, pointsize=4)
  plot(x,f,type='l',col='black',ylim=c(-4,8))
  lines(x,y_lower,lty=2,col='red')
  lines(x,y_upper,lty=2,col='red')
  dev.off()
}