inverse_gen<-function(num_var, N, lambda){
  # num_var: The number of Xmax random variables wanted from the distribution given by FXmax
  # N: The total number of exponential random variables Xi, such that we want to find Xmax = 
  # max{X1,...,XN}
  # lambda: The rate of these exponential random variables
  library(pracma)
  vec_max=rep(0,num_var)
  Fx=runif(num_var,0,1)
  for (i in 1:num_var){
    Fmax=nthroot(Fx[i],N)
    Xmax=log(1-Fmax)/(-lambda)
    vec_max[i]=Xmax
  }
  return(vec_max)
}


# Cite: work with Rui Chen, rc687