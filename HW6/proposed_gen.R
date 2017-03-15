proposed_gen<-function(num_var, N, lambda, a){
  # num_var: The number of Xmax random variables wanted from the distribution given by FXmax
  # N: The total number of exponential random variables Xi, such that we want to find Xmax = 
  # max{X1  ,...,XN}
  # lambda: The rate of these exponential random variables
  p=1-(1-exp(-lambda*a))
  vec_max=rep(0,num_var)
  p=1-pexp(a,lambda) 
  b=rbinom(1,N,p)
  for (k in 1:num_var){
    if (b==0){
      num=0
      X=0
      while (num!=N){
        fx=runif(1,0,1)
        f_a_bar=fx/(1-p)
        X_i=log((1-p)*f_a_bar/lambda)/(-lambda)
        if (X<a){
          num=num+1
          X[num]=X_i
        } 
      }
      vec_max[k]=max(X)
    } else{
      fx=runif(b,0,1)
      f_a=fx/p
      X=log(p*f_a/lambda)/(-lambda)
      vec_max[k]=max(X)
    }
  }
  return(vec_max)
}



# Cite: work with Rui Chen, rc687