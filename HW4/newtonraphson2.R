Exact_interval<-function(data,alpha,tol=1e-9,max.iter=100){
  return(list(lower = lower, upper = upper))
}


newtonraphson2<-function(ftn, x0, tol = 1e-9, max.iter = 100){
  x=x0
  fx=ftn(x)
  iter=0
  eplison=10^-6/x
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
    fx_eplison=ftn(x+eplison)
    dfx=(fx_eplison[1]-fx[1])/eplison
    x <- x - fx[1]/dfx
    fx <- ftn(x)
    fxeplison <- ftn(x+eplison)
    iter<-iter+1
    cat("At iteration", iter, "value of x is:", x, "\n")
  }
  if (abs(fx[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    cat("Algorithm converged\n")
  }
}