Score_interval<-function(data,alpha){
  browser()
  xbar=mean(data)
  n=length(data)
  z=qnorm((1+alpha)/2)
  output=list()
  output$lower=((2*xbar+z^2/n)-sqrt((2*xbar+z^2/n)^2-4*(xbar^2)))/2
  output$upper=((2*xbar+z^2/n)+sqrt((2*xbar+z^2/n)^2-4*(xbar^2)))/2
  return (output)
}


LR_interval<-function(data,alpha,tol=1e-9,max.iter=100){
  if (all(data==0)){
    cat("All the data values are zero\n")
    stop("function stops")
  }
  if (any(data%%1!=0)){
    cat("Some of the values are not integers\n")
  }
  if (any(data<0)){
    cat("There are negetive values\n")
    stop("function stops")
  }
  output=Score_interval(data,alpha)
  lower=output$lower
  upper=output$upper
  xbar=mean(data)
  z=qnorm((1+alpha)/2)
  n=length(data)
  fx_lower=-2*n*(xbar*log(lower/xbar)+xbar-lower)-z^2
  fx_upper=-2*n*(xbar*log(upper/xbar)+xbar-upper)-z^2
  i=0
  m=0
  k=0
  iter=0
  while((abs(fx_upper)>tol) && (abs(fx_lower)>tol) && (iter<max.iter)){
    dfx_lower=-2*n*(xbar/lower-1)
    dfx_upper=-2*n*(xbar/upper-1)
    lower=lower-fx_lower/dfx_lower
    upper=upper-fx_upper/dfx_upper
    iter=iter+1
    fx_lower=-2*n*(xbar*log(lower/xbar)+xbar-lower)-z^2
    fx_upper=-2*n*(xbar*log(upper/xbar)+xbar-upper)-z^2
  }
  return(list(lower = lower, upper = upper))
}

#cite: work with Rui Chen
