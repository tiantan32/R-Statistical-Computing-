FindDeriv<-function(ftn, x){
  fx=ftn(x)
  epsilon=(10^-7)*x
  epsilon2=(10^-7)*(x+24*epsilon)
  if (x==0){
    epsilon=10^-6
    epsilon2=10^-6*(1+24*epsilon)
  }
  dfx=(ftn(x+epsilon)-ftn(x))/epsilon
  ddfx=((ftn(x+epsilon+epsilon2)-ftn(x+epsilon))/epsilon2-dfx)/epsilon
  return(c(fx,dfx,ddfx))
}

newtonraphson2<-function(ftn, x0, tol = 1e-9, max.iter = 100){
  x=x0
  fx=FindDeriv(ftn,x)
  iter=0
  eplison=10^-6/x
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
    x=x - fx[1]/fx[2]
    fx=FindDeriv(ftn,x)
    iter=iter+1
  }
  if (abs(fx[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    return(c(x,iter))
  }
}


Exact_interval<-function(data,alpha,tol=1e-9,max.iter=100){
  possionupper<-function(nmu,value=data,ALPHA=alpha){
    X=sum(value)
    return(ppois(X,nmu)-0.5*dpois(X,nmu)-ALPHA/2)
  }
  possionlower<-function(nmu,value=data,ALPHA=alpha){
    X=sum(value)
    return(1-ppois(X,nmu)+0.5*dpois(X,nmu)-ALPHA/2)
  }
  n=length(data)
  X=sum(data)
  lower_output=newtonraphson2(possionlower,X)
  upper_output=newtonraphson2(possionupper,X)
  lower=lower_output[1]/n
  upper=upper_output[1]/n
  return(list(lower = lower, upper = upper))
}

#cite: work with Rui Chen

