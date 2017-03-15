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

#cite: work with Rui Chen 