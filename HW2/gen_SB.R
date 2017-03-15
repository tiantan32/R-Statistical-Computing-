gen_SB<-function(p,k,s){
# generate R from SB(s) 
  if (s==1){
    r=runif(p*k,-1,1)
    R=matrix(abs(r)/r,p,k)
  }
  if (s>1){
    r=1*sqrt(s)
#  r should be related to uniform distribution   
    r=r*trunc(runif(p*k,-1-1/(s-1),1+1/(s-1)))
    R=matrix(r,p,k)
  }
  return (R)
}



