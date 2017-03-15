Mygibbs<-function(N,Y,sens,spec,L){
  #L=chain length
  Pchain=rep(0,L)
  x=rexp(1)
  for (i in 1:L){
    ptp=sens*x/(sens*x+(1-spec)*(1-x))
    ntp=rbinom(1,Y,ptp)
    ptn=spec*(1-x)/(spec*(1-x)+(1-sens)*(x))
    ntn=rbinom(1,N-Y,ptn)
    x=rbeta(1,ntp+N-Y-ntn+1,ntn+Y-ntp+1)
    Pchain[i]=x
  }
  return(Pchain)
}


