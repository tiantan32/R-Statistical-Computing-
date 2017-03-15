Finv<-function(u){
  # u is a sequence of random variables
  x=rep(length(u))
  for (i in 1:length(u))
    if (u[i]<=1/2){
      x[i]=sqrt(2*u[i])
    } else{
      x[i]=2-sqrt(2-2*u[i])
    }
  return(x)
}


# Cite: work with Rui Chen, rc687