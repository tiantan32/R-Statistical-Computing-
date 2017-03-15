Findx<-function(A,b){
  #A is a n by n square matrix
  #b is a n by 1 column vector
  #x is a n by 1 column vectors such that A*x=b
  #You may use the package {\tt matrixcalc} or similar
  #but only to find the LU decomposition of a square matrix
  library(matrixcalc)
  LU_A=lu.decomposition(A)
  L=LU_A$L
  U=LU_A$U
  n=length(b)
  for (i in 1:n){
    if (L[i,i]==0 | U[i,i]==0 ){
      print("There is no solution")
      return(NULL)
    }
  }
  y=rep(0,n)
  y[1]=b[1]/L[1,1]
  for (i in 2:n){
    y[i]=b[i]
    for (j in 1:(i-1)){
      y[i]=y[i]-L[i,j]*y[j] 
    }
    y[i]=y[i]/L[i,i]
  }
  x=rep(0,n)
  x[n]=y[n]/U[n,n]
  for (i in (n-1):1){
    x[i]=y[i]
    for (j in n:(i+1)){
      x[i]=x[i]-U[i,j]*x[j] 
    }
    x[i]=x[i]/U[i,i]
  }
  return(x)
}