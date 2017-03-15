LPmodel<-function(Y,N,tol=1e-5,maxiter=100){
  ## compute starting values inside the function
  ## del=change in sum of squares between iterations
  beta0=1.95550
  beta1=0.00475
  Nmax=max(N)-5
  theta=c(beta0,beta1,Nmax)
  
  fn<-function(t,x){
    # t is theta, x is the function variable
    return(t[1]+t[2]*pmin(x,t[3]))
  }
  dfn<-function(t,x){
    return(cbind(1,pmin(x,t[3]),t[2]*((x>=t[3])==TRUE)))
  }
  
  
  tol.met=FALSE
  iter=0
  del=0
  while(!tol.met){
    iter=iter+1
    oldtheta=theta
    f=fn(theta,N)
    g=dfn(theta,N)
    theta=theta+solve(t(g)%*%g,t(g)%*%(Y-f))
    fnew=fn(theta,N)
    del[iter]=sum((Y-f)^2)-sum((Y-fnew)^2)
    if (max(abs(theta-oldtheta))<tol|iter> maxiter){
      tol.met=TRUE
    }
  }
  return(list(b0=theta[1],b1=theta[2],Nmax=theta[3],iter=iter,del=del))
}