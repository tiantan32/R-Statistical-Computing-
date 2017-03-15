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
    del[iter]=sum((Y-f)^2)
    g=dfn(theta,N)
    theta=theta+solve(t(g)%*%g,t(g)%*%(Y-f))
    if (max(abs(theta-oldtheta))<tol|iter> maxiter){
      fnew=fn(theta,N)
      tol.met=TRUE
    }
  }
  return(list(b0=theta[1],b1=theta[2],Nmax=theta[3],iter=iter,del=del[iter]-del[iter-1]))
}

dataN=data.frame("nitrogen"= c(rep(0,4),rep(30,4),rep(60,4),rep(90,4),rep(120,4)),
                 "yield"=c(1.41,1.75,2.02,2.13,1.93,2.24,2.29,2.35,2.12,2.38,
                           2.49,2.57,2.16,2.20,2.28,2.49,2.34,2.45,2.59,2.62))

LPmodel(dataN$yield,dataN$nitrogen)


m<-function(x,theta3){
  return(min(x,theta3))
}
nls(yield~theta1+theta2*pmin(nitrogen,theta3),data=dataN,start=list(theta1=1.95550,theta2=0.00475,theta3=115))
theta1=1.943
theta2=5.175e-03
theta3=1.077e+02