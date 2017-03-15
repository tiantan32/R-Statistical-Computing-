oringNR<-function(filename){
  data = read.csv(filename)
  x=data[,3]  #vector of x
  y=data[,2]>0  #vector of y
  n=length(data[,1])
  ybar=sum(y)/n
  b0=log(ybar/(1-ybar))
  b1=0
  mu=c(b0,b1)
  
  NewtonRaphson2 = function(mu,dfn,d2fn,X,Y,tol=1e-8,maxit=100){
    tol.met=FALSE; iter = 0; iterhist = mu
    result=list()
    while(!tol.met){
      result[iter]=list(c(iter,mu))
      iter = iter + 1
      oldmu = mu
      g = dfn(mu,X,Y)            # Gradient
      H = d2fn(mu,X,Y)           # Hessian
      mu = mu - solve(H,g)     # Update
      iterhist = rbind(iterhist,mu)
      
      if( (max(abs( mu-oldmu )) < tol & max(abs(g)) < tol) | iter > maxit){
        tol.met=TRUE 
      }
    }
    return(result)
  }
  
  result=NewtonRaphson2(mu,dfn,d2fn,x,y,tol=1e-8,maxit=100)
  result=data.frame(result)
  histOR=t(result)
  iter=length(histOR[,1])
  colnames(histOR)=c("iter","beta0","beta1")
  rownames(histOR)=c(1:iter)
  return(histOR)
}

dfn = function(mu,x,y){
  l= sum(y*(mu[1]+mu[2]*x)-log(1+exp(mu[1]+mu[2]*x)))
  dx1 = sum(y-exp(mu[1]+mu[2]*x)/(1+exp(mu[1]+mu[2]*x)))
  dx2 = sum(y*x-x*exp(mu[1]+mu[2]*x)/(1+exp(mu[1]+mu[2]*x)))
  return(c(dx1,dx2))
}

d2fn = function(mu,x,y){
  dx11 = sum(-exp(mu[1]+mu[2]*x)/(1+exp(mu[1]+mu[2]*x))^2)  # Second derivatives
  dx12 = sum(-x*exp(mu[1]+mu[2]*x)/(1+exp(mu[1]+mu[2]*x))^2)
  dx21 = sum(-x*exp(mu[1]+mu[2]*x)/(1+exp(mu[1]+mu[2]*x))^2)
  dx22 = sum(-x^2*exp(mu[1]+mu[2]*x)/(1+exp(mu[1]+mu[2]*x))^2)
  return(matrix( c(dx11,dx12,dx21,dx22),2,2))
}