oringCA<-function(filename){
  data = read.csv(filename)
  x=data[,3]  #vector of x
  y=data[,2]>0 #vector of y
  n=length(data[,1])
  ybar=sum(y)/n
  b0=log(ybar/(1-ybar))
  b1=0
  mu=c(b0,b1)
  
  GoldenSection = function(fn,mul,mur,mu,dim,X,Y,tol=1e-8,maxit=10000){
    # Here mu is a vector of inputs into fn, mul and mur are upper and lower
    # values for mu, also given as vectors. However, the function only works
    # on the dimension dim.
    
    gr = (1 + sqrt(5))/2  # Pre-calculate golden ratio
    xl = mu               # We'll set xl, xr and xm to be vectors, but only
    xl[dim] = mul[dim]    # the dim'th entry will be different. Note that in
                          # all our updating rules, the other entries will
    xr = mu               # not be affected.
    xr[dim] = mur[dim]
    
    xm = mu
    xm[dim] =  mul[dim] + (mur[dim]-mul[dim])/(1+gr)
    
    fl = fn(xl,X,Y); fr = fn(xr,X,Y); fm = fn(xm,X,Y)
    
    tol.met = FALSE    # No tolerance met
    iter = 0           # No iterations
    
    while(!tol.met){         # Here we only need to check conditions on the
      iter = iter + 1        # dim'th entry.
      
      if( (xr[dim]-xm[dim]) > (xm[dim]-xl[dim]) ){  # Working on the right-hand side
        y = xm + (xr-xm)/(1+gr); fy = fn(y,X,Y);
        if( fy > fm){ xl = xm; fl = fm; xm = y; fm = fy }
        else{ xr = y; fr = fy }
      }
      else{
        y = xm - (xm-xl)/(1+gr); fy = fn(y,X,Y);
        if( fy > fm){ xr = xm; fr = fm; xm = y; fm = fy }
        else{ xl = y; fl = fy }
      }
      
      if( (xr[dim]-xm[dim]) < tol | iter > maxit ){ tol.met=TRUE }
    }
    return(list(xm=xm,iter=iter))
  }
  
  CoordinateAscent = function(mu,mul,mur,fn,X,Y,tol=1e-8,maxit=10000)
  {
    iter = 0              # Initialization
    tol.met = FALSE
    muhist = c()
    while(!tol.met){  # Tolerance will be checked by how much we move mu
      oldmu = mu      # over one cycle accross the dimensions.
      
      for(dim in 1:length(mu)){    # But we'll update the history at each
        iter = iter + 1            # coordinate
        mu = GoldenSection(fn,mul,mur,mu,dim,X,Y)$xm
        muhist = rbind(muhist,mu)
      }
      
      if( max(abs(mu - oldmu))< tol | iter > maxit){
        tol.met = TRUE
      } else{
        oldmu = mu 
      }
    }
    
    return(list(mu=mu, iter=iter, muhist = muhist))
  }
  
  result=CoordinateAscent(mu,c(-100,-100),c(100,100),loglikelihood,x,y,tol=1e-8,maxit=10000)
  mu_final=result$mu
  b0=mu_final[1]
  b1=mu_final[2]
  num_iter=result$iter
  return(c(num_iter,b0,b1))
}

loglikelihood<-function(mu,x,y){
  l= sum(y*(mu[1]+mu[2]*x)-log(1+exp(mu[1]+mu[2]*x)))
  return (l)
}


