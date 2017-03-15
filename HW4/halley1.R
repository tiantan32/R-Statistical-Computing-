halley1<-function(ftn,x0,tol,max.iter){
  x<-x0
  fx<-ftn(x)
  iter<-0
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
    iter<-iter+1
    x<-x - fx[1]/(fx[2]-fx[1]*fx[3]/(2*fx[2]))
    fx<-ftn(x)
  }
  if (abs(fx[1]) > tol) {
    return(c('NA',-1))
  } else {
    return(c(x, iter))
  }
}


