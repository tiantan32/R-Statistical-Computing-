f<-function(x){ 0.2*x*exp(-(x-4)) }
g1<-function(x){ 0.5*exp(-0.5*(x-4)) }
g2<-function(x){ 0.2*exp(-0.2*(x-4)) }
g3<-function(x){ 1/(0.078*pi*(1+x^2)) }

rej_sample<-function(N){
  Y=-2*log(2*runif(N,0,0.5))+4
  Z1=runif(100)*2*g1(Y)
  Z2=runif(100)*4*g2(Y)
  Z3=runif(100)*3.332601*g3(Y)
  Accept1=Z1<f(Y)
  Accept2=Z2<f(Y)
  Accept3=Z3<f(Y)
  
  X1=Y[Accept1]
  X2=Y[Accept2]
  X3=Y[Accept3]
  rejected=list(x1=(N-length(X1)),x2=(N-length(X2)),x3=(N-length(X3)))
  
  return(rbind(rejected))
}
  
rej_sample(1000)


# Cite: work with Rui Chen, rc687