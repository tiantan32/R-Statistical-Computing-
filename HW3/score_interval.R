Score_interval<-function(data,alpha){
  xbar=mean(data)
  n=length(data)
  z=qnorm((1+alpha)/2)
  output=list()
  output$lower=((2*xbar+z^2/n)-sqrt((2*xbar+z^2/n)^2-4*(xbar^2)))/2
  output$upper=((2*xbar+z^2/n)+sqrt((2*xbar+z^2/n)^2-4*(xbar^2)))/2
  return (output)
}