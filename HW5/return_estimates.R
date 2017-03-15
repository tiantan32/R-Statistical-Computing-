return_estimates<-function(word1, word2, seed, sample_size){
  set.seed(seed)
  docs = sample(0:65535,sample_size)
  Word1=read.table(word1)
  Word2=read.table(word2)
  M1=length(Word1[,2])
  M2=length(Word2[,2])
  m1=sum(is.element(Word1[,2],docs))
  m2=sum(is.element(Word2[,2],docs))
  word_1=intersect(Word1[,2],docs)
  word_2=intersect(Word2[,2],docs)
  N=65536
  n=sample_size
  n1= sum(is.element(word_1,word_2))
  n2= m1-n1
  n3= m2-n1
  n4= n-n1-n2-n3
  
  newtonraphson1<-function(ftn, x0, tol = 1e-9, max.iter = 1000000){
    x=x0
    fx=ftn(x)
    iter=0
    while ((abs(fx[1]) > tol) && (iter < max.iter)) {
      x <- x - fx[1]/fx[2]
      fx <- ftn(x)
      iter<-iter+1
    }
    if (abs(fx[1]) > tol) {
      cat("Algorithm failed to converge\n")
      return(c("NA",-1))
    } else {
      return(c(x,iter))
    }
  }
  
  likelihood<-function(pi){
    fx=n1/pi-n2/(M1/N-pi)-n3/(M2/N-pi)+n4/((N-M1-M2)/N+pi)
    epsilon=10^-6
    fx_epsilon=n1/(pi+epsilon)-n2/(M1/N-(pi+epsilon))-n3/(M2/N-(pi+epsilon))+
      n4/((N-M1-M2)/N+(pi+epsilon))
    dfx=(fx_epsilon-fx)/epsilon
    return (c(fx,dfx))
  }
  

  result=newtonraphson1(likelihood,n1/n)[1]
  N1_hat=result*N
  N2_hat=M1-N1_hat
  N3_hat=M2-N1_hat
  N4_hat=N-M1-M2+N1_hat
  
  return(c(N1_hat, N2_hat, N3_hat, N4_hat))
}

