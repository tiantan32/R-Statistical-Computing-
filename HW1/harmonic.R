## a
FindHn<-function(n){
  Hn=0
  for (i in 1:n){
      Hn=Hn+1/i    
  }
  print(Hn)
}



## b
FindHn<-function(n){
  counter=1
  mysum=0
  while(counter){
    mysum=mysum+1/counter
    counter=counter+1
    if (counter>n){
      print(mysum)
      break
    }
  }
}

## c
FindN<-function(B){
  Hn=0
  n=0
  while(Hn<=B){
    n=n+1
    Hn=Hn+1/n
  }
    print(n)
}


## d
## It gradually changes much more slowely after a really long time, because 
## when time goes to infinity, n becomes infinite,and 1/n inclines 
## to 0, but it can never be 0. Thus the sum increases much 
## slowerly after a long time.


## e
## For Loop 1: we can use recursion instead.  
Findhn<-function(n){
  if (n==1){
    Hn=1
  } else{
  Hn=1/n+Findhn(n-1)
  }
  return(Hn)
}  

## For Loop 2: It is impossible to find the smallest n without loop. One recursion is not the proper method here. It might be possible to use recursion twice to get our desired result, but I can't find the method now due to the limited knowledge in recursion. 


