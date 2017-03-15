polyfunction<-function(x,vec,type){
  len=length(vec)
  if (type=="direct"){
    f=0
    for (i in 1:len){
      f=f+(x**(i-1))*vec[i]
    }
  }
  if (type=="horner"){
    if (len==1){
      f=vec[1]
    } else{
      f=vec[1]+x*polyfunction(x,vec[-1],type)
    }
  }
  if (type=="factor"){
    f=vec[1]
    for (i in 2:len){
      f=f*(x-vec[i])
    }     
  }
  return (f)
}

