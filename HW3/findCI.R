findCI<-function(word1, word2, nump, alpha){
   output=list()
   word1=read.table(word1)
   word2=read.table(word2)
   A_cap_B=intersect(word1[,2],word2[,2])
   A_cup_B=union(word1[,2],word2[,2])
   output$res=length(A_cap_B)/length(A_cup_B)
   prob=0
   for (i in 1:nump){
     perm = sample(1:65536, replace = FALSE) 
     A=perm[word1[,2]]
     B=perm[word2[,2]]
     if (max(A)==max(B)){
           prob=prob+1
     }
   }
   prob=prob/nump
   output$phat=prob
   z=qnorm(1-alpha/2)
   output$l_int=prob-z*sqrt(prob*(1-prob)/nump)
   output$r_int=prob+z*sqrt(prob*(1-prob)/nump)
   return(output)
}

