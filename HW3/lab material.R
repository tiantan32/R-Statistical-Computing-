## Lab 3

## By the end of this lab, lectures, and   
## by doing homework 2, everyone should    
## have the skills to run simulations      
## in R                                    

## This means:                             
## + Being able to translate ideas into R 
##   code and see if they are feasible     
##   (evaluate hypotheses)                 

## + Being able to read about a new        
##   algorithm or model or technique in a  
##   textbook or paper, and run            
##   simulations to verify its correctness 
##   (test hypotheses and see if true)     

## + Being able to publish papers and not  
##   get laughed at by people (debatable)  

## We'll do two examples in this lab     
## : Speed of calculating (matrix) inverses      
## : A small example on minwise hashing (poll results)!

## Indirectly, we'll cover more R commands


## Matrix inverses                         

## (X^TX)^{-1}X^Ty   <- least squares!     

## Computational complexity of taking the  
## inverse of a matrix is O(n^3)           

## If you haven't seen this notation       
## before, you can google Big O notation. 
## You can take O(f(x)) to (usually) be    
## slower than O(g(x)) if f(x) > g(x) as   
## n-> infty                               
 
## Eg O(n^3) is slower than O(n^2)         
  
## But for the purpose of this lab, take   
## it that computing matrix inverses are   
## slow, and we want to find a way to      
## speed it up                             

## If matrix has some special form,        
## usually faster                          

## Symmetric - half the entries are the    
##             same!                       
## Anti-symmetric - half the entries have  
##                  a different sign       
## Topelitz - rows are shifted by 1        
##   ( a_1 a_2 a_3 )                       
##     a_3 a_1 a_2                         
##     a_2 a_3 a_1                         
##                3 by 3 Topelitz matrix   
## 
## Recursively generated matrices          
##  (Hadamard, Haar, ...)

## Think of this as because entries in the 
## matrix are repeated / can be found from 
## previous matrices, computing an inverse 
## "ought" to be faster                    

## We'll work with symmetric matrices      
## (where do we seem them in statistics?)  

## In this part, we demostrate a faster    
## way of getting the inverse of a matrix, 
## and the procedure for such a            
## demostration                            

## Why is this important?                  

## For those taking the course because     
## they have to                             
## + Homework will be faster to complete   
##   with optimized code                   

## For those taking the course because     
## it's a pre-req                          
## + Optimization is good                  

## For those taking the course for         
## interest's sake                         
## + There's more to finding the inverse   
## than just Gaussian elimination (or LU) 

## For those taking the course for         
## eventual research purposes              
## + Simulations are faster to run, can be 
##   more productive


## A familiar matrix decomposition         

##   A  =  PDP^T    (or PDP^{-1})          
## (do you know the difference?)           

## Cholesky Decomposition                  

## A = GG^T    (Symmetric matrices)        

## G is lower triangular so                
## G^T is upper triangular                 


## Thus, we will look at symmetric         
## matrices in R                           

## Method 1:                               
## solve(X)                                

## Method 2:                               
## chol2inv(chol(X))                       


## Example:

X = matrix(rnorm(16),nrow = 4)             
A = t(X) %*% X   # This is symmetric       

A_inv1 = solve(A)                          

## 

chol(A)  # look at its output!             

t(chol(A)) %*% chol(A) - A  ## verify      

## i.e. if G^T = chol(A)
#

norm(t(chol(A)) %*% chol(A) - A) ## better 

## So if we think of chol(A) giving us G^T 

# solve gives the inverse of the matrix

A_inv2 = chol2inv(chol(A))                 

norm(A_inv1 - A_inv2) # verify             

## How can we check which is faster?       

## Denoting N to be the dimension of the   
## symmetric matrix X                      
## We will generate random matrices        
## of size N by N                          
## where N ranges from 1 to 1000           
## and compare the time taken for          
## solve(X)                                
## and                                     
## chol2inv(chol(X))                       

## In pseudo code we will do               

##  Initialize timevec1, timevec2 to zeros
##  For N = 1, 2, .... 1000               
##    For k = 1, ... , K                   
##      Generate random symmetric          
##        matrices X of size N             
##      Find time taken T1 for solve(X)    
##      Set timevec1[N] = timevec1[N] + T1 
##      Find time taken T2 for chol2inv(chol(X))
##      Set timevec2[N] = timevec2[N] + T2 
##    end                                  
##  end
##  Divide timevec1 by K
##  Divide timevec2 by K   

## Here's the function, we'll go through
## what each line does. This is what you
## can also do when faced with an unfamiliar
## loop. Go through one iteration and see
## what it does.

CompareTime<-function(nvec, avg){
  ## Note, here we don't use 
  ## timevec1 = vector("numeric")
  ## Why? Change the code and see what happens!

  timevec1 = rep(0,length(nvec))  
  timevec2 = rep(0,length(nvec))

  # We now go through each element of nvec

  # Note the i^th element in nvec is the size 
  # of the matrix we want to create

  for (i in 1:length(nvec)){
    N = nvec[i]  # Let's set N to the ith 
                 # element of nvec
    for (j in 1:avg){
	
      # We can't create a N by N symmetric 
      # matrix directly (at least without 
      # using a package) so one way is
	
      # Create a data matrix D of n observations,
      # p parameters where p = N
	
      # Set X = D^T D, i.e. t(D) %*% D in R
      # X is now invertible thus
	
	  n = 1000
	  D = matrix(runif(1000*N), nrow = n, ncol = N)
	
      # X is now our N by N matrix 
	  X = t(D) %*% D
	
      # proc.time()[3] is the system time elapsed
      # since R was first opened
	
	  tic = proc.time()[3]
	  solve(X)
	  toc = proc.time()[3] - tic 
	
      # so toc is the difference in system time
	
	  timevec1[i] = timevec1[i] + toc
	
      tic = proc.time()[3]
      chol2inv(chol(X))
	  toc = proc.time()[3] - tic      
	  timevec2[i] = timevec2[i] + toc      
	}            

    # We don't want to print out every ith  
    # iteration since printing is costly (note
    # here iteration is where n increases)

    # Let's take a return to the mod function, 
    # we print out every 20th iteration (or 
    # every 10th, 50th, 100th) iteration 
    # depending on how comfortable you are

    # Aside: Why do you want to print out
    # iterations, if you tested your code on 
    # small test sets and it works?

    # Sometimes, when values get large, R might
    # take an insanely long time to compute 
    # values, or  get stuck in a loop (rare, 
    # but happens)

    # If your printed counter hasn't been 
    # increasing for a long time it might be a  
    # sign to check your function, rewrite code,
    # not consider that  iteration if it's 
	# computationally costly, etc

    # If we want to print out every 20th 
    # iteration

    if ((i %% 20) == 0){
      print(i)
	  # Actually, let's save our values at every
      # 20th iteration as well

      # Saving: For the obvious reason - you are 
      # running a simulation that takes a few 
      # hours, someone cuts the power

      # Second less obvious reason. You want to
      # run perhaps 10000 iterations. The first 
      # 9900 of them takes a day

      # The last 100 seem to take an increasingly 
      # exponential length of time, and might even 
      # take a week. 

      # Do you stop the function, realize no data 
      # is saved, change the input to take 9900 
      # iterations and wait another day to get 
      # data or just use your saved output instead?

      # Even worse case: Deadline for homework in a 
      # few hours and you don't want to rage-drop 
      # the course....

      # Make sure the directory exists, or at least
      # is writable
      
      # For those using Windows, don't forget a
      # "C:\ ..." or similar
      # Leaving the below commented out because I
      # don't want to overwrite my results

      # save(i, timevec1,timevec2, file = "/Users/blahblah/Desktop/tmp/myresults.rData")
      
      # Question: Why are we saving i as well?
      ## i is the number of iteration, to divide the function
      
      # Comment out your printing (and saving) for
      # code you submit 
	}
  }
  timevec1 = timevec1 / avg
  timevec2 = timevec2 / avg
  ## Can return multiple outputs as a list
  return(list(timevec1 = timevec1, timevec2 = timevec2))
}


nvec = c(2:99, seq(100,200,10), seq(220,400,20), seq(450,1000,50))

CompareTime(nvec,100)

## But whoops, this takes too long.

## Lucky we saved our results...

## Let's read in what we've saved!

results = load("myresults.rData")   

minT = min(timevec1,timevec2)
maxT = max(timevec1,timevec2)

plot(0, main = "Comparison of time taken", ylab = "Time taken(s)", 
  xlab = "Size of X (n by n)", ylim = c(minT,maxT), xlim = c(0,1000),
    type = "n")

lines(nvec, timevec1, col = "red", lty = 1)
lines(nvec, timevec2, col = "blue", lty = 2)

legend("topleft", c("solve(X)", "chol2inv(chol(X))"), lty = c(1,2), 
  col = c("red", "blue"))


## What are your conclusions?

## PS: Homeworks will have questions like: "What are 
## your conclusions?", based on the analysis you do.

## Perfectly okay to say you can't conclude anything
## based on the experiments you do - full points
## will be given for that answer - provided there's
## justification

## Not okay to quote Wikipedia / etc which doesn't
## have any relation to your plots. (eg HW1 when
## asked about similarities between Chernoff Faces
## and PCA)

## If you quote Wikipedia, it means you don't know
## how to interpret your results / would trust an
## outside source rather than what you see. 
		
## Imagine running some analysis on data you have
## but then instead of telling your boss what you
## see, you instead tell him something from Wikipedia...



## Takeaway:

## 1) A loop structure (can use this in the homework) to
##    compare different times of two (or more) methods

## 2) If your matrix is special, usually a quicker way
##    to find its inverse apart from generic (solve)

##    Could google and find packages to do matrix
##    inversion, or code your own in R

##    How to tell which technique is faster for
##    your work? We've just had a loop that does
##    that.

##    Extra References:
##    Matrix Computations (Golub and Van Loan)
##    (talk to Van Loan directly, he's in CS)

## 3) Some (other) matrix decompositions 

##    LU Decomposition
##    Cholesky Decomposition (what we did)
##    Schur Decomposition


## Let's now look at minwise hashing

## Different from hashing first encountered in CS 
## intro courses

## Motivation for people with CS background:
## In CS courses, collisions are inevitable
## (in hash tables) - good hash functions
## reduce collisions

## Here, we look at collision probabilities
## and we are "neutral" to collisions
## If they happen more often, we're fine
## If they don't happen often, we're also fine

## Back to the stats

## This deals with the idea of permutations
## How do we get permutations in R?

## Permutation of length 5

sample(1:5, replace = FALSE)  

## We can permute vectors, given a permutation

set.seed(0)
perm = sample(1:5, replace = FALSE)  
perm
# [1] 5 2 4 3 1

# For a five element vector, this permutation would

# Map the first element to the fifth element
# Map the second element to itself
# Map the third element to the fourth element
# Map the fourth element to the third element
# Map the fifth element to the first element

vec = c(10,20,30,40,50)
vec[perm]
# [1] 50 20 40 30 10

## Let's consider intersection between sets

## \Omega = {0,1,2,3,4,5,6,7,8,9,10}
## Universe has 11 "numbers"

## A = {2,4,5,6,10}
avec = c(2,4,5,6,10)
## B = {0,1,4,8,9,10}
bvec = c(0,1,4,8,9,10)

## |A| = 5
length(avec)
## |B| = 6
length(bvec)


## |A \cup B| = 9       # Set union
length(union(avec,bvec))
## |A \cap B| = 2       # Set intersection
length(intersect(avec,bvec))


## We are interested in the fraction
##   |A \cap B|
##   ----------
##   | A \cup B|
##
## Think of this as intersect over union

## "Set Similarity"
## Eg, numbers could be words, we're checking
## how similar two writeups are (turnitin)?

## Suppose we define a permutation on Omega

set.seed(0)
perm = sample(1:11, replace = FALSE)  

# sample(1:11,replace=FALSE)
# [1]  4  1 10 11  6  2  7  5  9  3  8
# claim: permute all # in the set. Take maxA=maxB 
# A intersection B = 2, A union B = 9

## Permute Avec and Bvec 
avec_new = perm[avec]
bvec_new = perm[bvec]

## Look at the maximum of both sets

max(avec_new)
max(bvec_new)

## If I repeat this often enough, i.e.
## Generate new permutation
## Look at maximums of new sets

## Then the proportion of (equal) maximums
## tend to the fraction 
##   |A \cap B|
##   ----------
##   | A \cup B|

## Do you believe this?


Test_absurd_claim<-function(avec,bvec,N,iter){

	prop_vec = 0
	
	for (i in 1:iter){
	
      # Construct permutation
	  perm = sample(1:N, replace = FALSE)  
      avec_new = perm[avec]
      bvec_new = perm[bvec]
	  
      if(max(avec_new) == max(bvec_new)){
        prop_vec = prop_vec + 1
	  }
	
	}
	
  prop_vec = prop_vec/iter	
  return(prop_vec)
}

## Let's try with universe being {0, 1,2, .. 100}
set.seed(0)
N = 101
avec = sample(1:100, 40)
bvec = sample(1:100, 40)
ratio_avec_bvec = length(intersect(avec,bvec))/ length(union(avec,bvec)) 
ratio_avec_bvec
# [1] 0.2698413

set.seed(0)
## These are point estimates..
Test_absurd_claim(avec,bvec,N,10)
# [1] 0.3
Test_absurd_claim(avec,bvec,N,100)
# [1] 0.22
Test_absurd_claim(avec,bvec,N,100000)
# [1] 0.27051

## Seems like it's true

## Why does taking the permutation guarantee this though?
## Discussion of proof




## Is there a way we can do better?
## Eg, we want to say something like
## If we generate K different permutations
## Take the proportion
## We are guaranteed that 95% of our observed
## proportion lies within this interval
## (ratio - something, ratio + something)
## Lab exercise for the week!


## What's the point of this? If we look at the time
## taken to generate these permutations, it's longer
## than just finding
## length(intersect(avec,bvec)) / length(union(avec,bvec))

# Let our universe be all words
# eg 
# 0 1     10000
# | |       |
# a aa     zebra

## Consider a set of all webpages, and suppose we have
## a table like this:

# number means words, want to find how similar webpages are

#         wp1   wp2   wp3   wp4   wp5 ...

#perm1    6     2     6     1      4
#perm2    7     9     1     1      2
#perm3
..
..
..


## Just need to store maximum value under each permutation

## If a new webpage is added, quickly find the
## max value under permutations, and add a new column

## BUT 
## Why are we not storing pairwise similarities instead, which are exact? Why are we storing value under permutations which give inexact similarities? 

## If we had 10^10 webpages
## added one more
## Have to calculate 10^10 different similarities

## If we stored 1000 permutations
## Just calculate the max val under these 


## Then, assuming this table is called M, and you
## want to find similarity between website i and j

## sum(M[,i] == M[,j])/total_perm

## gives you an estimate of the similarity between
## these two webpages. 

## Can we reduce the size of this table?

## Yes - collision probabilities again!
## Instead of storing maximum values, we hash them
## For the non CS -> we could map these values
## evens to 0s
## odds to 1s

## This gives us a table of 0s and 1s

## How can we reconstruct the max values?

## Think back to the first lab session on Wed where
## I asked everyone to run

## rbinom(1,1,0.5)

## and said: "If it was 0, raise your hand"
## If it was 1, answer the question truthfully and
## raise your hand if you had no programming background"

## Take a look at b-bit minwise hashing
## http://research.microsoft.com/pubs/120078/wfc0398-liPS.pdf

## Despite its complex name, the paper is very
## readable, and doesn't assume any advanced
## math. 

## You should be able to implement the simulations
## in that paper though (if you want). 







## Lab questions - like Lab 1, mostly
## open ended. You can just put your solutions in an R file
## no need to copy the question

## Question 1: We looked at the "hashing" example in
## lab. Do either a) or b) {or both} depending on
## where your strengths are, and c)

## a) For those who are more coding oriented, think of
## a way to run simulations to find this interval.

## Failing that, think of a way to run simulations
## such that you are some % sure that after running
## K such permutations, you are within the true
## ratio with some small error -> say 0.01

## Note this is technically dependent on N, our
## size of our universe. We'll take N = 1000 here.


## b) For those who are stats oriented, note that
## this proportion can be thought of as a Bernoulli
## distribution. Either the max are the same, or
## they are not. 
## Given that we ran K permutations, Write down a CI
## for R and show that most ratios R lie within this
## interval by running simulations. How does this 
## depend on N?

## c) We gave an explanation about storing the table
## of permutations and webpages. This relies on
## us pre-computing the permutation and storing
## the max value. In this case, why can't we just
## compute this pair-wise ratio for every webpage
## and store this pair-wise ratio instead of storing
## a table of max values?


## Question 2

## Investigations 

## Suppose you didn't know anything about MLEs

## You toss a coin 10 times. You see it come up
## H 9 times.

## You want to say something about the probability
## of it coming up H.

## Or rather, you want to find whether it's
## possible to say something about the probability
## of the coin coming up H - note that this is
## different from the above. **

## Here's one way to do this.

## Set prob_vec = c(0.1,0.2,0.3, 0.4, 0.5,0.6,0.7,0.8,0.9,0.95)

## For n = 1000 iterations, simulate tossing
## such a coin with these different probabilities
## in pvec

## (eg generate bernoulli random variable with
## varying p, take 0 as T, 1 as H)

CheckProp<-function(p){
  prop = 0
  for (i in 1:1000){
    ## Because we tossed it 10 times, heads were 9
    if (rbinom(1,10,p) == 9){
      prop = prop + 1
    } 
  }

  return(prop)
}

for (p in prob_vec){
  print(paste("When p =", p, "number of times is", CheckProp(p)))
}

## Is there a probability such that the majority
## of the coin tosses are H?

## You'd find out via simulations that most probably
## this probability will be p =0.9

## In other words, your simulations might tell you that
## for the event {9 out of 10 heads},

## [1] "When p = 0.1 number of times is 0"
## [1] "When p = 0.2 number of times is 0"
## [1] "When p = 0.3 number of times is 0"
## [1] "When p = 0.4 number of times is 4"
## [1] "When p = 0.5 number of times is 13"
## [1] "When p = 0.6 number of times is 42"
## [1] "When p = 0.7 number of times is 112"
## [1] "When p = 0.8 number of times is 283"
## [1] "When p = 0.9 number of times is 386"
## [1] "When p = 0.95 number of times is 321"

## You might not be able to explain why p =0.9
## but you do know that when p =0.9, that's when
## the "most" occurences happen.

## On the other hand, if your simulations gave you
## something like 

## [1] "When p = 0.1 number of times is 20"
## [1] "When p = 0.2 number of times is 19"
## [1] "When p = 0.3 number of times is 21"
## [1] "When p = 0.4 number of times is 19"
## [1] "When p = 0.5 number of times is 18"
## [1] "When p = 0.6 number of times is 22"
## [1] "When p = 0.7 number of times is 20"
## [1] "When p = 0.8 number of times is 21"
## [1] "When p = 0.9 number of times is 21"
## [1] "When p = 0.95 number of times is 22"

## Then you'd probably not conclude anything (i.e.
## we can't find a p).

## To summarize, your thoughts might be of this form:
## Because I ran simulations, and because I see that
## p = 0.9 is when the "most" number of times this
## case occurs, then whenever I see heads 9 times
## out of 10, I will claim that the coin has
## a probability of landing heads of 0.9

## So here are some scenarios. Devise (briefly explain)
## a way to check if it is possible to say "something"
## about the parameter which we are interested in.

## We'll make this "something" more concrete in later 
## labs / lectures

## Eg: I had a coin that came up heads 9 times out of 10.
## Can I say something about the parameter?
## Ans: Yes, could run simulations (eg above), etc etc..

## Scenario 1
## 
## Suppose you play a game, and each time you quit the game
## a motivational message is given.
## You know the motivational message is probably taken from
## a list of N such messages.
## You want to find the length N of such a list.

## You propose to do the following:
## You quit the game, and see message A
## Now, you repeatedly log in and out, until message A is displayed again
## Suppose after K times, you see message A. 

## Is it possible to say something about the length N of this list given K?

## And what are the odds are that you get deleted from the game because
## the admins thought you had special access to find the length N?

## Either describe a way you would code up simulations to do this,
## or just submit the code. Better still, a one line explanation
## of what you see from your results

## Scenario 2
##
## Suppose you had a vector of 0s and 1s with length N
## eg, (0, 0, 1, 1, 1, 0, 0, 0, 0)
## 
## Adam claims: I want to do something similar to the
##              hashing shown in lab.
##
## He says, "I will create a permutation of length N"
##
## He continues, "But this time, I will look at the
##                entries up to the Kth 1.
##
## For example, Suppose I had the vector
##
## vec = c(1,0,0,0,1,1,0,1,0,0,0,0,1,0,1,0)
##
## I get the permutation of length (16)
## set.seed(0)
## perm = sample(16)
## [1] 15  4  6  8 11  3  9 10 14  5  1  2 13 12 16  7
##
## Suppose I define K = 3
## Then, instead of vec[perm] to get
## the entire permutation
## [1] 1 0 1 1 0 0 0 0 0 1 1 0 1 0 0 0

## I construct each value of my new A vec 
## until I see K ones.

## Since I set K = 3

## In this case, I would have stopped at position 4.
## because the third one is at position 4.

## Call this position POS

## I claim: My estimate of the number of 1s in this
## vector is

## D/POS * K

## by simple proportion

## i.e. 16/4 * 3 = 12

## Note that POS will change for every permutation
## Eg, 
set.seed(5)
perm = sample(16)
vec[perm] 
#  [1] 0 0 1 0 0 1 1 1 0 1 0 0 0 0 0 1
# will give POS = 7 since the 3rd 1 is at position 7

# So if I run this repeatedly with multiple permutations
# then I claim that the average of the
# D/POS * K, i.e.
# 1/n * sum_{i=1}^n D/pos_i * K will tend to the number of 1s
# in the vector.

# Devise a way to show whether this claim is true. 

# Suppose this is true ; can there be any practical
# purposes for this claim? 



































