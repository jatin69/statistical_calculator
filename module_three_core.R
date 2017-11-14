# ================ CORE FUNCTIONS FOR MODULE THREE =================


## =========================== factorial ======================================

my_factorial<-function(number){
  if(number==0 || number==1){
    return(1)
  }
  prod <- 1
  for( i in 1:number){
    prod <- prod*i
  }
  return(prod)
}

## =========================== permutations and combinations ======================================

my_permutation<-function(n,r){
  
  result <- my_factorial(n)/ my_factorial(n-r)
  return(result)
  
}

my_combination<-function(n,r){
  
  result <- my_factorial(n)/ (my_factorial(r)*my_factorial(n-r))
  return(result)
  
}

## =========================== Probability ======================================

my_basic_prob <-function(favorable,total){
  
  result<- (favorable/total)
  return(result)
}


# Finds P(A|B) => Probability of A given B
my_cond_prob <- function(A,B,S){

  # A : fav outcomes for event A
  # B : fav outcomes for event B
  # S : Sample space
    
  # Obtain Intersecting elements
  A_intersection_B <- A[A%in%B]
  
  # calculate conditional probability
  cond_prob<- length(A_intersection_B)/length(B)
  
  return(cond_prob)
}


## =========================== Bayes's theorm ======================================


my_bayes_theorm <- function(An,Bn_given_An,k){
  
  # An<-c(.30,.30,.40)      # events
  # Bn_given_An<-c(0.20, 0.15, 0.10)  # hypothesis | event
  # k<-3 # kth event
  
  # An<-c(.51,.49)      # events
  # Bn_given_An<-c(0.095, 0.017)  # hypothesis | event
  # k<-1 # kth event
  # 
  num<- An[k] * Bn_given_An[k]
  n<- length(An)
  
  den<-0
  for(i in 1:n){
    den <- den + (An[i] * Bn_given_An[i])
  }
  
  result<- num/den
  return(result)
}

