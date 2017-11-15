# ================ CORE FUNCTIONS FOR MODULE FOUR =================

# Dependencies : my_combination, my_factorial
# Dependency modules : Module 3

# Note: Should be okay, as global include handles dependecies which are from lower modules

## =========================== Uniform ======================================


# total : Total No of discrete groups
# k : upto which group
# cond : k< total
my_discrete_uniform<-function(total,k) {
  
  if(k<=total){
    result<-NULL
    result<-k/total
    return(result)
  }
  else{
    return("Error : K must be less than equal to total")
  }
  
  
}

## =========================== bernoulli ======================================

# p: prob
# x : flag drop down for 0 or 1

my_bernoulli<-function(p,x) { 
  
  if(prob<=1 && prob>=0){
    result<-NULL
    if(x==1) {
      result<-p
    }
    else {
      result<-1-p
    }
    
    return(result)  
  }
  else{
    return("Error : Probability should be between 0 and 1")
  }
  
}
  
## =========================== Binomial ======================================


# total : n 
# fav : r
# prob : prob of success (p)

my_binomial<-function(fav,total,prob) { 
  
  # Inbuilt : dbinom()
  
  if(prob<=1 && prob>=0){
    
    if(fav<=total){
      probability<-0
      
        probability<-probability + 
          my_combination(total,fav)*(prob**fav)*((1-prob)**(total-fav)) #using combination function
      
      result <- as.numeric(formatC(probability,digits = 6,format = "f"))
      return(result)
    }else{
      return("Error : r should be less than equal to n")
    }
    
  }
  else{
    return("Error : Probability should be between 0 and 1")
  }
  
}


## =========================== Negative Binomial ( separate concept ) ======================================

# total : n 
# fav : r
# prob : prob of success (p)

my_negative_binomial<-function(fav,total,prob) {
  if(prob<=1 && prob>=0){
    
    if(fav<=total){
      probability<-my_binomial(fav-1,total-1,prob)   # using my_binomial function
      probability<-probability*prob
      result <- as.numeric(formatC(probability,digits = 6,format = "f"))
      return(result)
    }
    else{
      return("Error : r should be less than equal to n")
    }
    
  }
  else{
    return("Error : Probability should be between 0 and 1")
  }
}

## =========================== geometric ======================================

#prob=prob of success
# xth trial : at which attemp user succeed , x>0
my_geometric<-function(x,prob) {
  
  if(prob<=1 && prob>=0){
    probability<-0
    for ( i in 1:x ) {
      probability<- probability + (prob * ((1-prob)**(i-1)))
    }
    result <- as.numeric(formatC(probability,digits = 6,format = "f"))
    return(result)
  }
  else{
    return("Error : Probability should be between 0 and 1")  
  }
  
  
}


## =========================== hyper geometric ======================================

#N=total number
#M=success out of N
# N>=M

#x=that goes from 0 till n
#n=total cases to examine
# x>=n

my_hyper_geometric<-function(N,n,M,x) {
  
  if(prob<=1 && prob>=0){
    temp<-0
    probability<-0
    i<-x
      probability <- probability  + my_combination(M,i)*my_combination(N-M,n-i)/my_combination(N,n) #using combination function
    	
      result <- as.numeric(formatC(probability,digits = 6,format = "f"))
      return(result)
  }
  else{
    return("Error : Probability should be between 0 and 1")  
  }
}

## =========================== Multivariate Hypergeometric ======================================

#x must be multiple input
#M must be multiple input
#x[i] must be less than or equal to M[i]

# standard -------------------

my_multivariate_hyper_geometric<-function(M,D,n,x) {
  
  # test case
  # M<-10
  # D<-c(1,2,3,4)
  # n<-3
  # x<-c(0,1,0,2)
  
  if ( length(x) != length(D) ) {
    return("Error : Length of both data must be equal")
  }
  if(M!= sum(D)){
    return("Error : Favourable outcomes from subgroups must be equal to total favourable outcomes in dataset")
  }
  if(n!=sum(x)){
    return("Error : Wanted favourable outcomes must be equal to the draw size")
  }
  
    
  probability<-1
    for( i in 1:length(x) ) {
      probability<-probability*(my_combination(D[i],x[i])) 	# using my_combination function
    }
    
    probability<-probability/(my_combination(M,n))			    # using my_combination function
    
    return(probability)
}

## =========================== Poisson ======================================

# Fav
# par=lamda
my_poisson<-function( x , par ) {
  probability<-0
  i<-x
  temp<-(par**(i))*exp(-par)/(my_factorial(i)) # using My_factorial function
    probability<- probability + temp
  
  result <- as.numeric(formatC(probability,digits = 6,format = "f"))
  return(result)
}

        
## =========================== Multinomial ======================================

#data must be multiple input 
#total must be single input
#prob must be multiple input

# data - vector /// sum(data)==total
# data : 5,3,1
# prob - corresponding to each data
# 0.5, 0.3, 0.2

my_multinomial<-function(data,total,prob) {
  if ( length(data) == length(prob) ) {
    if ( sum(data) != total)  {
      return("Error : Sum of outcomes of events must be equal to total available outcomes.")
    }
  else if(sum(prob) !=1 ){
    return("Error : Sum of probabilities of events must be 1")
  }
    else {
      n<-length(data)
      probability<-1
      for( i in 1:n) {
        probability<-probability*(my_factorial(data[i]))	#using My_factorial function
      }	
      probability<-(1/probability)*(my_factorial(total))	#using My_factorial function
      for( i in 1:n) {
        probability<-probability*(prob[i]**data[i])
      }
      return(probability)
    }
  }
  return("Length of both data must be equal")
}

