# ================ CORE FUNCTIONS FOR MODULE ONE =================


## =========================== MEAN ======================================

my_mean<-function(x){
  
  # OUR LOGIC -
  result<- (sum(x)/length(x))
  
  # In built function
  #result<- mean(x)
  
  return(result)
}

## =========================== MEDIAN ======================================

my_median<-function(x){
  
  # OUR LOGIC -
  result<-NULL
  temp<-sort(x)
  n<-length(temp)
  if(n %% 2 == 1) {
    result<- ( temp[(n+1)/2] )
  }
  else {
    result<- ( 0.5 * ( temp[n/2] + temp[(n/2)+1] ) )
  }
  
  # In built function
  #result<-median(x)
  
  return(result)
}


## =========================== MODE ======================================

my_mode<-function(x){
  
  # later on implement your own logic
  result<-NULL
  #result<-mode(x)
  return(result)
}



# similarly others