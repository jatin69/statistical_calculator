# ================ CORE FUNCTIONS FOR MODULE ONE =================


## =========================== MEAN, MODE, MEDIAN ======================================

my_mean<-function(x){
  
  # In built function
  #result<- mean(x)

  # LOGIC -
  result<- (sum(x)/length(x))
  result <- as.numeric(formatC(result,digits = 6,format = "f"))
  return(result)
}

my_median<-function(x){
  
  # In built function
  #result<-median(x)
  
  
  # LOGIC -
  result<-NULL
  temp<-sort(x)
  n<-length(temp)
  if(n %% 2 == 1) {
    result<- ( temp[(n+1)/2] )
  }
  else {
    result<- ( 0.5 * ( temp[n/2] + temp[(n/2)+1] ) )
  }
  
  result <- as.numeric(formatC(result,digits = 6,format = "f"))
  return(result)
}

my_mode<-function(data){

  # In built function
  # Not Available in R
  
  # Logic -
  result<-NULL
  
  # get unique data items
  unique_data <- unique(data)
  
  # we gotta find their count and store it
  count <- array()
  
  # For each unique element, find its occurences and store in count
  for ( i in 1:(length(unique_data))){
    count[i] <- length(grep(unique_data[i],data))
  }  
  
  # No mode
  if(max(count)==1){
    result<- "Does not exist"
  }
  else{
    mode<-unique_data[which(count == max(count))]
    
    if(length(mode)!=1){
      result<-"Does not exist"
    }
    else{
      result<-mode
    }
  }
  
  return(result)
}

## =========================== MIN, MAX, RANGE ======================================

my_min<-function(data){

  # In built function -
  # min(data)
  
  # Logic
  result<-NULL
  sorted<- sort(data)
  result<- sorted[1]
  return(result)
}

my_max<-function(data){
  
  # In built function -
  # max(data)
  
  # Logic
  result<-NULL
  sorted <- sort(data)
  result <- sorted[length(data)] 
  return(result)
}

my_range<-function(data){
  
  # In built function -
  # range(data)
  
  # Logic
  result<-NULL
  sorted_data <- sort(data)
  result <- c(sorted_data[1],sorted_data[length(data)])
  
  return(result)
}

## =========================== Variance and Standard Deviation ======================================

# when we are given a data set, say
# array <- c(1,2,3,4,9,4,2,3,6)
# we can treat it in two ways

# Case 1 :  A self sufficient sample, which is infact, all the population
# In this case, we calculate sample variance
# Here, sample and population variance are same.

# Case 2. A sample from an unknown size population. 
# In this case, we assume it coming from a big population
# Here, sample and population variance will differ.


my_sample_variance<-function(array){
  
  # In built R function : var(array)
  
  result<-NULL
  temp<-0
  total<-length(array)
  mean<-my_mean(array)
  for(i in 1:total){
    temp <- temp + (array[i]-mean)*(array[i]-mean)
  }
  
  # dividing by n-1, to provide an unbiased estimate of population size
  answer<- temp / (total-1)	
  result <- as.numeric(formatC(answer,digits = 6,format = "f"))
  return(result)
}

my_sample_SD<-function(array){
  answer<-sqrt(my_sample_variance(array))
  result <- as.numeric(formatC(answer,digits = 6,format = "f"))
  return(result)
}

my_population_variance<-function(array){
  result<-NULL
  temp<-0
  total<-length(array)
  mean<-my_mean(array)
  for(i in 1:total){
    temp <- temp + (array[i]-mean)*(array[i]-mean)
  }
  
  # dividing by n, as the population size is already known
  result <- temp / total	
  result <- as.numeric(formatC(result,digits = 6,format = "f"))
  return(result)
}


my_population_SD<-function(array){
  result<-sqrt(my_population_variance(array))
  result <- as.numeric(formatC(result,digits = 6,format = "f"))
  return(result)
}

## =========================== Mean Absolute Devation ======================================

my_mad<-function(array){
  
  # Inbuilt : mad(array) : result does not match inbuilt's output
  
  result<-NULL
  temp<-0
  total<-length(array)
  mean<-my_mean(array)
  for(i in 1:total){
    temp <- temp + abs(array[i]-mean)
  }
  result <- temp / total	
  result <- as.numeric(formatC(result,digits = 6,format = "f"))
  return(result)
}

## =========================== Quartiles and IQR ======================================

# We can use either of 3 approcahes
# length = (n-1) or (n) or (n+1)
# as the size of data set increases, the difference between the 3 algorithm diminishes.

# These functions Calculates quartiles of a raw data
# Here, We use length = n+1 approach, as it is rooted at the base of index base=1 languages

my_first_quartile<-function(arr) {
  array<-sort(arr)
  result<-NULL
  result<-array[(1/4)*(length(array)+1)]
  return(result)
}

my_second_quartile<-function(arr) {
  array<-sort(arr)
  result<-NULL
  result<-my_median(array)
  return(result)
}

my_third_quartile<-function(arr) {
  array<-sort(arr)
  result<-NULL
  result<-array[(3/4)*(length(array)+1)]
  return(result)
}

my_quantile<-function(array){
  
  # Inbuilt : quantile(array)
  
  sorted_array<-sort(array)
  
  q1<-my_first_quartile(sorted_array)
  q2<-my_second_quartile(sorted_array)
  q3<-my_third_quartile(sorted_array)
  
  return(c(q1,q2,q3))
}

# This function Calculates Inter Quartile Range of a raw Data
my_IQR<-function(arr) {
  
  # Inbuilt : IQR(array)
  array<-sort(arr)
  q3<-my_third_quartile(array)
  q1<-my_first_quartile(array)
  return(q3-q1)
}

## =========================== Moments - raw and central ======================================

# library(e1071)                    # load e1071
# d<-c(1:100)
# moment(d, order=1, center=FALSE)


# This function Calculates central moments of a raw Data
my_central_moments<-function(array, r ) {
  # moments around mean
  
  # In built 
  # library(e1071)                    # load e1071
  # d<-c(1:100)
  # moment(d, order=2, center=TRUE) 
  
  
  result<-NULL
  total<-length(array)
  mean<-my_mean(array)
  if( r == 0 ) {
    result<-1
  }
  else if ( r == 1 ) {
    result<-0
  }
  else {
    temp<-0
    mul<-1
    for(i in 1:total) {
      temp1<-(array[i]-mean)**r
      temp<-temp + temp1
    } 
    result<-temp / total
  }
  return(result)
}


# This function Calculates central moments of A raw Data
my_raw_moments<-function(array , r , A) { 
  
  # moments around arbitrary point (A), taken zero generally
  
  # In built
  # library(e1071)                    # load e1071
  # d<-c(1:100)
  # moment(d, order=1, center=FALSE)
  
  result<-NULL
  total<-length(array)
  if( r == 0 ) {
    result<-1
  }
  else {
    temp<-0
    temp1<-0
    mul<-1
    for(i in 1:total) {
      temp1<-(array[i]-A)**r
      temp<-temp + temp1
    } 
    result<-temp / total
  }
  return(result)
}

## =========================== Skewness and Kurtosis ======================================

#This function Calculates skewness of A raw Data
my_skewness<-function(array) {
 
  # In built
  # library(moments)
  # d<-c(1,1,1,1,4,5,9,9,15,5)
  # skewness(d)
  
  result<-NULL
  temp1<-my_central_moments(array,3)
  temp2<-my_population_SD(array) 
  temp3<-temp2**3
  result<-temp1/temp3
  return(result)
}

#This function Calculates kurtosis of A raw Data
my_kurtosis<-function(array) {
  
  # In built
  # library(moments)
  # d<-c(1,1,1,1,4,5,9,9,15,5)
  # kurtosis(d)
  
  temp1<-my_central_moments(array,4)
  temp2<-my_population_SD(array) 
  temp3<-temp2**4
  result<- (temp1/temp3) -3
  return(result)
}
