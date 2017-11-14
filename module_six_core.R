# ================ CORE FUNCTIONS FOR MODULE SIX =================

# Dependencies : module 1

## =========================== Z test ======================================


#Take a flag value like if it's value equal to 0 then h1 '=' ,if 1 then h1 '>' and if 2  then '<'
#if it's one tail , pass alpha/2 while calling the function and ask to user regarding one tail things

my_z_test<-function(avg,mean,populationSD,n,alpha,flag) {
  
  # avg=xbar
  # populationSD (sigma)
  # n is size of sample
  # alpha is level of significance

    temp<-sqrt(n)
  z<-temp*(avg-mean)/populationSD
  pvalue<-pnorm(z)
  if( z > 0 ) {
    pvalue<-1 - pvalue
  }
  if( flag == 0 ) {     # flag==0 means two tail cases
    pvalue<-2*pvalue    # two tail case
  }
  result <- as.numeric(formatC(pvalue,digits = 6,format = "f"))
  return(result)
}


## =========================== Student t test ======================================

#Take a flag value like if it's value equal to 0 then h1 '=' ,if 1 then h1 '>' and if 2  then '<'
#if it's one tail , pass alpha/2 while calling the function and ask to user regarding one tail thing

my_t_test<-function(data,mean,alpha,flag) {
  avg<-my_mean(data)   #using Mean Function
  n<-length(data) 
  sSD<-my_sample_SD(data) # using SampleSD function
  if ( n < 30 ) {
    temp<-sqrt(n)
    t<-(temp*(avg-mean))/sSD #t value
    pvalue<-pt(t,n-1)
    if( t > 0 ) {
      pvalue<-1-pvalue
    }
    if(flag == 0 ) {
      pvalue<-2*pvalue
    }
    result <- as.numeric(formatC(pvalue,digits = 6,format = "f"))
    return(result)
  }
  else {
    return(my_z_test(avg,mean,sSD,n,alpha,flag))
  }
}

## =========================== F test ======================================

# direct diaplay i=of accept or reject

my_f_test<-function(data1,data2,alpha) {
  sVar1<-my_sample_variance(data1)
  sVar2<-my_sample_variance(data2)
  n1<-length(data1)
  n2<-length(data2)
  f<-sVar1/sVar2
  qvalue1<-qf(alpha,n1-1,n2-1)
  qvalue2<-qf(1-alpha,n1-1,n2-1)
  if(f > qvalue1  && f < qvalue2 ) {
    return("Do not Reject NULL Hypothesis") #reject null hypothests
  }
  return("Reject NULL Hypothesis")
}

## =========================== Chi square test ======================================

#if it's one tail , pass alpha/2 while calling the function and ask to user regarding one tail things
#need to calculate sample variance to compute chisquare value

# P var : sigma sq
my_chi_square_test<-function(data,pVariance,alpha,flag) {
  
  # sVariance : s sq
  sVariance<-my_sample_variance(data)
  df<-length(data)-1
  chi<-df*sVariance/pVariance
  pvalue<-pchisq(chi,df)
  if(flag==0) {
    pvalue<-2*pvalue
  }
  result <- as.numeric(formatC(pvalue,digits = 6,format = "f"))
  return(result)
}


## =========================== Shapiro Wilk test ======================================

# Inbuilt

my_shapiro_test<-function(data,alpha){
  
  # Inbuilt
  #p value : shapiro.test(c(1:100))[2]
  
  pvalue<-shapiro.test(data)[2]$p.value
  result <- as.numeric(formatC(pvalue,digits = 6,format = "f"))
  return(result)
}

