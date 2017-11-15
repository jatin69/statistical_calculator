# ================ CORE FUNCTIONS FOR MODULE SEVEN =================

#Dependencies : sample_SD , moduleone

## =========================== Estimation of Means ======================================

#avg=xbar
#populationSD is variable name
#n is total value
#alpha is level of significance

# when variance is known
my_est_mean_var_known<-function(avg,populationSD,n,alpha) {  
  temp<-sqrt(n)
  qvalue<-qnorm(alpha/2)		#using qnorm function
  temp2<-qvalue*populationSD/temp 
  interval<-c(avg+temp2,avg-temp2) #calculation interval
  
  return(interval)	#returning interval value
}

# when variance is unknown
my_est_mean_var_unknown<-function(data,alpha) { 
  sSD<- my_sample_SD(data) 		#using SampleSD function
  n<-length(data)
  avg<- my_mean(data)			#using Mean function
  
  #if n < 30  , then use t value
  if ( n < 30 ) {			
    qvalue<-qt(alpha/2,n-1)
    temp2<-qvalue*sSD/sqrt(n)
    interval<-c(avg+temp2,avg-temp2)	#calculation interval
    return(interval)	#returning interval value
  }
  else {
    # if n >= 30 , then use z value
    return(my_est_mean_var_known(avg,sSD,n,alpha))
  }
}

## ====================== Estimation of Difference of Means ==================================

my_est_diff_mean_var_known<-function(avg1,avg2,pVar1,pVar2,n1,n2,alpha) {
    temp<-(pVar1/n1) + (pVar2/n2)
    temp<-sqrt(temp)	#calculation avg population SD
    qvalue<-qnorm(alpha/2)
    temp<-temp*qvalue
    interval<-c(avg1-avg2 + temp,avg1-avg2-temp)	#calculation interval
    return(interval)	#returning interval value
}

my_est_diff_mean_var_unknown<-function(data1,data2,alpha) {
  avg1<-my_mean(data1)		#using Mean Function for data1
  avg2<-my_mean(data2)		#using Mean Function for data2
  n1<-length(data1)
  n2<-length(data2)
  sVar1<-my_sample_variance(data1)	#using SampleVariance Function for data1
  sVar2<-my_sample_variance(data2)	#using SampleVariance Function for data2
  if ( n1 < 30  && n2 < 30 ) {	#if n1 < 30 & n2 < 30  , then use t value
    qvalue<-qt(alpha/2,n1+n2-2)
    temp<-(1/n1)+(1/n2)
    temp<-sqrt(temp)
    avgSD<-((n1-1)*sVar1) + ((n2-1)*sVar2)
    avgSD<-sqrt((avgSD)/(n1+n2-2))      # calculating average Sample SD
    temp<-temp*avgSD*qvalue
    interval<-c(avg1-avg2 + temp,avg1-avg2-temp)	#calculation interval
    return(interval)	#returning interval value
  }
  else {
    my_est_diff_mean_var_known(avg1,avg2,sVar1,sVar2,n1,n2,alpha)	# if n1 >= 30 & n2 >=30, then use z value
  }
}


## =========================== Estimation of Proportions ======================================

# enter fav
# enter total
# alpha

my_est_prop<-function(fav , n , alpha ) {
  
  #calculating favourable probabilty
  prob<-fav/n	
  
  temp2<-(prob*(1-prob))/n
  temp2<-sqrt(temp2)
  qvalue<-qnorm(alpha/2)
  temp2<-temp2*qvalue
  interval<-c( prob + temp2 , prob - temp2 ) 	#calculation interval
  return(interval)	#returning interval value
}

## =================== Estimation of Difference of Proportions =========================

# same as above for 2 people

my_est_diff_prop<-function(fav1,n1,fav2,n2,alpha) {
  
  #calculating favourable probabilty
  prob1<-fav1/n1		
  prob2<-fav2/n2
  
  temp2<- (prob1*(1-prob1)/n1) + (prob2*(1-prob2)/n2)
  temp2<-sqrt(temp2)
  qvalue<-qnorm(alpha/2)
  temp2<-temp2*qvalue
  interval<-c( prob1 - prob2 + temp2 , prob1 - prob2 - temp2 ) #calculation interval
  return(interval)	#returning interval value
}



## =========================== Estimation of variances ======================================

my_est_var<-function(data,pVar,alpha) {
  
  sVar<-my_sample_variance(data)		#using SampleVariance function
  n<-length(data)
  
  qvalue1<-qchisq(1 - (alpha/2) , n-1) 	#calculation chi square value for alpha/2 & and df=n-1
  qvalue2<-qchisq(alpha/2 , n-1)		#calculation chi square value for 1-alpha/2 & and df=n-1
  
  temp<-(n-1)*sVar/qvalue1
  interval<-temp
  temp<-(n-1)*sVar/qvalue2
  
  interval<-c(interval,temp)	#calculation interval
  return(interval)	#returning interval value
}


## ======================= Estimation of Ratio of Two Variances ==========================


my_est_ratio_var<-function(data1,data2,pVar1,pVar2,alpha) {
  
  sVar1<-my_sample_variance(data1) 	#using SampleVariance function
  sVar2<-my_sample_variance(data2)	#using SampleVariance function
  
  n1<-length(data1)
  n2<-length(data2)
  
  qvalue1<-qf(1-(alpha/2) , n1-1 , n2-1)	#calculation f value for df1=n1-1 and df2=n2-1
  qvalue2<-qf(1-(alpha/2) , n2-1 , n1-1)	#calcualtion f value for df1=n2-1 and df2=n1-1
  
  temp<-(sVar1/sVar2)*(1/qvalue1)
  interval<-temp
  temp<-(sVar1/sVar2)*(qvalue2)
  
  interval<-c(interval,temp)	#calculation interval
  return(interval) 	#returning interval value
}
