# ================ CORE FUNCTIONS FOR MODULE EIGHT =================


## =========================== My sign Test ======================================

# #flag=0 '=' case
# #flag=1 '<' case
# #flag=2 '>' case

my_sign_test<-function(data , mean , alpha , flag ) {
  x<-0				
  discard<-0			
  n<-length(data)			# total values
  for ( i in 1:n ) {
    if( data[i] > mean ) {
      x<-x+1			#count values > than mean
    }
    if( data[i] == mean ) {
      discard<-discard + 1	#count values = to mean
    }
  }
  n<- n - discard				#total values after discarding
  if( n < 30 ) {				# if n < 30 use binomial distribution
    temp<-my_binomial( x , n , 0.5) 	#using binomial function
    if(flag==1) {
      temp<-1-temp
    }
    if ( flag==0 ) {
      temp<-2*temp
    }
    return(temp)
  }
  else {				# if n>=30 , use normal distribution
    pSD<-sqrt(n*0.25)
    z<-(x-n*0.5)/pSD
    pvalue<-pnorm(z)
    if ( z > 0 ) {
      pvalue<-1-pvalue
    }
    if ( flag==0 ) {
      pvalue<-2*pvalue
    }
    return(pvalue)
  }
}

## =========================== My signed rank Test ======================================

  
#flag=0 '=' case
#flag=1 '<' case
#flag=2 '>' case
my_signed_rank_test<-function(data , mean , alpha,flag ) {
  
  # test case
  # data<-c(97.5,95.2,97.3,96.0,96.8,100.3,97.4,95.3,93.2,99.1,96.1,97.6,98.2,98.5,94.9)
  # my_signed_rank(data,98.5,0.05,0)
  
  sortDif<-sort(abs(data-mean))
  n<-length(data)
  negcount<-0  #Negative Rank
  poscount<-0  #Positive Rank
  discard<-0   #number of elements which are discarded
  count1<-0    #for discarded element it must be subtracted from the total value for Negative Rank
  count2<-0    #for discarded element it must be subtracted from the total value for positive Rank
  for(i in 1:n) {
    if(data[i]==mean){
      discard<-discard+1	#calculating discarded element
    }
  }
  for(i in 1:n) {
    for( j in (1+discard):n) {
      if(sortDif[j]!=0) {
        if(abs(data[i]-mean)==sortDif[j] && (data[i]-mean)<0 ) {
          negcount<-negcount+j #calculating negative rank
          count1<-count1+1
          break
        }
        else if(abs(data[i]-mean)==sortDif[j] && (data[i]-mean)>0 ) {
          poscount<-poscount+j #calculating positive rank
          count2<-count2+1
          break
        }
      }
    }
  }
  negcount<-negcount-count1*discard
  poscount<-poscount-count2*discard
  calculated<-0
  if(flag==0) {
    qvalue<-qsignrank(alpha,n-discard-1) #two Tail
    calculated<-min(negcount,poscount)
  }
  else if(flag==1) {
    qvalue<-qsignrank(2*alpha,n-discard-1) #left tail
    calculated<-poscount
  }
  else if(flag==2) {
    qvalue<-qsignrank(2*alpha,n-discard-1) #right tail
    calculated<-negcount
  }
  return(c(calculated,qvalue))
}

## =========================== Mann whitney Test ======================================

my_mann_whitney_test<-function(data1,data2,alpha,flag) {
  # Test case    
   # data1<-c(14.9,11.3,13.2,16.6,17.0,14.1,15.4,13.0,16.8)
   # data2<-c(15.2,19.8,14.7,18.3,16.2,21.2,18.9,12.2,15.3,19.4)
   # mann_whitney(data1,data2,0.05,1)
   # 
  temp<-c(data1,data2)
  temp<-sort(temp) #sort temp
  n1<-length(data1) #length of 1st data
  n2<-length(data2) #length of 2nd data
  n<-n1+n2          #total data
  w1<-0
  w2<-0
  for(i in 1:n) {
    for(j in 1:n) {
      if(temp[i] == data1[j] && j<=n1) {
        w1<-w1+i #calculating w1
      }
      if(temp[i]==data2[j] && j<=n2) {
        w2<-w2+i #calculating w2
      }
    }
  }
  calculated<-0
  u1<-w1-(n1*(n1+1)/2) #calculating u1
  u2<-w2-(n2*(n2+1)/2) #calculating u2
  if(flag==0) {
    qvalue<-qwilcox(alpha,n1-1,n2-1) #two Tail
    calculated<-min(u1,u2)
  }
  else if(flag==1) {
    qvalue<-qwilcox(2*alpha,n1-1,n2-1) #left tail
    calculated<-u1
  }
  else if(flag==2) {
    qvalue<-qwilcox(2*alpha,n1-1,n2-1) #right tail
    calculated<-u2
  }
  return(c(calculated,qvalue))
}



## =========================== error :Krushkal Test ======================================

# === =error -=====
# data1<-c(94,88,91,74,87,97)
# data2<-c(85,82,79,84,61,72,80)
# data3<-c(89,67,72,76,69)
# alpha<-0.05
#kruskal_wallis(data1,data2,data3,0.05)

kruskal_wallis<-function(data1,data2,data3,alpha) {
  temp<-c(data1,data2,data3)
  temp<-sort(temp) #sort temp
  n1<-length(data1) #length of 1st data
  n2<-length(data2) #length of 2nd data
  n3<-length(data3) #length of 2nd data
  n<-n1+n2+n2          #total data
  R1<-0
  R2<-0
  R3<-0
  for(i in 1:n) {
    for(j in 1:n) {
      if(j<=n1 && temp[i]==data1[j] ) {
        R1<-R1+i #calculating R1
      }
      if(j<=n2 && temp[i]==data2[j]) {
        R2<-R2+i #calculating R2
      }
      if(j<=n3 && temp[i]==data3[j] ) {
        R3<-R3+i #calculating R3
      }
    }
  }

  H<-(12/(n*(n+1)))*( (R1**2)/n1  +  (R2**2)/n2   + (R3**2)/n3 )     - 3*(n+1) #calculating H value
  qvalue<-qchisq(1-alpha,2)						#calculating qvalue
  if(qvalue <= H) {
    return("Reject Null Hypothesis")
  }
  return("Don't Reject Null Hypothesis")
}

