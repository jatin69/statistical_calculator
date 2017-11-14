# ================ CORE FUNCTIONS FOR MODULE TWO =================


## =========================== Correlation ======================================

my_cor<-function(x,y) {
  
  # x and y must be multile data with same length
  
  # In built 
  # x<-c(8,9,7,6,13,7,11,12)
  # y<-c(35,49,27,33,60,21,45,51)
  # cor(x,y)
  if(length(x)==length(y)){
    n<-length(x)
    r<-( n * sum(x**2) - (sum(x)**2) ) * ( n * sum(y**2) - (sum(y)**2) )  
    r<-sqrt(r)
    r<-1/r
    r<-( n * sum(x*y) - sum(x) *sum(y) ) * r
    
    # 6 digit precision
    result <- formatC(r,digits = 6,format = "f")
    return(result)
  }
  else{
    return("Error : X and Y must be of same length.")
  }
}


## ========================== Multi Linear Regression ======================================

my_multi_linear_regression<-function(x1 , x2 , y ) {
  
  
  if(length(x1)==length(x2)){
    
    xMatrix<-cbind(c(length(x1),sum(x1),sum(x2)) , 
                   c(sum(x1),sum(x1**2),sum(x1*x2)) , 
                   c(sum(x2),sum(x1*x2),sum(x2**2)))
    
    xyMatrix<-cbind(c(sum(y),
                      sum(x1*y),
                      sum(x2*y)))
    
    result<-solve(xMatrix,xyMatrix)    #matrix inverse , then multiply with inverse and save final result in result
    
    return(c(result[1][1] ,result[2][1],  result[3][1]))
  }
  else{
    return("Error : X1 and X2 must be of same length.")
  }
  
  

  }
