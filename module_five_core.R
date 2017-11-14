# ================ CORE FUNCTIONS FOR MODULE FIVE =================


## =========================== uniform continous distribution ======================================

# alpha beta is the limits where uniform distribution lies
# low and high are the integration limits (low<high)
my_cont_uniform<-function(alpha, beta ,low , high ) {
  result<-(high-low)/(beta-alpha)
  result<-as.numeric(formatC(result,digits=6,format="f"))
  return(result)
}

## ========================== normal distribution ======================================

# low and high are the integration limits (low<high)
my_cont_normal<-function( mean , psd ,low , high) {
  fun<-function(x) {
    norm<-(exp((-0.5)*((x-mean)/psd)**2))/(psd*((2*pi)**0.5)) #normal function
    return(norm)
  }
  normintegrate<-integrate(fun , low , high)$value
  normintegrate<-as.numeric(formatC(normintegrate,digits=6,format="f"))
  return(normintegrate)
}


## ========================== gamma and exponential function ======================================

#Gamma distribution
#alpha and beta are the parameters of gamma distribution
# low and high are the integration limits (low<high)
my_gamma_test<-function( alpha , beta ,low , high ) {
  gfun<-function(x){
    temp<-(x**(alpha-1))*exp(-x)
    temp<-as.numeric(formatC(temp,digits=6,format="f"))
    return(temp)
  }
  galpha<-integrate(gfun,0,Inf)$value	#calculation gamma of alpha
  
  fun<-function(x) {
    gvalue<-beta**alpha
    gvalue<-(1/gvalue)*(galpha)
    gvalue<-gvalue*(x**(alpha-1))*(exp(-x/beta)) #gamma function
    gvalue<-as.numeric(formatC(gvalue,digits=6,format="f"))
    return(gvalue)
  }
  
  if(low < 0 ) {
    low=0
  }
  gammaIntegrate<-integrate(fun , low , high)$value
  gammaIntegrate<-as.numeric(formatC(gammaIntegrate,digits=6,format="f"))
  return(gammaIntegrate)
}
        

# low and high are the integration limits (low<high)
# special case of gamma distribution alpha=1 and beta=1/lambda
my_exp_dist<-function( lambda ,low , high ) {
  return(gamma(1,1/lambda,low,high))
}



## ========================== error : Bi variate normal ======================================

#= ============== ERROR ===================
#mean1 , sd1 of mean and sd of x
#mean2 , sd2 of mean and sd of y
# r is correlation between x and y
# lowx and highx is integration limits for x
# lowy and highy is integration limits for y
bivariate_normal<-function(mean1,sd1,mean2,sd2,r,lowx,highx,lowy,highy) {
  fun<-function(x,y) {
    num<-(((x-mean1)/sd1)**2)+(((y-mean2)/sd2)**2)-2*r*((x-mean1)/sd1)*((y-mean2)/sd2)
    bivnorm<-(exp((-0.5)*(1/(1-(r**2)))*num))/(2*pi*sd1*sd2*(sqrt(1-(r**2))))
    return(bivnorm)
  }
  binormintegrate<-integrate( function(y) {
    sapply(y, function(y) {
      return(integrate(fun, lowx, highx)$value)
    }
    )
  }, lowy, highy)$value
  #int2(fun , c(lowx,lowy),c(highx,highy) #alternative joint integration
  binormintegrate<-as.numeric(formatC(binormintegrate,digits=6,format="f"))
  return(binormintegrate)
}
