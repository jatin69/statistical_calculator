# Z values in R
pnorm(-2.12)

# ===================================================================

# t test
qt(.9995, df=1)

# one tail
abs(qt(0.25, 40)) # 75% confidence, 1 sided (same as qt(0.75, 40))
abs(qt(0.01, 40)) # 99% confidence, 1 sided (same as qt(0.99, 40))

# two tail
# Note that the t-distribution is symmetric. 
# For a 2-sided test (say with 99% confidence) you can use the critical value
abs(qt(0.01/2, 40)) # 99% confidence, 2 sided => similar to 0.005 of single tail

# ===================================================================

# Simple linear regression
# critical.t <- function(){
#   cat("\n","\bEnter Alpha Level","\n")
#   alpha<-scan(n=1,what = double(0),quiet=T)
#   cat("\n","\b1 Tailed or 2 Tailed:\nEnter either 1 or 2","\n")
#   tt <- scan(n=1,what = double(0),quiet=T)
#   cat("\n","\bEnter Number of Observations","\n")
#   n <- scan(n=1,what = double(0),quiet=T)
#   cat("\n\nCritical Value =",qt(1-(alpha/tt), n-2), "\n")
# }
# critical.t()

# ===================================================================

# F test - ( level of significance, df1, df2)
qf(.95, df1=5, df2=1) 

# ===================================================================

# chi square - (level of significance, df1 )
qchisq(.95,df=2)

# ===================================================================
