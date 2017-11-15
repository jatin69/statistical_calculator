# Including the core functionality
source("module_six_core.R")


## ======================= UTILITY LIST ==========================

module_six_list<-c("Z-test",
                   "Student t-test",
                   "F-test",
                   "Chi-Square",
                   "Shapiro Wilk test"
)


## ================================= I/O FUNCTIONS ==================================


## ==================================== Z Test =======================================

my_z_test_input<-function(){
  tagList(
    textInput('my_z_test_input_dataOne', 'Enter Sample size (n)', "20"),
    textInput('my_z_test_input_dataTwo', 'Enter average (x bar)', "5"),
    textInput('my_z_test_input_dataThree', 'Enter Mean for hypothesis testing (Mu)', "5.5"),
    textInput('my_z_test_input_dataFour', 'Enter Standard Deviation (sigma)', "2"),
    textInput('my_z_test_input_dataFive', 'Enter level of significance (alpha) ', "0.05"),
    selectInput("my_z_test_input_dataSix", "Choose Tail :", c("Two Tailed" = 0 , "One Tailed" = 1))
    
  )
}


my_z_test_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      n <- as.numeric(unlist(strsplit(input$my_z_test_input_dataOne,",")))
      avg<-as.numeric(unlist(strsplit(input$my_z_test_input_dataTwo,",")))
      meu <- as.numeric(unlist(strsplit(input$my_z_test_input_dataThree,",")))
      mySD <- as.numeric(unlist(strsplit(input$my_z_test_input_dataFour,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_z_test_input_dataFive,",")))
      flag <- as.numeric(unlist(strsplit(input$my_z_test_input_dataSix,",")))

      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("For Hypothesis Testing - Z test :\n\n"))
      cat(sprintf("\nSample size           (   n   ) : %s",n))
      cat(sprintf("\nAverage               ( x bar ) : %s",avg))
      cat(sprintf("\nStandard Deviation    ( sigma ) : %s",mySD))
      cat(sprintf("\n\nTesting Hypothesis    (   Mu  ) : %s",meu))
      cat(sprintf("\nlevel of significance ( alpha ) : %s",myalpha))
      cat(sprintf("\n\nNULL Hypothesis : \nX bar is equal to Mu"))

      cat(sprintf("\n\nTest Results : \n"))
      
      pvalue<- my_z_test(avg,meu,mySD,n,myalpha,flag)
      if( pvalue < myalpha ) {
        cat(sprintf("\nP value is less than alpha"))
        cat(sprintf("\n%s < %s",pvalue,myalpha))
        cat(sprintf("\n\nReject NULL Hypothesis"))   
      }
      else{
        cat(sprintf("\nP value is greater than alpha"))
        cat(sprintf("\n%s > %s",pvalue,myalpha))
        cat(sprintf("\n\nDo not Reject NULL Hypothesis"))      
      }
    })
  )
}


## ==================================== T Test =======================================

my_t_test_input<-function(){
  tagList(
    textInput('my_t_test_input_dataOne', 'Enter Data', "1,2,3,4,5,6,7,8,9,10,11,12"),
    textInput('my_t_test_input_dataTwo', 'Enter Mean for hypothesis testing (Mu)', "2"),
    textInput('my_t_test_input_dataThree', 'Enter level of significance (alpha) ', "0.05"),
    selectInput("my_t_test_input_dataFour", "Choose Tail :", c("Two Tailed" = 0 , "One Tailed" = 1))
    
  )
}


my_t_test_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data <- as.numeric(unlist(strsplit(input$my_t_test_input_dataOne,",")))
      meu <- as.numeric(unlist(strsplit(input$my_t_test_input_dataTwo,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_t_test_input_dataThree,",")))
      flag <- as.numeric(unlist(strsplit(input$my_t_test_input_dataFour,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("For Hypothesis Testing - T test :\n\n"))
      cat(sprintf("\nSample size           (   n   ) : %s",length(data)))
      cat(sprintf("\nAverage               ( x bar ) : %s",my_mean(data)))
      cat(sprintf("\nStandard Deviation    ( sigma ) : %s",my_sample_SD(data)))
      cat(sprintf("\n\nTesting Hypothesis    (   Mu  ) : %s",meu))
      cat(sprintf("\nlevel of significance ( alpha ) : %s",myalpha))
      cat(sprintf("\n\nNULL Hypothesis : \nX bar is equal to Mu"))
      
      
      if(length(data)>30){
        cat(sprintf("\n\nSample size > 30\nSo it is a Normal Distribution"))
        cat(sprintf("\nCalling Z-test "))
      }
      
      cat(sprintf("\n\nTest Results : \n"))
      
      pvalue<- my_t_test(data,meu,myalpha,flag)
      if( pvalue < myalpha ) {
        cat(sprintf("\nP value is less than alpha"))
        cat(sprintf("\n%s < %s",pvalue,myalpha))
        cat(sprintf("\n\nReject NULL Hypothesis"))   
      }
      else{
        cat(sprintf("\nP value is greater than alpha"))
        cat(sprintf("\n%s > %s",pvalue,myalpha))
        cat(sprintf("\n\nDo not Reject NULL Hypothesis"))      
      }
    })
  )
}


## =========================== F test ======================================


my_f_test_input<-function(){
  tagList(
    textInput('my_f_test_input_dataOne', 'Enter Data', "1,2,3,4,5,6,7,8,9,10,11,12"),
    textInput('my_f_test_input_dataTwo', 'Enter Data', "9,11,3,7,8"),
    textInput('my_f_test_input_dataThree', 'Enter level of significance (alpha) ', "0.05")
  )
}


my_f_test_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data1 <- as.numeric(unlist(strsplit(input$my_f_test_input_dataOne,",")))
      data2 <- as.numeric(unlist(strsplit(input$my_f_test_input_dataTwo,",")))
     
      myalpha <- as.numeric(unlist(strsplit(input$my_f_test_input_dataThree,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("For Hypothesis Testing - F test :\n\n"))
      cat(sprintf("Data Set 1\n"))
      cat(sprintf("\nSample size  (   n1   ) : %s",length(data1)))
      cat(sprintf("\nAverage      ( x bar1 ) : %s",my_mean(data1)))
      cat(sprintf("\nVariance     ( s1 sq  ) : %s",my_sample_variance(data1)))
      
      cat(sprintf("\n\nData Set 2\n"))
      cat(sprintf("\nSample size  (   n2   ) : %s",length(data2)))
      cat(sprintf("\nAverage      ( x bar2 ) : %s",my_mean(data2)))
      cat(sprintf("\nVariance     ( s2 sq  ) : %s",my_sample_variance(data2)))
      
      cat(sprintf("\n\nlevel of significance (  alpha ) : %s",myalpha))
      
      cat(sprintf("\n\nNULL Hypothesis : \nBoth Data sets have approximately equal variances"))
      
      cat(sprintf("\n\nTest Result : \n"))
      
      decision<- my_f_test(data1,data2,myalpha)
      cat(sprintf("\n%s",decision))
    })
  )
}


## =========================== Chi square Test ======================================

my_chi_square_test_input<-function(){
  tagList(
    textInput('my_chi_square_test_input_dataOne', 'Enter Data', "1,2,2"),
    textInput('my_chi_square_test_input_dataTwo', 'Enter Test Hypothesis (Sigma square)', "1.2"),
    textInput('my_chi_square_test_input_dataThree', 'Enter level of significance (alpha) ', "0.05"),
    selectInput("my_chi_square_test_input_dataFour", "Choose Tail :", c("Two Tailed" = 0 , "One Tailed" = 1))
    
  )
}


my_chi_square_test_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data <- as.numeric(unlist(strsplit(input$my_chi_square_test_input_dataOne,",")))
      sigmasq <- as.numeric(unlist(strsplit(input$my_chi_square_test_input_dataTwo,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_chi_square_test_input_dataThree,",")))
      flag <- as.numeric(unlist(strsplit(input$my_chi_square_test_input_dataFour,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("For Hypothesis Testing - Chi Square test :\n\n"))
      cat(sprintf("\nSample size      (   n   ) : %s",length(data)))
      cat(sprintf("\nAverage          ( x bar ) : %s",my_mean(data)))
      cat(sprintf("\nSample Variance  ( s sq. ) : %s",my_sample_variance(data)))
      
      cat(sprintf("\n\nTesting at l.o.s ( alpha ) : %s",myalpha))
      cat(sprintf("\n\nNULL Hypothesis : \nsigma square = %s",sigmasq))
      cat(sprintf("\n\nTest Results : \n"))
      
      pvalue<- my_chi_square_test(data,sigmasq,alpha,flag)
      if( pvalue < myalpha ) {
        cat(sprintf("\nP value is less than alpha"))
        cat(sprintf("\n%s < %s",pvalue,myalpha))
        cat(sprintf("\n\nReject NULL Hypothesis"))   
      }
      else{
        cat(sprintf("\nP value is greater than alpha"))
        cat(sprintf("\n%s > %s",pvalue,myalpha))
        cat(sprintf("\n\nDo not Reject NULL Hypothesis"))      
      }
    })
  )
}


## =========================== Shapiro Wilk test ======================================

my_shapiro_test_input<-function(){
  tagList(
    textInput('my_shapiro_test_input_dataOne', 'Enter Data', "1,2,3,4,5,6,7,8,9,10"),
    textInput('my_shapiro_test_input_dataTwo', 'Enter level of significance (alpha) ', "0.05")
  )
}

my_shapiro_test_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data <- as.numeric(unlist(strsplit(input$my_shapiro_test_input_dataOne,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_shapiro_test_input_dataTwo,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Shapiro Wilk Normality test :\n\n"))
      cat(sprintf("\nSample size      (   n   ) : %s",length(data)))
      cat(sprintf("\nAverage          ( x bar ) : %s",my_mean(data)))
      cat(sprintf("\n\nTesting at l.o.s ( alpha ) : %s",myalpha))
      cat(sprintf("\n\nNULL Hypothesis : \nThe population is normally distributed."))
      cat(sprintf("\n\nTest Results : \n"))
      
      pvalue<- my_shapiro_test(data,alpha)
      if( pvalue < myalpha ) {
        cat(sprintf("\nP value is less than alpha"))
        cat(sprintf("\n%s < %s",pvalue,myalpha))
        cat(sprintf("\n\nReject NULL Hypothesis"))   
      }
      else{
        cat(sprintf("\nP value is greater than alpha"))
        cat(sprintf("\n%s > %s",pvalue,myalpha))
        cat(sprintf("\n\nDo not Reject NULL Hypothesis"))      
      }
    })
  )
}

# ========================= END ==================================