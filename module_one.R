# Including the core functionality
source("module_one_core.R")


## ======================= UTILITY LIST ==========================

module_one_list<-c( "Mean", 
                    "Median", 
                    "Mode", 
                    "Variance | SD", 
                    "Mean Absolute Deviation", 
                    "Range | Max | Min", 
                    "Quartiles | IQR",
                    "Moments : Raw | Central",
                    "Skewness | Kurtosis"
)


## ========================= I/O FUNCTIONS ==============================


# Mean, Median, Mode can also be merged as one utility
# For now, separate is fine.

## =========================== MEAN ======================================

my_mean_input<-function(){
  tagList(
    textInput('my_mean_input_dataOne', 'Enter a vector (comma delimited)', "0,1,2")
  )
}

my_mean_output<-function(){
  tagList(
    renderPrint({
      
      # Getting data from UI
      raw_data<- (input$my_mean_input_dataOne)
      # Preparing data
      my_data <- as.numeric(unlist(strsplit(raw_data,",")))
      
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=10){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      
      # calling the core function
      result<-my_mean(my_data)
      
      # Print the result
      cat(sprintf("\n\nMean : \n %.6f",result))
      })
  )
}

## =========================== MEDIAN ======================================

my_median_input<-function(){
  tagList(
    textInput('my_median_input_dataOne', 'Enter a vector (comma delimited)', "0,1,2")
  )
}


my_median_output<-function(){
  tagList(
    renderPrint({
      
      # Getting data from UI
      raw_data<- (input$my_median_input_dataOne)
      # Preparing data
      my_data <- as.numeric(unlist(strsplit(raw_data,",")))
      
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=10){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      
      # calling the core function
      result<-my_median(my_data)
      
      # Print the result
      cat(sprintf("\n\nMedian : \n %.6f",result))
    })
  )
}

## =========================== MODE ======================================

my_mode_input<-function(){
  tagList(
    textInput('my_mode_input_dataOne', 'Enter a vector (comma delimited)', "0,1,1,2")
  )
}


my_mode_output<-function(){
  tagList(
    renderPrint({
      
      # Getting data from UI
      raw_data<- (input$my_mode_input_dataOne)
      # Preparing data
      my_data <- as.numeric(unlist(strsplit(raw_data,",")))
      
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=10){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      
      # calling the core function
      result<-my_mode(my_data)
      
      # Print the result
      cat(sprintf("\n\nMode : \n%s",result))
      
      # TO merge similar functionalities, just use this type of merging
      # # calling the core function
      # result<-my_median(my_data)
      # 
      # # Print the result
      # cat(sprintf("\n\nMedian : \n %.6f",result))
    })
  )
}

## =========================== MIN, MAX, RANGE ======================================

my_range_input<-function(){
  tagList(
    textInput('my_range_input_dataOne', 'Enter a vector (comma delimited)', "1,2,4")
  )
}


my_range_output<-function(){
  tagList(
    renderPrint({
      
      # Getting data from UI
      raw_data<- (input$my_range_input_dataOne)
      # Preparing data
      my_data <- as.numeric(unlist(strsplit(raw_data,",")))
      
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=10){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      
      # calling the core function
      result<-my_range(my_data)
      
      # Print the result
      cat(sprintf("\n\nRange : \n%s",result[2]-result[1]))
      
      # Print the result
      cat(sprintf("\n\nMinimum : \n%s",result[1]))
      
      # Print the result
      cat(sprintf("\n\nMaximum : \n%s",result[2]))
      
    })
  )
}

## =========================== Variance | SD ======================================

my_var_input<-function(){
  tagList(
    textInput('my_var_input_dataOne', 'Enter a vector (comma delimited)', "1,2,4")
  )
}


my_var_output<-function(){
  tagList(
    renderPrint({
      
      # Getting data from UI
      raw_data<- (input$my_var_input_dataOne)
      # Preparing data
      my_data <- as.numeric(unlist(strsplit(raw_data,",")))
      
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=10){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      
      # Print the result
      cat(sprintf("\n\nWhen the data set is just a sample of population : "))
      cat(sprintf("\nSample Variance : \n%.6f",my_sample_variance(my_data)))
      cat(sprintf("\n\nSample Standard Deviation : \n%.6f",my_sample_SD(my_data)))
              
      # Print the result
      cat(sprintf("\n\n\nWhen the data set is a complete population : "))
      cat(sprintf("\n\nPopulation Variance : \n%.6f",my_population_variance(my_data)))
      cat(sprintf("\n\nPopulation Standard Deviation : \n%.6f",my_population_SD(my_data)))
              
    })
  )
}



## ======================== Mean Absolute Deviation ==================================

my_mad_input<-function(){
  tagList(
    textInput('my_mad_input_dataOne', 'Enter a vector (comma delimited)', "1,2,4,6")
  )
}


my_mad_output<-function(){
  tagList(
    renderPrint({
      
      # Getting data from UI
      raw_data<- (input$my_mad_input_dataOne)
      # Preparing data
      my_data <- as.numeric(unlist(strsplit(raw_data,",")))
      
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=10){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      
      # calling the core function
      result<-my_mad(my_data)
      
      # Print the result
      cat(sprintf("\n\nMean Absolute Deviation : \n %.6f",result))
    })
  )
}

## =========================== Quartiles and IQR ======================================

my_quantile_input<-function(){
  tagList(
    textInput('my_quantile_input_dataOne', 'Enter a vector (comma delimited)', "1,2,4,6,2,3,6,6,7,8,4,2")
  )
}


my_quantile_output<-function(){
  tagList(
    renderPrint({
      
      # Getting data from UI
      raw_data<- (input$my_quantile_input_dataOne)
      # Preparing data
      my_data <- as.numeric(unlist(strsplit(raw_data,",")))
      
      my_data<-sort(my_data)
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=25){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      
      # calling the core function
      result<-my_quantile(my_data)
      
      # Print the result
      cat(sprintf("\n\n1st Quartile : %s",result[1]))
      cat(sprintf("\n2nd Quartile : %s",result[2]))
      cat(sprintf("\n3rd Quartile : %s",result[3]))
      cat(sprintf("\n\nIQR : %s",my_IQR(my_data)))
    })
  )
}


## ======================== Moments : Raw and central ==================================

my_moments_input<-function(){
  tagList(
    textInput('my_moments_input_dataOne', 'Enter a vector (comma delimited)', "1,2,3,4,5,6"),
    textInput('my_moments_input_dataTwo', 'To show first r moments. Enter r', "1"),
    textInput('my_moments_input_dataThree', 'Arbitrary point (A) for Raw Moments', "0")
  )
}


my_moments_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      my_data <- as.numeric(unlist(strsplit(input$my_moments_input_dataOne,",")))
      r <- as.numeric(unlist(strsplit(input$my_moments_input_dataTwo,",")))
      A <- as.numeric(unlist(strsplit(input$my_moments_input_dataThree,",")))
      
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=10){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      for( i in 1:r ){
        
        cat(sprintf("\n"))
        
        cat(sprintf("\n\n%s",i))
        if(i==1){ cat(sprintf("st")) }
        else if(i==2){ cat(sprintf("nd")) }
        else if(i==3){ cat(sprintf("rd")) }
        else { cat(sprintf("th")) }
        cat(sprintf(" Central Moment (around mean): \n %.6f",my_central_moments(my_data,i)))
        
        
        cat(sprintf("\n\n%s",i))
        if(i==1){ cat(sprintf("st")) }
        else if(i==2){ cat(sprintf("nd")) }
        else if(i==3){ cat(sprintf("rd")) }
        else { cat(sprintf("th")) }
        cat(sprintf(" Raw Moment (around A): \n %.6f",my_raw_moments(my_data,i,A)))
      }
    })
  )
}


## ======================== Skewness and kurtosis ==================================

my_skewness_input<-function(){
  tagList(
    textInput('my_skewness_input_dataOne', 'Enter a vector (comma delimited)', "1,2,4,6,2,3,6,6,7,8,4,2")
  )
}


my_skewness_output<-function(){
  tagList(
    renderPrint({
      
      # Getting data from UI
      raw_data<- (input$my_skewness_input_dataOne)
      # Preparing data
      my_data <- as.numeric(unlist(strsplit(raw_data,",")))
      
      my_data<-sort(my_data)
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=25){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      # To plot graph of skewness and kurtosis
      # my_data<-c(1,2,9,3,2,1,0,1)
      # qqnorm(my_data)
      # qqline(my_data)
      # 
      # Print the result
      cat(sprintf("\n\nSkewness : \n%.6f",my_skewness(my_data)))
      cat(sprintf("\n\nKurtosis : \n%.6f",my_kurtosis(my_data)))
    })
  )
}


