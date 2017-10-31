# Including the core functionality
source("module_one_core.R")


## ======================= UTILITY LIST ==========================

module_one_list<-c( "Mean", 
                    "Median", 
                    "Mode", 
                    "Variance", 
                    "Standard Deviation",
                    "Mean Absolute",
                    "Deviation", 
                    "Range", 
                    "Quartiles", 
                    "IQR", 
                    "Minimum", 
                    "Maximum", 
                    "Skewness", 
                    "Kurtosis", 
                    "Moments"
)


## ========================= I/O FUNCTIONS ==============================


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
      # calling the core function
      result<-my_mean(my_data)
    
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=10){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      
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
      # calling the core function
      result<-my_median(my_data)
      
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=10){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      
      # Print the result
      cat(sprintf("\n\nMedian : \n %.6f",result))
    })
  )
}

## =========================== MODE ======================================

my_mode_input<-function(){
  tagList(
    textInput('my_mode_input_dataOne', 'Enter a vector (comma delimited)', "0,1,2")
  )
}


my_mode_output<-function(){
  tagList(
    renderPrint({
      
      # Getting data from UI
      raw_data<- (input$my_mode_input_dataOne)
      # Preparing data
      my_data <- as.numeric(unlist(strsplit(raw_data,",")))
      # calling the core function
      result<-my_mode(my_data)
      
      # Nicely Display the source data
      cat("For dataset :\n")
      if(length(my_data)<=10){
        cat(my_data)
      }
      else{
        cat(head(my_data,4),"...",tail(my_data,4))
      }
      
      # Print the result
      cat(sprintf("\n\nMode : \n %.6f",result))
    })
  )
}


# Similarly do all