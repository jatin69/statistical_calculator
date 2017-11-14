# Including the core functionality
source("module_two_core.R")


## ======================= UTILITY LIST ==========================

module_two_list<-c( "Correlation",
                    "Multiple Linear Regression"
)


## ========================= I/O FUNCTIONS ==============================


## ======================== Correlation ==================================

my_cor_input<-function(){
  tagList(
    textInput('my_cor_input_dataOne', 'Enter a vector (comma delimited)', "8,9,7,6,13,7,11,12"),
    textInput('my_cor_input_dataTwo', 'Enter a vector (comma delimited)', "35,49,27,33,60,21,45,51"),
    textInput('my_cor_input_dataThree', 'Enter level of significance (alpha) for significance test ', "0.05")
  )
}


my_cor_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      x <- as.numeric(unlist(strsplit(input$my_cor_input_dataOne,",")))
      y <- as.numeric(unlist(strsplit(input$my_cor_input_dataTwo,",")))
      myalpha<-as.numeric(unlist(strsplit(input$my_cor_input_dataThree,",")))
      
      #my_data<-sort(my_data)
      # Nicely Display the source data
      cat("For dataset :\n")
      
      cat("\nX : ")
      if(length(x)<=12){
        cat(x)
      }
      else{
        cat(head(x,4),"...",tail(x,4))
      }
      
      
      cat("\ny : ")
      if(length(y)<=12){
        cat(y)
      }
      else{
        cat(head(y,4),"...",tail(y,4))
      }
      
      cor_result<-my_cor(x,y)
      if(cor_result=="Error : X and Y must be of same length."){
        cat(sprintf("\n\nCorrelation :\n%s",cor_result))
      }
      else{
        cat(sprintf("\n\nCorrelation : %.6f",cor_result)) 
        cat(sprintf("\n\nSignificance Test - "))
        cat(sprintf("\n\nNULL Hypothesis : \nCorrelation between two datasets is \nstatistically significant.\n"))
        result<-my_cor_significance_test(cor_result,length(x),myalpha)
        if( result[1] <= result[2] ) {
          cat(sprintf("\nCalculated value is less than equal to \ntabulated value"))
          cat(sprintf("\n%s <= %s",result[1],result[2]))
          cat(sprintf("\n\nReject NULL Hypothesis"))
          cat(sprintf("\n\nCorrelation Between two these datasets is \nstatistically NOT significant."))
        }
        else{
          cat(sprintf("\nCalculated value is greater than tabulated value"))
          cat(sprintf("\n%s > %s",result[1],result[2]))
          cat(sprintf("\n\nDo not Reject NULL Hypothesis"))
          cat(sprintf("\n\nCorrelation Between two these datasets is \nstatistically significant."))
        }
      }
      
            
      
    })
  )
}


## ========================== Multi Linear Regression ======================================


my_multi_linear_regression_input<-function(){
  tagList(
    textInput('my_multi_linear_regression_input_dataOne', 'Enter X1', "3,2,4,2,3,2,5,4"),
    textInput('my_multi_linear_regression_input_dataTwo', 'Enter X2', "2,1,3,1,2,2,3,2"),
    textInput('my_multi_linear_regression_input_dataThree', 'Enter Y', "78800,7430,83800,74200,79700,74900,88400,82900")
  )
}



my_multi_linear_regression_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      x1 <- as.numeric(unlist(strsplit(input$my_multi_linear_regression_input_dataOne,",")))
      x2 <- as.numeric(unlist(strsplit(input$my_multi_linear_regression_input_dataTwo,",")))
      y <- as.numeric(unlist(strsplit(input$my_multi_linear_regression_input_dataThree,",")))
      
      #my_data<-sort(my_data)
      # Nicely Display the source data
      cat("For equations :\n")
      
      cat("\nX1 : ")
      if(length(x1)<=12){
        cat(x1)
      }
      else{
        cat(head(x1,4),"...",tail(x1,4))
      }
      
      
      cat("\nX2 : ")
      if(length(x2)<=12){
        cat(x2)
      }
      else{
        cat(head(x2,4),"...",tail(x2,4))
      }
      
      cat("\nY : ")
      if(length(y)<=12){
        cat(y)
      }
      else{
        cat(head(y,4),"...",tail(y,4))
      }
      
      cat(sprintf("\n\nMultiple Linear Regression : \n"))
      result<-my_multi_linear_regression(x1,x2,y)
      if(result=="Error : X1 and X2 must be of same length."){
        cat(sprintf(result))
      }
      else{
        # Print the result
        cat(sprintf("Y =        %.6f \n  + (x1) * %.6f \n  + (x2) * %.6f",result[1],result[2],result[3]))  
      }
      
    })
  )
  
}

