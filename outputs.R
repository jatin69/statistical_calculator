library(shiny)

source("functions.R")

mean_output<-function(){
  tagList(
    renderPrint({
      #print(input$mean_input_data1)
      
      vector_data<- (input$mean_input_data1)
      x <- as.numeric(unlist(strsplit(vector_data,",")))
      result<-my_mean(x)
      
      # cool dude display
      cat("For dataset :\n")
      if(length(x)<=10){
        cat(x)
      }
      else{
        cat(head(x,4),"...",tail(x,4))
      }
      
      cat(sprintf("\n\nMean is : \n %.6f",result))
      # # cat("As atomic vector:\n")
      # print(x)
      # cat("Mean is :\n",my_mean(x))
      # 
    })
    # ,
    # 
    # renderText({
    #   x <- as.numeric(unlist(strsplit(input$mean_input_data,",")))
    #   cat("As atomic vector:\n")
    #   print(x)
    # })
  
    
    )
}