# Including the core functionality
source("module_eight_core.R")


## ======================= UTILITY LIST ==========================

module_eight_list<-c( 	"Sign Test",
                      "Wilcoxon Signed-Rank test",
                      "Mann-Whitney Test"
                      #,"Kruskal-Wallis Test"
)


## ========================= I/O FUNCTIONS ==============================


## =========================== sign Test ======================================

my_sign_test_input<-function(){
  tagList(
    textInput('my_sign_test_input_dataOne', 'Enter Data', "1,2,3,4,5,6,7,8,9,10,11,12"),
    textInput('my_sign_test_input_dataTwo', 'Enter Mean for hypothesis testing (Mu)', "2"),
    textInput('my_sign_test_input_dataThree', 'Enter level of significance (alpha) ', "0.05"),
    selectInput("my_sign_test_input_dataFour", "Choose Tail :", c("Two Tailed" = 0 , "One Tailed" = 1))
    
  )
}


my_sign_test_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data <- as.numeric(unlist(strsplit(input$my_sign_test_input_dataOne,",")))
      meu <- as.numeric(unlist(strsplit(input$my_sign_test_input_dataTwo,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_sign_test_input_dataThree,",")))
      flag <- as.numeric(unlist(strsplit(input$my_sign_test_input_dataFour,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Non Parametric - Sign Test \n"))
      cat(sprintf("\n\nTesting Hypothesis    (   Mu  ) : %s",meu))
      cat(sprintf("\nlevel of significance ( alpha ) : %s",myalpha))
      cat(sprintf("\n\nNULL Hypothesis : \nX bar is equal to Mu"))
      cat(sprintf("\n\nTest Results : \n"))
      
      pvalue<- my_sign_test(data,meu,myalpha,flag)
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


## =========================== signed Rank Test ======================================

my_signed_rank_test_input<-function(){
  tagList(
    textInput('my_signed_rank_test_input_dataOne', 'Enter Data', "1,2,3,4,5,6,7,8,9,10,11,12"),
    textInput('my_signed_rank_test_input_dataTwo', 'Enter Mean for hypothesis testing (Mu)', "2"),
    textInput('my_signed_rank_test_input_dataThree', 'Enter level of significance (alpha) ', "0.05"),
    selectInput("my_signed_rank_test_input_dataFour", "Choose Tail :", c("Two Tailed" = 0 , "One Tailed" = 1))
    
  )
}

my_signed_rank_test_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data <- as.numeric(unlist(strsplit(input$my_signed_rank_test_input_dataOne,",")))
      meu <- as.numeric(unlist(strsplit(input$my_signed_rank_test_input_dataTwo,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_signed_rank_test_input_dataThree,",")))
      flag <- as.numeric(unlist(strsplit(input$my_signed_rank_test_input_dataFour,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Non Parametric - Signed Rank Test \n"))
      cat(sprintf("\n\nTesting Hypothesis    (   Mu  ) : %s",meu))
      cat(sprintf("\nlevel of significance ( alpha ) : %s",myalpha))
      cat(sprintf("\n\nNULL Hypothesis : \nX bar is equal to Mu"))
      cat(sprintf("\n\nTest Results : \n"))
      
      result<- my_signed_rank_test(data,meu,myalpha,flag)
      if( result[1] <= result[2] ) {
        cat(sprintf("\nCalculated value is less than equal to tabulated value"))
        cat(sprintf("\n%s <= %s",result[1],result[2]))
        cat(sprintf("\n\nReject NULL Hypothesis"))   
      }
      else{
        cat(sprintf("\nCalculated value is greater than tabulated value"))
        cat(sprintf("\n%s > %s",result[1],result[2]))
        cat(sprintf("\n\nDo not Reject NULL Hypothesis"))        
      }
    })
  )
}

## =========================== Mann whitney Test ======================================


my_mann_whitney_test_input<-function(){
  tagList(
    textInput('my_mann_whitney_test_input_dataOne', 'Enter Data set 1', "1,2,3,4,5,6,7,8,9,10,11,12"),
    textInput('my_mann_whitney_test_input_dataTwo', 'Enter Data set 2', "5,6,7,8,9,10,11,12,1,43,1,4"),
    textInput('my_mann_whitney_test_input_dataThree', 'Enter level of significance (alpha) ', "0.05"),
    selectInput("my_mann_whitney_test_input_dataFour", "Choose Tail :", c("Two Tailed" = 0 , "One Tailed" = 1))
    
  )
}

my_mann_whitney_test_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data1 <- as.numeric(unlist(strsplit(input$my_mann_whitney_test_input_dataOne,",")))
      data2 <- as.numeric(unlist(strsplit(input$my_mann_whitney_test_input_dataTwo,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_mann_whitney_test_input_dataThree,",")))
      flag <- as.numeric(unlist(strsplit(input$my_mann_whitney_test_input_dataFour,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Non Parametric - Mann Whitney Test \n"))
      cat(sprintf("\nlevel of significance ( alpha ) : %s",myalpha))
      cat(sprintf("\n\nNULL Hypothesis : \nBoth Data Sets have approximately equal means."))
      cat(sprintf("\n\nTest Results : \n"))
      
      result<- my_mann_whitney_test(data1,data2,myalpha,flag)
      if( result[1] <= result[2] ) {
        cat(sprintf("\nCalculated value is less than equal to tabulated value"))
        cat(sprintf("\n%s <= %s",result[1],result[2]))
        cat(sprintf("\n\nReject NULL Hypothesis"))   
      }
      else{
        cat(sprintf("\nCalculated value is greater than tabulated value"))
        cat(sprintf("\n%s > %s",result[1],result[2]))
        cat(sprintf("\n\nDo not Reject NULL Hypothesis"))        
      }
    })
  )
}




