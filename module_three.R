# Including the core functionality
source("module_three_core.R")


## ======================= UTILITY LIST ==========================

module_three_list<-c( "Permutations",
                    "Combinations",
                    "Basic Probability",
                    "Conditional Probability",
                    "Bayes Theorem"
)


## ========================= I/O FUNCTIONS ==============================


## =========================== permutations ======================================

my_permutation_input<-function(){
  tagList(
    textInput('my_permutation_input_dataOne', 'Enter n (objects) ', "4"),
    textInput('my_permutation_input_dataTwo', 'Enter r (sample) ', "2")
  )
}

my_permutation_output<-function(){
  tagList(
    renderPrint({
      
      # get data from UI and prepare
      n <- as.numeric(unlist(strsplit(input$my_permutation_input_dataOne,",")))
      r <- as.numeric(unlist(strsplit(input$my_permutation_input_dataTwo,",")))
      
      # Nicely Display the source data
        cat("Total ( n ) number of objects  : ",n)
      cat("\nTaken ( r ) at a time          : ",r)
      
      # calling the core function
      result<-my_permutation(n,r)
      # Print the result
      cat(sprintf("\n\nPermutations : \n%s",result))
    })
  )
}

## =========================== combinations ======================================

my_combination_input<-function(){
  tagList(
    textInput('my_combination_input_dataOne', 'Enter n (objects) ', "4"),
    textInput('my_combination_input_dataTwo', 'Enter r (sample) ', "2")
  )
}

my_combination_output<-function(){
  tagList(
    renderPrint({
      
      # get data from UI and prepare
      n <- as.numeric(unlist(strsplit(input$my_combination_input_dataOne,",")))
      r <- as.numeric(unlist(strsplit(input$my_combination_input_dataTwo,",")))
      
      # Nicely Display the source data
      cat("Total ( n ) number of objects  : ",n)
      cat("\nTaken ( r ) at a time          : ",r)
      
      # calling the core function
      result<-my_combination(n,r)
      # Print the result
      cat(sprintf("\n\nCombination : \n%s",result))
    })
  )
}


## =========================== Basic probability ======================================

my_basic_prob_input<-function(){
  tagList(
    textInput('my_basic_prob_input_dataOne', 'Enter total number of outcomes ', "6"),
    textInput('my_basic_prob_input_dataTwo', 'Enter number of favourable outcomes ', "3")
  )
}

my_basic_prob_output<-function(){
  tagList(
    renderPrint({
      
      # get data from UI and prepare
      total <- as.numeric(unlist(strsplit(input$my_basic_prob_input_dataOne,",")))
      fav <- as.numeric(unlist(strsplit(input$my_basic_prob_input_dataTwo,",")))
      
      # Nicely Display the source data
      cat("Total number of outcomes      : ",total)
      cat("\nNumber of favourable outcomes : ",fav)
      
      # calling the core function
      result<-my_basic_prob(fav,total)
      # Print the result
      cat(sprintf("\n\nProbability : \n%.6f",result))
    })
  )
}


## =========================== Conditional probability ======================================

my_cond_prob_input<-function(){
  tagList(
    textInput('my_cond_prob_input_dataOne', 'Sample Space (S) ', "1,2,3,4,5,6"),
    textInput('my_cond_prob_input_dataTwo', 'fav outcomes for Given Event (B) ', "1,3,5"),
    textInput('my_cond_prob_input_dataThree', 'fav outcomes for Desired Event (A) ', "1")
  )
}

my_cond_prob_output<-function(){
  tagList(
    renderPrint({
      
      # get data from UI and prepare
      S <- as.numeric(unlist(strsplit(input$my_cond_prob_input_dataOne,",")))
      B <- as.numeric(unlist(strsplit(input$my_cond_prob_input_dataTwo,",")))
      A <- as.numeric(unlist(strsplit(input$my_cond_prob_input_dataThree,",")))
      
      # Nicely Display the source data
      cat("Sample Space : \n",S)
      cat("\n\nFavourable outcomes for given event B:\n",B)
      cat("\n\nFavourable outcomes for desired event A:\n",A)
      
      # calling the core function
      result<-my_cond_prob(A,B,S)
      # Print the result
      cat(sprintf("\n\nProbability of A given B: \n%.6f",result))
    })
  )
}



## =========================== Baye's theorm ======================================

my_bayes_theorm_input<-function(){
  
  tagList(
    textInput('my_bayes_theorm_input_dataZero', 'Sample space can be divided into N events. Enter N :', "3"),
    textInput('my_bayes_theorm_input_dataOne', "For each event i, Enter P(Ai) : ( sum of probablitities of Ai must be 1 )", ".30, .30, .40"),
    textInput('my_bayes_theorm_input_dataTwo', 'For each event i, Enter P( Hypothesis | Ai )', ".20, .15, .10"),
    textInput('my_bayes_theorm_input_dataThree', 'Objective : P( Ak | Hypothesis ), Enter k :', "1")
  )
}

my_bayes_theorm_output<-function(){
  tagList(
    renderPrint({
      
      # get data from UI and prepare
      n <- as.numeric(unlist(strsplit(input$my_bayes_theorm_input_dataZero,",")))
      An <- as.numeric(unlist(strsplit(input$my_bayes_theorm_input_dataOne,",")))
      Bn_given_An <- as.numeric(unlist(strsplit(input$my_bayes_theorm_input_dataTwo,",")))
      k <- as.numeric(unlist(strsplit(input$my_bayes_theorm_input_dataThree,",")))
      
      # Nicely Display the source data
      cat("Number of events in sample space : \n",n)
      cat("\n\nProbability of each event i : \n",An)
      
      if(length(An)==n && length(Bn_given_An)==n){
        
        cat("\n\nP( Hypothesis | Ai ) :\n",Bn_given_An)
        
        cat("\n\nFixed event :\n",k)
        if(k==1){ cat("st") }
        else if(k==2){ cat("nd") }
        else if(k==3){ cat("rd") }
        else { cat("th") }
        
        # calling the core function
        result<-my_bayes_theorm(An,Bn_given_An,k)
        # Print the result
        cat(sprintf("\n\nP( Hypothesis | Ak ): \n%.6f",result))
      }
      
    })
  )
}

