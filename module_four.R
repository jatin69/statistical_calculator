# Including the core functionality
source("module_four_core.R")


## ======================= UTILITY LIST ==========================

module_four_list<-c( 	"Uniform",
                      "Bernoulli", 
                      "Binomial", 
                      "Geometric", 
                      "Hyper-geometric",
                      "Negative Binomial", 
                      "Poisson", 
                      "Multinomial", 
                      "Multivariate Hypergeometric"
)


## ========================= I/O FUNCTIONS ==============================



## =========================== Uniform ======================================

my_discrete_uniform_input<-function(){
  tagList(
    textInput('my_discrete_uniform_input_dataOne', 'Total Number of discrete groups', "7"),
    textInput('my_discrete_uniform_input_dataTwo', 'Find upto which group', "3")
  )
}

my_discrete_uniform_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      n <- as.numeric(unlist(strsplit(input$my_discrete_uniform_input_dataOne,",")))
      k <- as.numeric(unlist(strsplit(input$my_discrete_uniform_input_dataTwo,",")))
      
      cat(sprintf("Discrete Uniform Distribution  : \n"))
      cat(sprintf("\nTotal Number of groups ( n ) : %s",n))
      cat(sprintf("\nFind upto which  group ( k ) : %s",k))
      
      result<-my_discrete_uniform(n,k)
      
      if(result=="Error : K must be less than equal to total"){
        cat(sprintf("\n\nResult :\n%s",result))
      }
      else{
        cat(sprintf("\n\nResult :\n%.6f",result))  
      }
      
      
      
    })
  )
}

## =========================== Bernoulli ======================================

my_bernoulli_input<-function(){
  tagList(
    textInput('my_bernoulli_input_dataOne', 'Probability (p)', "0.2"),
    selectInput("my_bernoulli_input_dataTwo", "x :", c("0" = 0 , "1" = 1))
  )
}

my_bernoulli_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      p <- as.numeric(unlist(strsplit(input$my_bernoulli_input_dataOne,",")))
      x <- as.numeric(unlist(strsplit(input$my_bernoulli_input_dataTwo,",")))
      
      cat(sprintf("Benoulli Distribution  : \n"))
      cat(sprintf("\np  : %s",p))
      cat(sprintf("\nx  : %s",x))
      
      result<-my_bernoulli(p,x)
      
      if(result=="Error : Probability should be between 0 and 1"){
        cat(sprintf("\n\nResult :\n%s",result))
      }
      else{
        cat(sprintf("\n\nResult :\n%.6f",result))  
      }
      
      
    })
  )
}



## =========================== Binomial ======================================


my_binomial_input<-function(){
  tagList(
    textInput('my_binomial_input_dataOne', 'Enter n :', "10"),
    textInput('my_binomial_input_dataTwo', 'Enter r:', "5"),
    textInput('my_binomial_input_dataThree', 'Enter Probability (p)', "0.2")
  )
}

my_binomial_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      n <- as.numeric(unlist(strsplit(input$my_binomial_input_dataOne,",")))
      r <- as.numeric(unlist(strsplit(input$my_binomial_input_dataTwo,",")))
      p <- as.numeric(unlist(strsplit(input$my_binomial_input_dataThree,",")))
      
      cat(sprintf("Binomial Distribution  : \n"))
      cat(sprintf("\nn  : %s",n))
      cat(sprintf("\nr  : %s",r))
      cat(sprintf("\np  : %s",p))
      
      
      result<-my_binomial(r,n,p)
      
      if(result=="Error : Probability should be between 0 and 1"){
        cat(sprintf("\n\nResult :\n%s",result))
      }
      else if(result=="Error : r should be less than equal to n"){
        cat(sprintf("\n\nResult :\n%s",result))
      }
      else{
        cat(sprintf("\n\nResult :\n%.6f",result))  
      }
      
      
    })
  )
}



## ===================== Negative Binomial  ==============

my_negative_binomial_input<-function(){
  tagList(
    textInput('my_binomial_input_dataOne', 'Enter n :', "10"),
    textInput('my_binomial_input_dataTwo', 'Enter r:', "5"),
    textInput('my_binomial_input_dataThree', 'Enter Probability (p)', "0.2")
  )
}

my_negative_binomial_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      n <- as.numeric(unlist(strsplit(input$my_binomial_input_dataOne,",")))
      r <- as.numeric(unlist(strsplit(input$my_binomial_input_dataTwo,",")))
      p <- as.numeric(unlist(strsplit(input$my_binomial_input_dataThree,",")))
      
      cat(sprintf("Negative Binomial Distribution  : \n"))
      cat(sprintf("\nn  : %s",n))
      cat(sprintf("\nr  : %s",r))
      cat(sprintf("\np  : %s",p))
      
      
      result<-my_negative_binomial(r,n,p)
      
      if(result=="Error : Probability should be between 0 and 1"){
        cat(sprintf("\n\nResult :\n%s",result))
      }
      else if(result=="Error : r should be less than equal to n"){
        cat(sprintf("\n\nResult :\n%s",result))
      }
      else{
        cat(sprintf("\n\nResult :\n%.6f",result))  
      }
      
      
    })
  )
}

## =========================== geometric ======================================


my_geometric_input<-function(){
  tagList(
    textInput('my_geometric_input_dataOne', 'Probability (p)', "0.2"),
    textInput('my_geometric_input_dataTwo', 'Trial on which the user succeeded (x)', "3")
  )
}

my_geometric_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      p <- as.numeric(unlist(strsplit(input$my_geometric_input_dataOne,",")))
      x <- as.numeric(unlist(strsplit(input$my_geometric_input_dataTwo,",")))
      
      cat(sprintf("Geometric Distribution  : \n"))
      cat(sprintf("\nProbability             ( p ) : %s",p))
      cat(sprintf("\nTrial number of success ( x ) : %s",x))
      
      result<-my_geometric(x,p)
      
      if(result=="Error : Probability should be between 0 and 1"){
        cat(sprintf("\n\nResult :\n%s",result))
      }
      else{
        cat(sprintf("\n\nResult :\n%.6f",result))  
      }
      
      
    })
  )
}


## =========================== hyper geometric ======================================

my_hyper_geometric_input<-function(){
  tagList(
    textInput('my_hyper_geometric_input_dataOne', 'Enter population size (N)', "20"),
    textInput('my_hyper_geometric_input_dataTwo', 'Enter number of draws (n)', "5"),
    textInput('my_hyper_geometric_input_dataThree', 'Number of fav outcomes in population (M)', "6"),
    textInput('my_hyper_geometric_input_dataFour', 'Required fav outcomes in draws (x)', "4")
    
  )
}

my_hyper_geometric_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      N <- as.numeric(unlist(strsplit(input$my_hyper_geometric_input_dataOne,",")))
      n <- as.numeric(unlist(strsplit(input$my_hyper_geometric_input_dataTwo,",")))
      M <- as.numeric(unlist(strsplit(input$my_hyper_geometric_input_dataThree,",")))
      x <- as.numeric(unlist(strsplit(input$my_hyper_geometric_input_dataFour,",")))
      
      cat(sprintf("Hyper Geometric Distribution  : \n"))
      cat(sprintf("\nPopulation size                      ( N ) : %s",N))
      cat(sprintf("\nNumber of draws                      ( n ) : %s",n))
      cat(sprintf("\nNumber of fav outcomes in population ( M ) : %s",M))
      cat(sprintf("\nRequired fav outcomes in draws       ( x ) : %s",x))
      
      result<-my_hyper_geometric(N,n,M,x)
      
      if(result=="Error : Probability should be between 0 and 1"){
        cat(sprintf("\n\nResult :\n%s",result))
      }
      else{
        cat(sprintf("\n\nResult :\n%.6f",result))  
      }
      
      
    })
  )
}


## =========================== Poisson ======================================


my_poisson_input<-function(){
  tagList(
    textInput('my_poisson_input_dataOne', 'Enter the mean value of the number 
              of successes that are occurring in the region specified (lambda)', "3"),
    textInput('my_poisson_input_dataTwo', 'Enter the actual number 
              of the successes that are occurring in the region specified. (x)', "5")
  )
}

my_poisson_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      x <- as.numeric(unlist(strsplit(input$my_poisson_input_dataOne,",")))
      mylambda <- as.numeric(unlist(strsplit(input$my_poisson_input_dataTwo,",")))
      
      cat(sprintf("Poisson Distribution  : \n"))
      cat(sprintf("\nMean value of the number of successes that \nare occurring in the region specified (lambda) : \n%s",mylambda))
      cat(sprintf("\nThe actual number of the successes that \nare occurring in the region specified. (x) : \n%s",x))
      
      result<-my_poisson(x,mylambda)
      
      cat(sprintf("\n\nResult :\n%.6f",result))  
      
      
    })
  )
}


## =========================== Multinomial ======================================


my_multinomial_input<-function(){
  tagList(
    textInput('my_multinomial_input_dataZero', 'Total number of events occurred ( n )', "6"),
    textInput('my_multinomial_input_dataOne', 'Number of Subgroups to divide the events into ( s )', "3"),
    textInput('my_multinomial_input_dataTwo', 'Probability of winning for each subgroup si (p)', "0.2,0.3,0.5"),
    textInput('my_multinomial_input_dataThree', 'Number of events won by each subgroup (d) ', "1,2,3")
  )
}

my_multinomial_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      n <- as.numeric(unlist(strsplit(input$my_multinomial_input_dataZero,",")))
      s <- as.numeric(unlist(strsplit(input$my_multinomial_input_dataOne,",")))
      prob <- as.numeric(unlist(strsplit(input$my_multinomial_input_dataTwo,",")))
      data <- as.numeric(unlist(strsplit(input$my_multinomial_input_dataThree,",")))
      
      cat(sprintf("Multinomial Distribution  : \n"))
      cat(sprintf("\nTotal number of events occurred ( n ) : %s",n))
      cat(sprintf("\nNumber of Subgroups of events   ( s ) : %s",s))
      
      cat(sprintf("\n\nProbability of winning for each subgroup si (p) :\n"))
      if(length(prob)<=12){
        cat(prob)
      }
      else{
        cat(head(prob,5),"...",tail(prob,5))
      }
      
      cat(sprintf("\n\nNumber of events won by each subgroup (d)  : \n"))
      if(length(data)<=12){
        cat(data)
      }
      else{
        cat(head(data,5),"...",tail(data,5))
      }
      
      
      result<-my_multinomial(data,n,prob)
      
      if(result=="Error : Sum of outcomes of events must be equal to total available outcomes." ||
         result=="Length of both data must be equal" ||
         result=="Error : Sum of probabilities of events must be 1"){
        cat(sprintf("\n\nResult :\n%s",result))
      }
      else{
        cat(sprintf("\n\nResult :\n%.6f",result))  
      }
    })
  )
}



## =========================== Multivariate Hypergeometric ============================



my_multivariate_hyper_geometric_input<-function(){
  tagList(
    textInput('my_multivariate_hyper_geometric_input_dataOne', 'Total favourable outcomes in the dataset (M)', "10"),
    textInput('my_multivariate_hyper_geometric_input_dataTwo', 'Fav outcomes available in the sub groups of population (D)', "1,2,3,4"),
    textInput('my_multivariate_hyper_geometric_input_dataThree', 'Lot size for draw from population (n)', "4"),
    textInput('my_multivariate_hyper_geometric_input_dataFour', 'Number the fav outcomes wanted from each subgroup Di (x)', "0,2,2,0")
    
  )
}

my_multivariate_hyper_geometric_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      M <- as.numeric(unlist(strsplit(input$my_multivariate_hyper_geometric_input_dataOne,",")))
      D <- as.numeric(unlist(strsplit(input$my_multivariate_hyper_geometric_input_dataTwo,",")))
      n <- as.numeric(unlist(strsplit(input$my_multivariate_hyper_geometric_input_dataThree,",")))
      x <- as.numeric(unlist(strsplit(input$my_multivariate_hyper_geometric_input_dataFour,",")))
      
      cat(sprintf("Multivariate Hyper Geometric Distribution\n"))
      
      cat(sprintf("\nTotal favourable outcomes in the population (M) :\n%s",M))
      cat(sprintf("\n\nFav outcomes available in the \nsub groups of population (D) : \n"))
      if(length(D)<=12){
        cat(D)
      }
      else{
        cat(head(D,5),"...",tail(D,5))
      }
      
      
      cat(sprintf("\n\nLot size for draw from population (n) :\n%s",n))
      cat(sprintf("\n\nNumber the fav outcomes wanted \nfrom each subgroup Di (x)  : \n"))
      if(length(x)<=12){
        cat(x)
      }
      else{
        cat(head(x,5),"...",tail(x,5))
      }
      
      
      result<-my_multivariate_hyper_geometric(M,D,n,x)
      
      if(result=="Error : Length of both data must be equal"  ||
         result=="Error : Favourable outcomes from subgroups must be equal to total favourable outcomes in dataset"||
         result=="Error : Wanted favourable outcomes must be equal to the draw size"){
        cat(sprintf("\n\nResult :\n%s",result))
      }
      else{
        cat(sprintf("\n\nResult :\n%.6f",result))  
      }
    })
  )
}
