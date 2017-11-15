# Including the core functionality
source("module_five_core.R")


## ======================= UTILITY LIST ==========================

module_five_list<-c( 	"Continous Uniform",
                      "Continous Normal",
                      #"Bivariate Normal",
                      "Gamma Distribution",
                      "Exponential distribution"
)



## ========================= I/O FUNCTIONS ==============================

## =========================== uniform continous distribution ======================================

my_cont_uniform_input<-function(){
  tagList(
    textInput('my_cont_uniform_input_dataOne', 'alpha', "0.1"),
    textInput('my_cont_uniform_input_dataTwo', 'Beta', "0.9"),
    textInput('my_cont_uniform_input_dataThree', 'lower limit for Integration', "0"),
    textInput('my_cont_uniform_input_dataFour', 'upper limit for Integration', "1")
    
  )
}

my_cont_uniform_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      alpha <- as.numeric(unlist(strsplit(input$my_cont_uniform_input_dataOne,",")))
      beta <- as.numeric(unlist(strsplit(input$my_cont_uniform_input_dataTwo,",")))
      lower <- as.numeric(unlist(strsplit(input$my_cont_uniform_input_dataThree,",")))
      upper <- as.numeric(unlist(strsplit(input$my_cont_uniform_input_dataFour,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Continuous Uniform Distribution :\n\n"))
      cat(sprintf("\nalpha       : %s",alpha))
      cat(sprintf("\nbeta        : %s",beta))
      cat(sprintf("\nlower limit : %s",lower))
      cat(sprintf("\nupper limit : %s",upper))
      
      result<-my_cont_uniform(alpha,beta,lower,upper)
      cat(sprintf("\n\nResult : \n%s",result))
      
    })
  )
}

## =========================== Normal continous distribution ======================================

my_cont_normal_input<-function(){
  tagList(
    textInput('my_cont_normal_input_dataOne', 'Mean', "2"),
    textInput('my_cont_normal_input_dataTwo', 'Population SD', "1"),
    textInput('my_cont_normal_input_dataThree', 'lower limit for Integration', "0"),
    textInput('my_cont_normal_input_dataFour', 'upper limit for Integration', "1")
    
  )
}

my_cont_normal_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      mean <- as.numeric(unlist(strsplit(input$my_cont_normal_input_dataOne,",")))
      pSD <- as.numeric(unlist(strsplit(input$my_cont_normal_input_dataTwo,",")))
      lower <- as.numeric(unlist(strsplit(input$my_cont_normal_input_dataThree,",")))
      upper <- as.numeric(unlist(strsplit(input$my_cont_normal_input_dataFour,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Continuous Normal Distribution :\n\n"))
      cat(sprintf("\nMean          : %s",mean))
      cat(sprintf("\nPopulation SD : %s",pSD))
      cat(sprintf("\nlower limit   : %s",lower))
      cat(sprintf("\nupper limit   : %s",upper))
      
      result<-my_cont_normal(mean,pSD,lower,upper)
      cat(sprintf("\n\nResult : \n%s",result))
      
    })
  )
}


## ========================== gamma and exponential function ======================================


my_gamma_test_input<-function(){
  tagList(
    textInput('my_gamma_test_input_dataOne', 'alpha', "0.9"),
    textInput('my_gamma_test_input_dataTwo', 'Beta', "9"),
    textInput('my_gamma_test_input_dataThree', 'lower limit for Integration', "0"),
    textInput('my_gamma_test_input_dataFour', 'upper limit for Integration', "1")
    
  )
}

my_gamma_test_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      alpha <- as.numeric(unlist(strsplit(input$my_gamma_test_input_dataOne,",")))
      beta <- as.numeric(unlist(strsplit(input$my_gamma_test_input_dataTwo,",")))
      lower <- as.numeric(unlist(strsplit(input$my_gamma_test_input_dataThree,",")))
      upper <- as.numeric(unlist(strsplit(input$my_gamma_test_input_dataFour,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Gamma Distribution :\n\n"))
      cat(sprintf("\nalpha       : %s",alpha))
      cat(sprintf("\nbeta        : %s",beta))
      cat(sprintf("\nlower limit : %s",lower))
      cat(sprintf("\nupper limit : %s",upper))
      
      result<-my_gamma_test(alpha,beta,lower,upper)
      cat(sprintf("\n\nResult : \n%s",result))
      
    })
  )
}


my_exp_dist_input<-function(){
  tagList(
    textInput('my_exp_dist_input_dataOne', 'lambda', "0.4"),
    textInput('my_exp_dist_input_dataTwo', 'lower limit for Integration', "1"),
    textInput('my_exp_dist_input_dataThree', 'upper limit for Integration', "2")
    
  )
}

my_exp_dist_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      mylambda <- as.numeric(unlist(strsplit(input$my_exp_dist_input_dataOne,",")))
      lower <- as.numeric(unlist(strsplit(input$my_exp_dist_input_dataTwo,",")))
      upper <- as.numeric(unlist(strsplit(input$my_exp_dist_input_dataThree,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Exponential Distribution :\n\n"))
      cat(sprintf("\nlambda       : %s",mylambda))
      cat(sprintf("\nlower limit  : %s",lower))
      cat(sprintf("\nupper limit  : %s",upper))
      
      result<-my_exp_dist(mylambda,lower,upper)
      cat(sprintf("\n\nResult : \n%s",result))
      
    })
  )
}





