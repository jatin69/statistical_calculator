# Including the core functionality
source("module_seven_core.R")


## ======================= UTILITY LIST ==========================

module_seven_list<-c( "Estimation of Means | Variance known",
                      "Estimation of Means | Variance Unknown",
                      "Estimation of Differences in Means | Variance Known",
                      "Estimation of Differences in Means | Variance Unknown",
                      "Estimation of Proportions",
                      "Estimation of Differences in Proportions",
                      "Estimation of Variances",
                      "Estimation of Ratio of Two Variances"
)


## ========================= I/O FUNCTIONS ==============================


## ====================== Estimation of Means ============================


my_est_mean_var_known_input<-function(){
  tagList(
    textInput('my_est_mean_var_known_input_dataOne', 'Enter Sample size (n)', "20"),
    textInput('my_est_mean_var_known_input_dataTwo', 'Enter average (x bar)', "5"),
    textInput('my_est_mean_var_known_input_dataThree', 'Enter Standard Deviation (sigma)', "2"),
    textInput('my_est_mean_var_known_input_dataFour', 'Enter level of significance (alpha) ', "0.05")
  )
}

my_est_mean_var_known_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      n <- as.numeric(unlist(strsplit(input$my_est_mean_var_known_input_dataOne,",")))
      avg<-as.numeric(unlist(strsplit(input$my_est_mean_var_known_input_dataTwo,",")))
      mySD <- as.numeric(unlist(strsplit(input$my_est_mean_var_known_input_dataThree,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_est_mean_var_known_input_dataFour,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Estimation of Mean ( Variance known )\n\n"))
      cat(sprintf("\nSample size           (   n   ) : %s",n))
      cat(sprintf("\nAverage               ( x bar ) : %s",avg))
      cat(sprintf("\nStandard Deviation    ( sigma ) : %s",mySD))
      cat(sprintf("\nlevel of significance ( alpha ) : %s",myalpha))
      
      cat(sprintf("\n\nEstimation : \n"))
      
      interval<-my_est_mean_var_known(avg,mySD,n,myalpha) 
      cat(sprintf("\nMu lies in the interval :\n%s\nto\n%s",interval[1],interval[2]))
      cat(sprintf("\nat (1-alpha)*100 confidence interval"))
      # meu lies between interval[1] and interval[2]
      # at (1-alpha) * 100 confidence interval
      
    })
  )
}


my_est_mean_var_unknown_input<-function(){
  tagList(
    textInput('my_est_mean_var_unknown_input_dataOne', 'Enter Data', "1,2,3,4,5,6,7,8,9,10,11,12"),
    textInput('my_est_mean_var_unknown_input_dataTwo', 'Enter level of significance (alpha) ', "0.05")
  )
}

my_est_mean_var_unknown_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data <- as.numeric(unlist(strsplit(input$my_est_mean_var_unknown_input_dataOne,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_est_mean_var_unknown_input_dataTwo,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Estimation of Mean ( Variance UNknown )\n\n"))
      cat(sprintf("\nSample size           (   n   ) : %s",length(data)))
      cat(sprintf("\nAverage               ( x bar ) : %s",my_mean(data)))
      cat(sprintf("\nStandard Deviation    ( sigma ) : %s",my_sample_SD(data)))
      cat(sprintf("\nlevel of significance ( alpha ) : %s",myalpha))
      
      cat(sprintf("\n\nEstimation : \n"))
      
      interval<-my_est_mean_var_unknown(data,myalpha) 
      cat(sprintf("\nMu lies in the interval :\n%s\nto\n%s",interval[1],interval[2]))
      cat(sprintf("\nat (1-alpha)*100 confidence interval"))
      # meu lies between interval[1] and interval[2]
      # at (1-alpha) * 100 confidence interval
      
    })
  )
}


## ====================== Estimation of Difference of Means ============================


my_est_diff_mean_var_known_input<-function(){
  tagList(
    textInput('my_est_diff_mean_var_known_input_dataOne', 'Enter Sample size ( n1 )', "20"),
    textInput('my_est_diff_mean_var_known_input_dataTwo', 'Enter average (x bar1)', "5"),
    textInput('my_est_diff_mean_var_known_input_dataThree', 'Enter Population variance (sigma sq 1)', "2"),
    
    textInput('my_est_diff_mean_var_known_input_dataFour', 'Enter Sample size ( n2 )', "20"),
    textInput('my_est_diff_mean_var_known_input_dataFive', 'Enter average (x bar2)', "5"),
    textInput('my_est_diff_mean_var_known_input_dataSix', 'Enter Population variance (sigma sq 2)', "2"),
    
    textInput('my_est_diff_mean_var_known_input_dataSeven', 'Enter level of significance (alpha) ', "0.05")
  )
}

my_est_diff_mean_var_known_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      n1 <- as.numeric(unlist(strsplit(input$my_est_diff_mean_var_known_input_dataOne,",")))
      n2 <- as.numeric(unlist(strsplit(input$my_est_diff_mean_var_known_input_dataFour,",")))
      
      avg1<-as.numeric(unlist(strsplit(input$my_est_diff_mean_var_known_input_dataTwo,",")))
      avg2<-as.numeric(unlist(strsplit(input$my_est_diff_mean_var_known_input_dataFive,",")))
      
      var1 <- as.numeric(unlist(strsplit(input$my_est_diff_mean_var_known_input_dataThree,",")))
      var2 <- as.numeric(unlist(strsplit(input$my_est_diff_mean_var_known_input_dataSix,",")))
      
      myalpha <- as.numeric(unlist(strsplit(input$my_est_diff_mean_var_known_input_dataSeven,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Estimation of Difference of Means ( Variance known )\n\n"))
      cat(sprintf("Data Set 1"))
      cat(sprintf("\nSample size           (     n1    ) : %s",n1))
      cat(sprintf("\nAverage               (  x bar 1  ) : %s",avg1))
      cat(sprintf("\nPopulation Variance   ( sigmasq 1 ) : %s",var1))
      
      cat(sprintf("\n\nData Set 2"))
      cat(sprintf("\nSample size           (     n2    ) : %s",n2))
      cat(sprintf("\nAverage               (  x bar 2  ) : %s",avg2))
      cat(sprintf("\nPopulation Variance   ( sigmasq 2 ) : %s",var2))
      cat(sprintf("\n\nlevel of significance (    alpha  ) : %s",myalpha))
      cat(sprintf("\n\nEstimation : \n"))
      
      interval<-my_est_diff_mean_var_known(avg1,avg2,var1,var2,n1,n2,myalpha) 
      cat(sprintf("\nMu lies in the interval :\n%s\nto\n%s",interval[1],interval[2]))
      cat(sprintf("\nat (1-alpha)*100 confidence interval"))
      
    })
  )
}


my_est_diff_mean_var_unknown_input<-function(){
  tagList(
    textInput('my_est_diff_mean_var_unknown_input_dataOne', 'Enter Data Set 1', "1,2,3,4,5,6,7,8,9,10,11,12"),
    textInput('my_est_diff_mean_var_unknown_input_dataTwo', 'Enter Data Set 2', "6,3,1,2,45,8,1,12"),
    textInput('my_est_diff_mean_var_unknown_input_dataThree', 'Enter level of significance (alpha) ', "0.05")
  )
}

my_est_diff_mean_var_unknown_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data1 <- as.numeric(unlist(strsplit(input$my_est_diff_mean_var_unknown_input_dataOne,",")))
      data2 <- as.numeric(unlist(strsplit(input$my_est_diff_mean_var_unknown_input_dataTwo,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_est_diff_mean_var_unknown_input_dataThree,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Estimation of Difference of Means ( Variance Unknown )\n\n"))
      cat(sprintf("Data Set 1"))
      cat(sprintf("\nSample size           (   n1    ) : %s",length(data1)))
      cat(sprintf("\nAverage               ( x bar 1 ) : %s",my_mean(data1)))
      cat(sprintf("\nStandard Deviation    ( sigma 1 ) : %s",my_sample_SD(data1)))
      # ????????????? sample sd or population 
      cat(sprintf("\n\nData Set 2"))
      cat(sprintf("\nSample size           (   n2    ) : %s",length(data2)))
      cat(sprintf("\nAverage               ( x bar 2 ) : %s",my_mean(data2)))
      cat(sprintf("\nStandard Deviation    ( sigma 2 ) : %s",my_sample_SD(data2)))
      
      
      cat(sprintf("\n\nlevel of significance ( alpha ) : %s",myalpha))
      
      cat(sprintf("\n\nEstimation : \n"))
      
      interval<-my_est_diff_mean_var_unknown(data1,data2,myalpha) 
      cat(sprintf("\nMu lies in the interval :\n%s\nto\n%s",interval[1],interval[2]))
      cat(sprintf("\nat (1-alpha)*100 confidence interval"))

    })
  )
}


## ====================== Estimation of Proportions ============================


my_est_prop_input<-function(){
  tagList(
    textInput('my_est_prop_input_dataOne', 'Enter number of Total Outcomes (n)', "20"),
    textInput('my_est_prop_input_dataTwo', 'Enter number of Favourable outcomes (fav)', "5"),
    textInput('my_est_prop_input_dataThree', 'Enter level of significance (alpha) ', "0.05")
  )
}

my_est_prop_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      n <- as.numeric(unlist(strsplit(input$my_est_prop_input_dataOne,",")))
      fav<-as.numeric(unlist(strsplit(input$my_est_prop_input_dataTwo,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_est_prop_input_dataThree,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Estimation of Proportions \n\n"))
      cat(sprintf("\nTotal      (  n  ) : %s",n))
      cat(sprintf("\nFavourable ( fav ) : %s",fav))
      cat(sprintf("\nlevel of significance ( alpha ) : %s",myalpha))
      
      cat(sprintf("\n\nEstimation : \n"))
      
      interval<-my_est_prop(fav,n,myalpha)
      cat(sprintf("\nMu lies in the interval :\n%s\nto\n%s",interval[1],interval[2]))
      cat(sprintf("\nat (1-alpha)*100 confidence interval"))
      # meu lies between interval[1] and interval[2]
      # at (1-alpha) * 100 confidence interval
      
    })
  )
}



## ====================== Estimation of Difference of Proportions ============================


my_est_diff_prop_input<-function(){
  tagList(
    textInput('my_est_diff_prop_input_dataOne', 'Enter number of Total Outcomes for Data set 1 ( n1 )', "20"),
    textInput('my_est_diff_prop_input_dataTwo', 'Enter number of Favourable outcomes for Data set 1 (fav)', "5"),
    textInput('my_est_diff_prop_input_dataThree', 'Enter number of Total Outcomes for Data set 2 ( n2 )', "10"),
    textInput('my_est_diff_prop_input_dataFour', 'Enter number of Favourable outcomes for Data set 2 (fav)', "2"),
    
    textInput('my_est_diff_prop_input_dataFive', 'Enter level of significance (alpha) ', "0.05")
  )
}

my_est_diff_prop_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      n1 <- as.numeric(unlist(strsplit(input$my_est_diff_prop_input_dataOne,",")))
      fav1<-as.numeric(unlist(strsplit(input$my_est_diff_prop_input_dataTwo,",")))
      n2 <- as.numeric(unlist(strsplit(input$my_est_diff_prop_input_dataThree,",")))
      fav2<-as.numeric(unlist(strsplit(input$my_est_diff_prop_input_dataFour,",")))
      myalpha <- as.numeric(unlist(strsplit(input$my_est_diff_prop_input_dataFive,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Estimation of Difference of Proportions \n\n"))
      cat(sprintf("Data Set 1"))
      cat(sprintf("\nTotal      (  n1  ) : %s",n1))
      cat(sprintf("\nFavourable ( fav1 ) : %s",fav1))
      cat(sprintf("\n\nData Set 2"))
      cat(sprintf("\nTotal      (  n2  ) : %s",n2))
      cat(sprintf("\nFavourable ( fav2 ) : %s",fav2))
      cat(sprintf("\n\nlevel of significance ( alpha ) : %s",myalpha))
      
      cat(sprintf("\n\nEstimation : \n"))
      
      interval<-my_est_diff_prop(fav1,n1,fav2,n2,myalpha)
      cat(sprintf("\nMu lies in the interval :\n%s\nto\n%s",interval[1],interval[2]))
      cat(sprintf("\nat (1-alpha)*100 confidence interval"))
      # meu lies between interval[1] and interval[2]
      # at (1-alpha) * 100 confidence interval
      
    })
  )
}

## =========================== Estimation of variances ======================================


my_est_var_input<-function(){
  tagList(
    textInput('my_est_var_input_dataOne', 'Enter data set', "20,2,1,1,4,1,40"),
    textInput('my_est_var_input_dataTwo', 'Enter Population variance ', "2"),
    textInput('my_est_var_input_dataThree', 'Enter level of significance (alpha) ', "0.05")
  )
}

my_est_var_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data1 <- as.numeric(unlist(strsplit(input$my_est_var_input_dataOne,",")))
      var1<-as.numeric(unlist(strsplit(input$my_est_var_input_dataTwo,",")))
      
      myalpha <- as.numeric(unlist(strsplit(input$my_est_var_input_dataThree,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Estimation of Variances \n\n"))
      cat(sprintf("\nSample Size           (    n     ) : %s",length(data1)))
      cat(sprintf("\nPopulation Variance   ( sigma sq ) : %s",var1))
      cat(sprintf("\n\nlevel of significance (   alpha  ) : %s",myalpha))
      
      cat(sprintf("\n\nEstimation : \n"))
      
      interval<-my_est_var(data1,var1,myalpha)
      cat(sprintf("\nMu lies in the interval :\n%s\nto\n%s",interval[1],interval[2]))
      cat(sprintf("\nat (1-alpha)*100 confidence interval"))
      # meu lies between interval[1] and interval[2]
      # at (1-alpha) * 100 confidence interval
      
    })
  )
}



## ======================= Estimation of Ratio of Two Variances ==========================


my_est_ratio_var_input<-function(){
  tagList(
    textInput('my_est_ratio_var_input_dataOne', 'Enter data set 1', "2,2,1,1,4,1,4"),
    textInput('my_est_ratio_var_input_dataTwo', 'Enter Population variance for data set 1', "2.2"),
    textInput('my_est_ratio_var_input_dataThree', 'Enter data set 2', "1,2,4,5,6,7"),
    textInput('my_est_ratio_var_input_dataFour', 'Enter Population variance for data set 2 ', "3.1"),
    textInput('my_est_ratio_var_input_dataFive', 'Enter level of significance (alpha) ', "0.05")
  )
}

my_est_ratio_var_output<-function(){
  tagList(
    renderPrint({
      
      # Preparing data
      data1 <- as.numeric(unlist(strsplit(input$my_est_ratio_var_input_dataOne,",")))
      var1<-as.numeric(unlist(strsplit(input$my_est_ratio_var_input_dataTwo,",")))
      data2 <- as.numeric(unlist(strsplit(input$my_est_ratio_var_input_dataThree,",")))
      var2<-as.numeric(unlist(strsplit(input$my_est_ratio_var_input_dataFour,",")))
      
      myalpha <- as.numeric(unlist(strsplit(input$my_est_ratio_var_input_dataFive,",")))
      
      # ---------------- Display data set as well smoothly ---------------------- #
      
      # Nicely Display the source data
      cat(sprintf("Estimation of Ration of Variances \n\n"))
      cat(sprintf("Data Set 1"))
      cat(sprintf("\nSample Size           (    n1     ) : %s",length(data1)))
      cat(sprintf("\nPopulation Variance   ( sigma sq1 ) : %s",var1))
      cat(sprintf("\n\nData Set 2"))
      cat(sprintf("\nSample Size           (    n1     ) : %s",length(data2)))
      cat(sprintf("\nPopulation Variance   ( sigma sq1 ) : %s",var2))
      cat(sprintf("\n\nlevel of significance (   alpha   ) : %s",myalpha))
      
      cat(sprintf("\n\nEstimation : \n"))
      
      interval<-my_est_ratio_var(data1,data2,var1,var2,myalpha)
      cat(sprintf("\nMu lies in the interval :\n%s\nto\n%s",interval[1],interval[2]))
      cat(sprintf("\nat (1-alpha)*100 confidence interval"))
      # meu lies between interval[1] and interval[2]
      # at (1-alpha) * 100 confidence interval
      
    })
  )
}


## ======================= END ==========================
