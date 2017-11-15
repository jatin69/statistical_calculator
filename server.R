#' @author : Jatin Rohilla
#' @tool   : R studio v1.0.153
#' 
#' @ Statistical Calculator
#' @title : server.R


library(shiny)

source("module_includes.R")

# Server Logic

server <- function(input, output) {
  
  # Making input outputs global
  # sacrificing security for Modularity
  input<<-input
  output<<-output
  
  
  ### LEFT MOST PANEL ###
  
  ## renders module list ###
  output$module<-renderUI({
      
    # ask for module input
    selectInput(
        inputId = "module_menu", label = "Choose a module", 
        choices = module_list, selected = "-- Choose a module --"
      )
    
    })
  
  ## Renders sub module list ###
  output$sub_module<-renderUI({
    
    # The warning pops up for EXPR should be 1 because, module_one_list is a list of len >1
    
    sub_module_choices= switch(
    
        input$module_menu, # changes based on module choice
      
        "-- Choose a module --"= module_zero_list,
        "Descriptive Analysis"= module_one_list,
        "Predictive Analysis"= module_two_list,
        "Probability Analysis"= module_three_list,
        "Discrete Distribution Functions"=module_four_list,
        "Continuous Distribution Functions"=module_five_list,
        "Sample Distribution Test Statistic"=module_six_list,
        "Interval Estimation"=module_seven_list,
        "Non-Parametric Analysis"=module_eight_list,
        "Visualizations"=module_nine_list
        
    )
    
    # ask for sub_module input
    selectInput(
      inputId = "sub_module_menu", label = "Choose a Sub module", 
      choices = sub_module_choices
    )
  })
  
  
  ### MIDDLE PANEL ### - Most difficult
  ## Renders required input - arbitrary length - unstable ###
  ## also figure out - how to pass it as argument to my function and how to receive output ##
  
  output$sub_module_inputs<-renderUI({
    
    sub_module_inputs= switch(
      
      input$sub_module_menu,
      "-- First select a module --"= my_info_input,
      # module one
      "Mean"= my_mean_input,
      "Median"= my_median_input,
      "Mode"= my_mode_input,
      "Range | Max | Min"= my_range_input,
      "Variance | SD"=my_var_input,
      "Mean Absolute Deviation"=my_mad_input,
      "Quartiles | IQR"=my_quantile_input,
      "Moments : Raw | Central"= my_moments_input,
      "Skewness | Kurtosis"=my_skewness_input,
      
      # module 2
      "Correlation"=my_cor_input,
      "Multiple Linear Regression"=my_multi_linear_regression_input,
      
      # module 3
      "Permutations"= my_permutation_input,
      "Combinations"= my_combination_input,
      "Basic Probability"= my_basic_prob_input,
      "Conditional Probability"= my_cond_prob_input,
      "Bayes Theorem"= my_bayes_theorm_input,
      
      #module 4
      "Uniform"=my_discrete_uniform_input,
      "Bernoulli"=my_bernoulli_input,
      "Binomial"=my_binomial_input,
      "Negative Binomial"=my_negative_binomial_input,
      "Geometric"=my_geometric_input,
      "Hyper-geometric"=my_hyper_geometric_input,
      "Poisson"=my_poisson_input,
      "Multinomial"=my_multinomial_input,
      "Multivariate Hypergeometric"=my_multivariate_hyper_geometric_input,
      
      
      #module 5
      "Continous Uniform"=my_cont_uniform_input,
      "Continous Normal"=my_cont_normal_input,
      "Gamma Distribution"=my_gamma_test_input,
      "Exponential distribution"=my_exp_dist_input,
      
      # module 6
      "Z-test"=my_z_test_input,
      "Student t-test"=my_t_test_input,
      "F-test"=my_f_test_input,
      "Chi-Square"=my_chi_square_test_input,
      "Shapiro Wilk test"=my_shapiro_test_input,
      
      #module 7
      "Estimation of Means | Variance known"=my_est_mean_var_known_input,
      "Estimation of Means | Variance Unknown"=my_est_mean_var_unknown_input,
      "Estimation of Differences in Means | Variance Known"=my_est_diff_mean_var_known_input,
      "Estimation of Differences in Means | Variance Unknown"=my_est_diff_mean_var_unknown_input,
      "Estimation of Proportions"=my_est_prop_input,
      "Estimation of Differences in Proportions"=my_est_diff_prop_input,
      "Estimation of Variances"=my_est_var_input,
      "Estimation of Ratio of Two Variances"=my_est_ratio_var_input,
      
      
      # module 8
      "Sign Test"=my_sign_test_input,
      "Wilcoxon Signed-Rank test"=my_signed_rank_test_input,
      "Mann-Whitney Test"=my_mann_whitney_test_input,
      "Kruskal-Wallis Test"=my_kruskal_wallis_input,
      
      # module 9
      "Histograms"=my_hist_input,
      "Bar Graph"=my_barplot_input,
      "Line Graph"=my_linegraph_input,
      "Pie Chart"=my_pie_input,
      "Scatter plot"=my_scatter_input,
      "Box-plot"=my_boxplot_input,
      "q-q plot"=my_qplot_input,
      "Stem-leaf plot"=my_stem_input,
      "Pareto Chart"=my_pareto_chart_input
      
    )
    
    # Takes all the required inouts
    sub_module_inputs()
    
  })
  
  #### RIGHT MOST PART ####### - Equally difficult
  
  output$sub_module_result<-renderUI({
    
    sub_module_outputs= switch(
      input$sub_module_menu,
      "-- First select a module --"= my_info_output,
      
      # module 1
      "Mean"= my_mean_output,
      "Median"= my_median_output,
      "Mode"= my_mode_output,
      "Range | Max | Min"= my_range_output,
      "Variance | SD"= my_var_output,
      "Mean Absolute Deviation"=my_mad_output,
      "Quartiles | IQR"=my_quantile_output,
      "Moments : Raw | Central"= my_moments_output,
      "Skewness | Kurtosis"=my_skewness_output,
      
      # module 2
      "Correlation"=my_cor_output,
      "Multiple Linear Regression"=my_multi_linear_regression_output,
      
      
      # module 3
      "Permutations"= my_permutation_output,
      "Combinations"= my_combination_output,
      "Basic Probability"= my_basic_prob_output,
      "Conditional Probability"= my_cond_prob_output,
      "Bayes Theorem"= my_bayes_theorm_output,
      
      #module 4
      "Uniform"=my_discrete_uniform_output,
      "Bernoulli"=my_bernoulli_output,
      "Binomial"=my_binomial_output,
      "Negative Binomial"=my_negative_binomial_output,
      "Geometric"=my_geometric_output,
      "Hyper-geometric"=my_hyper_geometric_output,
      "Poisson"=my_poisson_output,
      "Multinomial"=my_multinomial_output,
      "Multivariate Hypergeometric"=my_multivariate_hyper_geometric_output,
      
      
      #module 5
      "Continous Uniform"=my_cont_uniform_output,
      "Continous Normal"=my_cont_normal_output,
      "Gamma Distribution"=my_gamma_test_output,
      "Exponential distribution"=my_exp_dist_output,
      
      
      # module 6
      "Z-test"=my_z_test_output,
      "Student t-test"=my_t_test_output,
      "F-test"=my_f_test_output,
      "Chi-Square"=my_chi_square_test_output,
      "Shapiro Wilk test"=my_shapiro_test_output,
      
      #module 7
      "Estimation of Means | Variance known"=my_est_mean_var_known_output,
      "Estimation of Means | Variance Unknown"=my_est_mean_var_unknown_output,
      "Estimation of Differences in Means | Variance Known"=my_est_diff_mean_var_known_output,
      "Estimation of Differences in Means | Variance Unknown"=my_est_diff_mean_var_unknown_output,
      "Estimation of Proportions"=my_est_prop_output,
      "Estimation of Differences in Proportions"=my_est_diff_prop_output,
      "Estimation of Variances"=my_est_var_output,
      "Estimation of Ratio of Two Variances"=my_est_ratio_var_output,
      
      
      # module 8
      "Sign Test"=my_sign_test_output,
      "Wilcoxon Signed-Rank test"=my_signed_rank_test_output,
      "Mann-Whitney Test"=my_mann_whitney_test_output,
      "Kruskal-Wallis Test"=my_kruskal_wallis_output,
      
      
      # module 9
      "Histograms"=my_hist_output,
      "Bar Graph"=my_barplot_output,
      "Line Graph"=my_linegraph_output,
      "Pie Chart"=my_pie_output,
      "Scatter plot"=my_scatter_output,
      "Box-plot"=my_boxplot_output,
      "q-q plot"=my_qplot_output,
      "Stem-leaf plot"=my_stem_output,
      "Pareto Chart"=my_pareto_chart_output
      
    )
    
    sub_module_outputs()
  })
  
}
# server logic ends
