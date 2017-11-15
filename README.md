# statistical_calculator
Statistical Calculator is Shiny web app made with R studio

## Version 1
Major modules and utilities working


SKULL
================

SKULL - A STATISTICAL CALCULATOR
================================

### Naming conventions

    R inbuilt function  : mean() 
    My version          : my_mean() 
    Its input           : my_mean_input() 
    Its output          : my_mean_output()

### Input Data variables -

    my_mean_input_dataOne
    my_mean_input_dataTwo

code heirarchy
==============

#### Individual files for Modules

    - module_one.R 
    - module_two.R

#### module\_one.R contains

    - the vector list of its utilities - mean, median, mode, etc.
    - Proper documentation of its utilities
    - my_mean_input()
    - my_mean_output()

#### module\_one\_core.R

    - my_mean()
    - Retains the core functionality

MODULES
=======

1. Descriptive Analysis
-----------------------

### Mean | Mode | Median

    my_mean(data) 
    my_mode(data)
    my_median(data) 

*where* *data* : A vector array of raw data

### Variance

    my_sample_variance(data)
    my_population_variance(data)

*where* *data* : A vector array of raw data

### Standard Deviation

    my_sample_SD(data)
    my_population_SD(data)

*where* *data* : A vector array of raw data

### Mean Absolute Deviation

    my_mad(data)

*where* *data* : A vector array of raw data

### Range | Max | Min

    my_range(data)
    my_min(data)
    my_max(data)

*where* *data* : A vector array of raw data

### Quartiles | IQR

    my_quantile(data)
    my_IQR(data)

*where* *data* : A vector array of raw data

### Moments

    my_central_moments(data)
    my_raw_moments(data)

*where* *data* : A vector array of raw data

### Skewness | Kurtosis

      my_skewness(data)
      my_kurtosis(data)

*where* *data* : A vector array of raw data

2. Predictive Analysis
----------------------

### Correlation with significance test

    my_cor(datasetA,datasetB)

*where,*
*datasetA* : A vector array of raw data
*datasetB* : A vector array of raw data

    my_cor_significance_test(r,n,alpha)

*where,*
*r* : The correlation value to be tested
*n* : Size of dataset on which the correlation is found
*alpha* : level of significance

### Multiple Linear Regression

    my_multi_linear_regression(x1,x2,y)

*where,*
*x1* , *x2* : Variable to form matrix X
*y* : Variable to form matrix Y

3. Probability Analysis
=======================

### factorial

    my_factorial(number)

*where*
*number* : The number whose factorial needs to be found.

### Permutations | Combinations

    my_permutation(n,r)
    my_permutation(n,r)

*where*
*n* : Total number of objects.
*r* : Objects taken at a time.

### Basic Probability

    my_basic_prob(favorable,total)

*where*
*favourable*: Favourable outcomes
*total* : Total outcomes

### Conditional Probability

    my_cond_prob(A,B,S)

*where,*
*A* : fav outcomes for Desired Event (A)
*B* : fav outcomes for Given Event (B)
*S* : Sample Space (S)

### Bayes Theorem

    my_bayes_theorm(An,Bn_given_An,k)

*where,*
*Sample space can be divided into N events*
*An* : A list of P(Ai) for each event `i` in `n`
*( sum of probablitities of Ai must be 1 )*
*Bn\_given\_An* : A list of P(B|Ai) for each event `i` in `n`
*k* : To find `P(Ak|Hypothesis)` we fix event `k`

4. Discrete Distribution Functions
==================================

### Uniform

    my_discrete_uniform(n,k)

*where*
*n* : Total Number of discrete groups
*k* : Find upto which group

### Bernoulli

    my_bernoulli(p,x)

*where*
*p* : probability of success in event
*x* : can have value either 0 or 1

### Binomial

    my_binomial(r,n,p)

*where*
*n* : Total number of events
*r* : No of events to be succeeded
*p* : probability of success in one event

### Geometric

    my_geometric(x,p)

*where*
*x* : Trial on which the user succeeded (x)
*p* : probability of success in one trial

### Hyper-geometric

    my_hyper_geometric(N,n,M,x)

*where*
*N* : population size (N)
*n* : number of draws (n)
*M* : Number of fav outcomes in population (M)
*x* : Required fav outcomes in draws (x)

### Negative Binomial

    my_negative_binomial(r,n,p)

*where*
*n* : Total number of events
*r* : No of events to be succeeded
*p* : probability of success in one event

### Poisson

    my_poisson(x,lambda)

*where*,
*lambda* : The mean value of the number of successes that are occurring in the region specified
*x* : The actual number of the successes that are occurring in the region specified. (x)

### Multinomial

    my_multinomial(d,n,prob)

*where*
*n* : Total number of events occurred
*Events can be divided into `s` subgroups*
*prob* : List of Probability of winning for each subgroup Si
*d* : List of Actual Number of events won by each subgroup (d)

### Multivariate Hypergeometric

    my_multivariate_hyper_geometric(M,D,n,x)

*where*
*M* : Total favourable outcomes in the dataset
*Dataset can be divided into `s` subgroups*
*D* : List of Fav outcomes available in each sub group Si
*n* : Lot size for draw from population
*x* : List of Number the fav outcomes wanted from each subgroup Si

5. Continuous Distribution Functions
====================================

### Uniform

    my_cont_uniform(alpha,beta,low,high)

*The continuous uniform distribution is the probability distribution
of random number selection from the continuous interval between alpha and beta*
*where*,
*alpha* : lower limit of interval
*beta* : upper limit of interval
*low* : lower limit for Integration
*high* : Upper limit for Integration

### Normal

    my_cont_normal(mean,psd,low,high)

*where*,
*mean* : Mean
*psd* : Population Standard Deviation
*low* : lower limit for Integration
*high* : Upper limit for Integration

### Gamma

    my_gamma_test(alpha,beta,low,high)

*A random variable X is gamma-distributed with shape alpha and rate beta*
*where*,
*alpha* : decides shape of distribution
*beta* : decides rate of distribution
*low* : lower limit for Integration
*high* : Upper limit for Integration

### Exponential

    my_exp_dist(lambda,low,high)

*where*,
*special case of gamma distribution*
*lambda*: 1/beta
*low* : lower limit for Integration
*high* : Upper limit for Integration

6. Sample Distribution Test Statistic
=====================================

### Z-test

    my_z_test(avg,mu,pSD,n,alpha,flag)

*where*
*n* : Sample size
*avg* : Sample Average (x bar)
*mu* : Mean for hypothesis testing
*pSD* : Standard Deviation (sigma)
*NULL Hypothesis : X bar is equal to Mu*
*alpha* : level of significance (alpha)
*flag* : To choose between two tail or one tail.  `flag==0` is `'='` case
`flag==1` is `'<'` case
`flag==2` is `'>'` case

### Student t-test

    my_t_test(data,mu,alpha,flag)

*where*,
*data* : Dataset on which the test is to be performed.
*mu* : Mean for hypothesis testing
*NULL Hypothesis : X bar of dataset is equal to Mu*
*alpha* : level of significance (alpha)
*flag* : To choose between two tail or one tail.
`flag==0` is `'='` case
`flag==1` is `'<'` case
`flag==2` is `'>'` case

### F-test

    my_f_test(data1,data2,alpha)

*where*
*data1* : Dataset 1
*data2* : Dataset 2
*NULL Hypothesis : Both Data sets have approximately equal variances*
*alpha* : level of significance (alpha)

### Chi-Square

    my_chi_square_test(data,pVar,alpha,flag)

*where*
*data* : Dataset on which the test is to be performed.
*pVar* : Variance for hypothesis testing
*NULL Hypothesis : Variance of dataset is equal to pvar*
*alpha* : level of significance (alpha)
*flag* : To choose between two tail or one tail.
`flag==0` is `'='` case
`flag==1` is `'<'` case
`flag==2` is `'>'` case

### Shapiro Wilk test - Inbuilt

    my_shapiro_test(data,alpha)

*where*,
*data* : Dataset on which the test is to be performed.
*NULL Hypothesis : Dataset is Normally distributed.*
*alpha* : level of significance (alpha)

7. Interval Estimation
======================

### Estimation of Means | Variance Known

    my_est_mean_var_known(avg,pSD,n,alpha)

*where*,
*n* : Sample size (n)
*avg* : average (x bar)
*pSD* : Standard Deviation (sigma)
*alpha* : level of significance (alpha)

### Estimation of Means | Variance Unknown

    my_est_mean_var_unknown(data,alpha)

*where*,
*data* : Dataset on which the estimate is to be done
*alpha* : level of significance (alpha)

### Estimation of Differences in Means | Variance Known

    my_est_diff_mean_var_known(avg1,avg2,pVar1,pVar2,n1,n2,alpha)

*where*,
*n1* : Sample size of dataset 1 (n1)
*avg1* : average of dataset 1 (x bar1)
*pVar1* : population variance (sigma sq1)
*n1* : Sample size of dataset 2 (n2)
*avg1* : average of dataset 2 (x bar2)
*pVar1* : population variance (sigma sq2)
*alpha* : level of significance (alpha)

### Estimation of Differences in Means | Variance Unknown

    my_est_diff_mean_var_unknown(data1,data2,alpha)

*where*,
*data1* : Dataset 1
*data2* : Dataset 2
*Estimate will be performed on these two data sets*
*alpha* : level of significance (alpha)

### Estimation of Proportions

    my_est_prop(fav,n,alpha)

*where*,
*fav* : number of Favourable outcomes
*n* : number of Total Outcomes
*alpha* : level of significance (alpha)

### Estimation of Differences in Proportions

    my_est_diff_prop(fav1,n1,fav2,n2,alpha)

*where*,
*fav1* : number of Favourable outcomes of dataset 1
*n1* : number of Total Outcomes of dataset 1
*fav2* : number of Favourable outcomes of dataset 2
*n2* : number of Total Outcomes of dataset 2
*alpha* : level of significance (alpha)

### Estimation of Variances

        my_est_var(data,pVar,alpha)

*where*,
*data* : Dataset on which the estimate is to be done
*pVar* : Population variance
*alpha* : level of significance (alpha)

### Estimation of Ratio of Two Variances

        my_est_ratio_var(data1,data2,pVar1,pVar2,alpha)

*where*,
*data1* : Dataset 1 on which the estimate is to be done
*pVar1* : Population variance for dataset 1
*data2* : Dataset 2 on which the estimate is to be done
*pVar2* : Population variance for dataset 2
*alpha* : level of significance (alpha)

8. Non-Parametric Analysis
==========================

### Sign Test | Wilcoxon Signed-Rank test

    my_sign_test(data,mean,alpha,flag)
    my_signed_rank_test(data,mean,alpha,flag)

*where*
*data* : Raw Data on which the test is to be performed.
*mean* : Mean for hypothesis testing (Mu)
*alpha* : level of significance (alpha)
*flag* : To choose between two tail or one tail.
`flag==0` is `'='` case
`flag==1` is `'<'` case
`flag==2` is `'>'` case

### Mann-Whitney Test

    my_mann_whitney_test(data1,data2,alpha,flag)

*where*,
*data1* : Data set 1
*data2* : Data set 2
*Null hypothesis : Both Data Sets have approximately equal means.*
*alpha* : level of significance (alpha)
*flag* : To choose between two tail or one tail.
`flag==0` is `'='` case
`flag==1` is `'<'` case
`flag==2` is `'>'` case

### Kruskal-Wallis Test

    my_kruskal_wallis(data1,data2,data3,alpha)

*where*,
*data1* : Data set 1
*data2* : Data set 2
*data3* : Data set 3
*Null hypothesis : All Data Sets have approximately equal means.*
*alpha* : level of significance (alpha)

9. Visualizations
=================

### Histograms | Bar Graph | Pie Chart | Stem-leaf plot | Pareto Chart

    my_hist(data)
    my_barplot(data)
    my_pie(data)
    my_stem(data)
    my_pareto_char(data)

*where*
*data* : is the raw data which needs to be plotted

### Scatter plot | Box-plot | q-q plot

    my_scatter(datax, datay)
    my_boxplot(datax, datay)
    my_qplot(datax, datay)

*where* *datax* : is the raw data which is plotted across x axis
*datay* : is the raw data which is plotted across y axis

### Line Graph

    my_linegraph(data1, data2, data3)

*where* *data1* : for Plotting line 1  *data2* : for Plotting line 2
*data3* : for Plotting line 3
