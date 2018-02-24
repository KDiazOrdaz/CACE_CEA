# CACE_CEA
Stata and R  scripts presenting code for performing Complier-average causal effects estimation of multivariate outcomes. (Three-stage least squares and Bayesian models)
README	Stata and R  scripts presenting code for performing Complier-average causal effects estimation of multivariate outcomes 


AUTHORS: Karla DiazOrdaz, Angelo Franchini and Richard Grieve (Methods for estimating complier-average causal effects for cost-effectiveness analysis)
contact author: e-mail: karla.diaz-ordaz@lshtm.ac.uk 
INSTALL:  
1. For 3sls.do you need access to Stata
2. For  3sls.R, you need to have R  installed. Within R, you need to install the following packages   
"MASS","AER", "systemfit","mice","foreign"

You can install them within R by typing
install.packages("MASS","AER", "systemfit","mice","foreign")

3. For the Bayesian code BFL.R (the script file)  and BFL.txt (the JAGS model files), you need R and JAGS installed in your computer.
In addition, R2jags needs to be installed in your copy of R.
 

Details:
The code provided in the files 3sls.do and 3sls.R, first  performs multiple imputation (MI) to deal with the missing bivariate outcomes and baseline covariates (in the example used in the paper, these were  total costs, total QALYs and the baseline EQ5D summary score eq5d_b).

MI is done by chained equations, using predictive mean matching with the 5 nearest neighbours. You can modify these options to suit your variables and analyses. We include the endogenous variable (in the example, treatment received) as a variable in the imputation models. As it is customary in randomised clinical trials,  imputations are performed separately by randomised treatment arm.
  
After obtaining 50 imputations, 3sls estimates are obtained, and pooled following Rubin's rules.

The code provided in the file BFL.R performs a Bayesian instrumental variable multivariate normal  analysis, to obtain the CACE of interest. This accounts for the missing outcome and covariates, by adding a model for the baseline covariate with missing data in the model file BFL.txt.
 
Details of all the methods are described in the accompanying paper (published in the Journal of the Royal Statistical Society, Series A). A version of this is also permanently archived in https://arxiv.org/abs/1601.07127


