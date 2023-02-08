## This script uses the object created in the raking.R script to fit a fixed-effects logistic regression



#call the script for raking
source(here::here("code","raking.R"))

# logistic regression model
m1<-svyglm(first_dose_m~Age_group+Race+Health_Region*Income+Health_Region+Age_group*Income+Race*Income,
           design=a1_rake,
           family = quasibinomial(), 
           control= list(maxit=25))

