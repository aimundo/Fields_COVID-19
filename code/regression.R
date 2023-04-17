## This script uses the object created in the raking.R script to fit a fixed-effects logistic regression

#model for uncorrected data
m0<-svyglm(first_dose_m~Age_group_ord+Month+income_ord+Race+Health_Region+Race*income_ord+Race*Health_Region,
           design=a1,
           family = quasibinomial(), 
           control= list(maxit=25))


# model for corrected data

m2<-svyglm(first_dose_m~Age_group_ord+Month+income_ord+Race+Health_Region+Race*income_ord+Race*Health_Region,
           design=a1_rake,
           family = quasibinomial(), 
           control= list(maxit=25))
