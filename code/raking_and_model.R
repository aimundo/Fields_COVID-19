#### this script uses the clean dataset to: 1) correct the observations in the survey
#### response using raking and 2) compute a model of logistic regression

#load packages

library(weights)
library(here)
library(survey)
library(tidyverse)
library(lme4)
library(gtsummary)

data<-read.csv(here("data","clean_dataset.csv"))

# remove rows with missing observations for race

clean_data<-data%>%drop_na(race)

#remove "municipal status" as is not part of the analysis (to avoid unexpected errors)

clean_data<-clean_data%>%
  select(-Municipal.status)

#make variables of interest factors
clean_data<-clean_data %<>%
  mutate_at(c("income_ord","race","age_group","location","Geographic.area","Geographic.area.title"),factor)

# recode the response of interest for categorical analysis
clean_data<-clean_data %>%
  mutate(first_dose_m=case_when(
    first_dose=="yes"~1,
    first_dose=="no"~0
  ))


# get percentages to see how different the data is from the Census (details of the Census values are in
# Data_cleaning_and_covariates_Jan_05.qmd in directory "data cleaning")

wpct(clean_data$income_ord)
wpct(clean_data$age_group)
wpct(clean_data$race)
wpct(clean_data$location)



################ RAKING ###################################
## I had originally used the percentages from the census, but the "population values"
## that need to be provided in the examples of the survey package are all based on total counts.
## This would make sense as to estimate the sampling probability we input from the dataset are counts as well.
## I transformed all the percentages using the total population for Ontario (from the 2016 census) and 
## household counts (also from the 2016 Census).

## Each of these dataframes provide the population totals for the different variables and their categories

ages<-data.frame(age_group=c("16_24",
                             "25_34",
                             "35_44",
                             "45_54",
                             "55_64",
                             "65_and_over"),
                 Freq=c(1707959,
                        1734856,
                        1721407,
                        2003826,
                        1842444,
                        2245898))



races<-data.frame(race=c("arab_middle_eastern",
                         "black",
                         "east_asian_pacific_islander",
                         "indigenous",
                         "latin_american",
                         "mixed",
                         "other",
                         "south_asian",
                         "white_caucasian"),
                  Freq=c(212782,
                         638346,
                         886592,
                         376558,
                         197020,
                         130033,
                         705333,
                         1166361,
                         9118079 ))

income_ords<-data.frame(income_ord=c("under_15000",
                                     "15000_24999",
                                     "25000_39999",
                                     "40000_59999",
                                     "60000_89999",
                                     "over_90000"),
                        Freq=c(294643,
                               387688,
                               599624,
                               796052,
                               1007988,
                               2083176
                               
                        ))

locations<-data.frame(location=c("All other cities",
                                 "Toronto"),
                      Freq=c(10812589,
                             2635905
                      ))


### Raking ####

## First, provide a survey design object. This is done using the dataset. The syntax
## means that there is no stratification (id=~1). When the line below is run it will throw
## a warning because we do not have "design" weights in the dataset (other datasets do have this).


a1<-svydesign(id=~1,data=clean_data)

## because there are no sampling weights, each observation as a probability of 1 of being sampled at this stage. 

a1$prob #all probs are 1

## The next line uses the survey design object a1, and the variables we will adjust for, with the population corrections
## from the data frames from above.

a1.rake<-rake(a1,
             sample=list(~age_group,
                         ~race,
                         ~income_ord,
                         ~location),
             population=list(ages,races,income_ords,locations))

## The sampling probabilities have now changed after the correction

a1.rake$prob #probabilities have changed


### Model ###

## the first model is a simple logistic regression with fixed effects only svyglm uses the glm
## function to calculate, so it does not allow for random effects.
## The model uses the response to first dose vaccine status (yes or no) using income, age group,
## race, and the geographic areas

model1<-svyglm(first_dose_m~income_ord+age_group+Geographic.area+race,
              design=a1.rake,
              family = quasibinomial(), 
              control= list(maxit=25))


## helps visualize the model output

model1 %>% tbl_regression(exponentiate = TRUE)

## There are some differences. A model with interaction of Geographic area and race (to see if there was 
## an interaction effect of both) had singularity issues.


## Using a GLMM. This paper, talks about when one wants to run a regression using data with weights. 
## https://www.jstor.org/stable/26408229#metadata_info_tab_contents (it was written by T. Lumley, the 
## author of the survey package). The paper is a bit technical for me, but as I understand it, 
## it shows weighted regression can be done just for GLMs. GLMMs can be run on the data but those are 
## unweighted. 
## It says  "if additionally, the mean model is correctly specified, weighted and unweighted
##            regressions will give estimators consistent for the same parameter". 
## the paper has examples of running a GLM and a GLMM for logistic regression. and it has 
## a repository https://github.com/tslumley/regression-paper
## The relevant script for me is "ish_regression.R" Because it has the models used in the paper. The model
## "unwtclus" is the GLMM the authors used. I will follow the same approach below to incorporate random effects by region


model2 <- glmer(first_dose_m~(1|Geographic.area)+race+age_group+income_ord,data=model.frame(a1.rake),family=binomial)

## This model has singularity issues and it seems geographic regions as a random effect
## is not being useful. 

summary(model2)


## It seems so far that I need to re-think the model and see what other options there are.