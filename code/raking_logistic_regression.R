#Each of these dataframes provides the population totals for the different variables and their categories

# clean_data %>%
#   tbl_summary(
#     include=c(
#     Health_Region,)
#   ) %>% bold_labels()


Ages<-data.frame(Age_group=c("16_24",
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



Races<-data.frame(Race=c("arab_middle_eastern",
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

Incomes<-data.frame(Income=c("under_15000",
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


# corrections for Health Regions, without the North East and North West regions

Health_Regions<-data.frame(Health_Region=c(
  #"North West",
  #"North East",
  "West",
  "East",
  "Central",
  "Toronto"),
  Freq=c(#232299,
    #557000,
    4095589,
    3742520,
    5032410,
    1440644
  )
)



### Raking ####

## First, provide a survey design object. This is done using the dataset. The syntax
## means that there is no stratification (id=~1). When the line below is run it will throw
## a warning because we do not have "design" weights in the dataset (other datasets do have this).


a1<-svydesign(id=~1,data=clean_data)

## because there are no sampling weights, each observation as a probability of 1 of being sampled at this stage. 


## The next line uses the survey design object a1, and the variables we will adjust for, with the population corrections
## from the data frames from above.

a1_rake<-rake(a1,
              sample=list(~Age_group,
                          ~Race,
                          ~Income,
                          ~Health_Region),
              population=list(Ages,
                              Races,
                              Incomes,
                              Health_Regions))

# logistic regression model

m1<-svyglm(first_dose_m~Age_group+Race+Health_Region*Income+Health_Region+Age_group*Income+Race*Income,
               design=a1_rake,
               family = quasibinomial(), 
               control= list(maxit=25))

