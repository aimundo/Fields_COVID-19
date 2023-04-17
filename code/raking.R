#Each of these dataframes provides the population totals for the different variables and their categories


Ages<-data.frame(Age_group_ord=c("16-34",
                                 "35-54",
                                 "55 and over"),
                 Freq=c(3442815,
                        3725233,
                        4088342))



Races<-data.frame(Race=c("Arab/Middle Eastern",
                         "Black",
                         "East Asian/Pacific Islander",
                         "Indigenous",
                         "Latin American",
                         "Mixed",
                         "Other",
                         "South Asian",
                         "White/Caucasian"),
                  Freq=c(212782,
                         638346,
                         886592,
                         376558,
                         197020,
                         130033,
                         705333,
                         1166361,
                         9118079 ))

Incomes<-data.frame(income_ord=c("under 25000",
                             "25000-59999",
                             "60000 and above"),
                    Freq=c(682331,
                           1395676,
                           3091164
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
              sample=list(~Age_group_ord,
                          ~Race,
                          ~income_ord,
                          ~Health_Region),
              population=list(Ages,
                              Races,
                              Incomes,
                              Health_Regions))
