library(here)
library(tidyverse)
library(naniar)
library(gtsummary)
library(lubridate)

rawdata<-read.csv(here("data","Fields_data.csv"),na.strings = c(""))
clean_data <-read.csv(here("data","clean_dataset.csv"))

## select the variables used in the analysis

rawdata <- rawdata %>%
  select(age,age_group,q09_hh_income,q16_race,city,q02_first_dose,complete,time_date_code)

# The dataset had a column called "complete". The data dictionary was not
# clear about what this meant, it seems that observations that are "complete" 
# have complete observations for vaccination status, and all other covariates, except race, with a 4%
# of missing obs.

test<-rawdata %>%
  subset(complete=="complete") 

test <- test %>%
  rename(date=time_date_code,
    first_dose=q02_first_dose,
         income=q09_hh_income,
         race=q16_race,
  )%>%
  mutate(across(c("age_group",
                  "city",
                  "income",
                  "race"),as.factor))


## Recode income

test<-test%>%
  mutate(income_ord = case_when(
    income %in% "under_15000" ~ "under 25000",
    income %in% "15000_24999" ~ "under 25000",
    income %in% "25000_39999" ~ "25000-59999",
    income %in% "40000_59999" ~ "25000-59999",
    income %in% "60000_89999" ~ "60000 and above",
    income %in% "90000_109999" ~ "60000 and above",
    income %in%"over_110000" ~ "60000 and above",
  ))%>%
  mutate(income_ord=as.factor(income_ord))

#re-group age groups

test<-test%>%
  mutate(Age_group_ord = case_when(
    age_group %in% "16_24" ~ "16-34",
    age_group %in% "25_34" ~ "16-34",
    age_group %in% "35_44" ~ "35-54",
    age_group %in% "45_54" ~ "35-54",
    age_group %in% "55_64" ~ "55 and over",
    age_group %in% "65_and_over" ~ "55 and over",
  ))%>%
  mutate(Age_group_ord=as.factor(Age_group_ord))

#fix Race/Ethnicity levels

test<- test %>%
  mutate(Race= case_when(
    race %in% "white_caucasian" ~ "White/Caucasian",
    race %in% "arab_middle_eastern" ~ "Arab/Middle Eastern",
    race %in% "black" ~ "Black",
    race %in% "east_asian_pacific_islander" ~ "East Asian/Pacific Islander",
    race %in% "indigenous" ~ "Indigenous",
    race %in% "latin_american" ~ "Latin American",
    race %in% "mixed" ~ "Mixed",
    race %in% "other" ~ "Other",
    race %in% "south_asian" ~ "South Asian"
  ))%>%
  mutate(Race=as.factor(Race))

# add municipal data

municipalities<-read.csv(here("data","municipalities_clean.csv"))

municipalities$Municipal.status<-as.factor(municipalities$Municipal.status)

municipalities$Geographic.area<-as.factor(municipalities$Geographic.area)

municipalities$city<-as.factor(municipalities$city)


test<-left_join(test,municipalities,by="city")

test %>%
  filter(is.na(Geographic.area))%>%
  distinct(city)


missing_municipalities<-read.csv(here("data","missing_municipalities_updated.csv"))


#combining geographical regions

test<-test %>%
  left_join(missing_municipalities, by = c("city")) %>%
  mutate(Geographic.area = coalesce(Geographic.area.x,Geographic.area.y)) %>%
  select(-c(Geographic.area.x,Geographic.area.y))

test$Geographic.area<-as.factor(test$Geographic.area)

# load the the titles for each region

geographic_areas<-read.csv(here("data","geographic_areas.csv"))

geographic_areas$Geographic_area<-as.factor(geographic_areas$Geographic_area)

geographic_areas$Geographic_area_title <-as.factor(geographic_areas$Geographic_area_title)

#merge the datasets

test <-left_join(test,geographic_areas,by=c("Geographic.area"="Geographic_area")) %>%
  subset(select=-c(Full_title.y))




## add LHINs

#load data
ltc_data <- read.csv(here("data","Consolidated_LTC_dataset.csv"))

## checking missing observations

sum(ltc_data$X_LHIN=="Not provided")

ltc_data <- ltc_data %>%
  mutate(X_LHIN=replace(X_LHIN,X_LHIN=="Not provided","South East"))

ltc_data <- ltc_data %>%
  mutate(COMMUNITY=replace(COMMUNITY,COMMUNITY==" 244 Main Street East","Stayner"))

#keep relevant columns and add Health region information
ltc_data <-ltc_data %>%
  select(c("COMMUNITY","X_LHIN"))%>%
  rename(city=COMMUNITY,
         LHIN=X_LHIN)%>%
  mutate(Health_Region=
           case_when(LHIN=="Central"~ "Central",
                     LHIN=="Central West"~"Central",
                     LHIN=="Mississauga Halton"~"Central",
                     LHIN=="North Simcoe Muskoka"~"Central",
                     LHIN=="Central East"~"East",
                     LHIN=="South East"~"East",
                     LHIN=="Champlain"~"East",
                     LHIN=="North East"~"North East",
                     LHIN=="North West"~"North West",
                     LHIN=="Toronto Central"~"Toronto",
                     LHIN=="South West"~"West",
                     LHIN=="Hamilton Niagara Haldimand Brant (Hnhb)"~"West",
                     LHIN=="Waterloo Wellington"~"West",
                     LHIN=="Erie St. Clair"~"West"
           )
  )%>%
  distinct(city,.keep_all = TRUE)



#Next, we combined the datasets and wrote csv file with those cities that were not assigned a Health Region. The next code chunk does these steps (note that the line for writing the csv file has ben commented as it was run only once).


test<-left_join(test,ltc_data,by="city")


missing_health_regions<-test %>% filter(is.na(LHIN)) %>%
  distinct(city,.keep_all = TRUE)%>%
  select(city,Geographic.area,Geographic_area_title,LHIN,Health_Region)



mhru<-read.csv(here("data","missing_health_regions_updated.csv"))

#rename columns
mhru <- mhru %>%
  rename(Geographic_area_title=Geographic.area.title)


test<-test %>%
  left_join(mhru, by = c("city")) %>%
  mutate(LHIN = coalesce(LHIN.x,LHIN.y)) %>%
  select(-c(LHIN.x,LHIN.y,Full_title.x,Geographic.area.y,Geographic_area_title.y))%>%
  mutate(Health_Region=
           case_when(LHIN=="Central"~ "Central",
                     LHIN=="Central West"~"Central",
                     LHIN=="Mississauga Halton"~"Central",
                     LHIN=="North Simcoe Muskoka"~"Central",
                     LHIN=="Central East"~"East",
                     LHIN=="South East"~"East",
                     LHIN=="Champlain"~"East",
                     LHIN=="North East"~"North East",
                     LHIN=="North West"~"North West",
                     LHIN=="Toronto Central"~"Toronto",
                     LHIN=="South West"~"West",
                     LHIN=="Hamilton Niagara Haldimand Brant (Hnhb)"~"West",
                     LHIN=="Waterloo Wellington"~"West",
                     LHIN=="Erie St. Clair"~"West"
           ))%>%
  rename(Geographic_area=Geographic.area.x,Geographic_area_title=Geographic_area_title.x)

# recode the response of interest for categorical analysis
test<-test %>%
  mutate(first_dose_m=case_when(
    first_dose=="yes"~1,
    first_dose=="no"~0
  ))

# get month

## get month from the data

test$date <-as_date(test$date)


test<-test %>%
  mutate(Month=format(date,"%B"))

test<-test %>%
  mutate(MonthDay=as.numeric(format(date,"%m.%d")))


test$Month <- as.factor(test$Month)

#remove september as has too few obs.

test <- test%>%
  subset(Month!="September")

test$Month <- fct_relevel(test$Month,levels=c("October","November","December","January"))


# Remove certain Health REgions due to low number of obs

#get health regions to eliminate
test <-test %>%
  filter(Health_Region!="North East" , Health_Region!="North West")

test<-droplevels(test)

# remove for the time being, obs with missing race or missing HR

test<-test %>%
  drop_na(Race,Health_Region)

test$Health_Region <-as.factor(test$Health_Region)

test<- within(test, Age_group_ord <- relevel(Age_group_ord, ref = "16-34"))
test<- within(test, Race <- relevel(Race, ref = "White/Caucasian"))
test<- within(test, Health_Region <- relevel(Health_Region, ref = "Toronto"))
test<- within(test, income_ord <- relevel(income_ord, ref = "60000 and above"))
test<- within(test, Month <- relevel(Month, ref = "October"))

test %>%
  tbl_summary(percent="row",
              by= first_dose,
              include=c(income_ord,
                        Age_group_ord,
                        Health_Region,
                        Month,
                        Race),
              label=list(Health_Region ~ "Health Region",
                         Age_group_ord ~ "Age Group",
                         income_ord~"Income (CAD)")
  ) %>% add_p(test = everything () ~ "chisq.test")%>%
  bold_labels() %>%
  modify_table_styling(
    columns=label,
    rows=label=="Other",
    footnote="Southeast Asian, Filipino, West Asian, \nand minorities not identified elsewhere according to the Census."
  )


# raking
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

library(survey)

### Raking ####

## First, provide a survey design object. This is done using the dataset. The syntax
## means that there is no stratification (id=~1). When the line below is run it will throw
## a warning because we do not have "design" weights in the dataset (other datasets do have this).


a1<-svydesign(id=~1,data=test)

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


m0<-svyglm(first_dose_m~Age_group_ord+Month+income_ord+Race+Health_Region+Race*income_ord+Race*Health_Region,
           design=a1,
           family = quasibinomial(), 
           control= list(maxit=25))


# model for corrected data

m2<-svyglm(first_dose_m~Age_group_ord+Month+income_ord+Race+Health_Region+Race*income_ord+Race*Health_Region,
           design=a1_rake,
           family = quasibinomial(), 
           control= list(maxit=25))


modelplot(m0,exponentiate = TRUE,coef_omit = "Intercept")+ geom_vline(xintercept = 1, linetype="dashed",color="red")
modelplot(m2,exponentiate = TRUE,coef_omit = "Intercept")+geom_vline(xintercept = 1, linetype="dashed",color="red")

tbl_regression(m0,exponentiate=TRUE) %>% 
  bold_labels()


tbl_regression(m2,exponentiate = TRUE) %>% 
  bold_labels()
