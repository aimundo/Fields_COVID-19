## data preparation for map, including the North West and North East Health Regions

# this script takes the clean dataset generated from the data cleaning process and does some preparatory steps such as 
# variable recoding, creating a variable for the dependent variable in the regression, and creating 
# a summary table of the dataset

data<-read.csv(here("data","clean_dataset.csv"))

# remove rows with missing observations for race

clean_data<-data%>%drop_na(race)

#remove "municipal status" as is not part of the analysis (to avoid unexpected errors)

clean_data<-clean_data %>%
  select(-Municipal.status) %>%
  rename(Race=race,Age_group=age_group,Income=income_ord)

#make variables of interest factors
clean_data<-clean_data %<>%
  mutate_at(c("Income","Race","Age_group","Health_Region"),factor)

# recode the response of interest for categorical analysis
clean_data<-clean_data %>%
  mutate(first_dose_m=case_when(
    first_dose=="yes"~1,
    first_dose=="no"~0
  ))

