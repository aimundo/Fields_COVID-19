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

# recode the response of interest for categorical analysis
clean_data<-clean_data %>%
  mutate(first_dose_m=case_when(
    first_dose=="yes"~1,
    first_dose=="no"~0
  ))

#get health regions to eliminate
clean_data <-clean_data %>%
  filter(Health_Region!="North East" , Health_Region!="North West")

## Recode income

clean_data<-clean_data%>%
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

clean_data<-clean_data%>%
  mutate(Age_group_ord = case_when(
    Age_group %in% "16_24" ~ "16-34",
    Age_group %in% "25_34" ~ "16-34",
    Age_group %in% "35_44" ~ "35-54",
    Age_group %in% "45_54" ~ "35-54",
    Age_group %in% "55_64" ~ "55 and over",
    Age_group %in% "65_and_over" ~ "55 and over",
  ))%>%
  mutate(Age_group_ord=as.factor(Age_group_ord))

#fix Race/Ethnicity levels

clean_data <-clean_data %>%
  mutate(Race= case_when(
    Race %in% "white_caucasian" ~ "White/Caucasian",
    Race %in% "arab_middle_eastern" ~ "Arab/Middle Eastern",
    Race %in% "black" ~ "Black",
    Race %in% "east_asian_pacific_islander" ~ "East Asian/Pacific Islander",
    Race %in% "indigenous" ~ "Indigenous",
    Race %in% "latin_american" ~ "Latin American",
    Race %in% "mixed" ~ "Mixed",
    Race %in% "other" ~ "Other",
    Race %in% "south_asian" ~ "South Asian"
  ))

## get month from the data

clean_data$date <-as_date(clean_data$date)


clean_data<-clean_data %>%
  mutate(Month=format(date,"%B"))

clean_data<-clean_data %>%
  mutate(MonthDay=as.numeric(format(date,"%m.%d")))


clean_data$Month <- as.factor(clean_data$Month)
clean_data$Month <- fct_relevel(clean_data$Month,levels=c("October","November","December"))

#make variables of interest factors
clean_data<-clean_data %<>%
  mutate_at(c("income_ord","Race","Age_group_ord","Health_Region"),factor)


## relevel for reference groups in the model

clean_data<- within(clean_data, Age_group_ord <- relevel(Age_group_ord, ref = "16-34"))
clean_data<- within(clean_data, Race <- relevel(Race, ref = "White/Caucasian"))
clean_data<- within(clean_data, Health_Region <- relevel(Health_Region, ref = "Toronto"))
clean_data<- within(clean_data, income_ord <- relevel(income_ord, ref = "60000 and above"))
#clean_data<- within(clean_data, Month <- relevel(Month, ref = "October"))

# remove september, as it only has 2 observations

clean_data <-clean_data %>%
  subset(Month!="September")

#eliminate the levels of the removed health regions
clean_data <- droplevels(clean_data)


