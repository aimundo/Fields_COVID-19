## script to analyze missing data pattern in the original dataset and compare with
## the clean dataset
library(here)
library(tidyverse)
library(naniar)
library(gtsummary)

rawdata<-read.csv(here("data","Fields_data.csv"),na.strings = c(""))
clean_data <-read.csv(here("data","clean_dataset.csv"))

## select the variables used in the analysis

rawdata <- rawdata %>%
 select(age,age_group,q09_hh_income,q16_race,city,q02_first_dose,complete)


# recode variables to match the column names from the clean dataset , and re-group age and income



rawdata <- rawdata %>%
  rename(first_dose=q02_first_dose,
         income=q09_hh_income,
         race=q16_race,
  )%>%
  mutate(across(c("age_group",
                  "city",
                  "income",
                  "race"),as.factor))


## Recode income

rawdata<-rawdata%>%
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

rawdata<-rawdata%>%
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

rawdata <- rawdata %>%
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
  ))

## so far I think we will assume that the order of the questions (which according to the
## data dictionary, between questions 4-15 was random) does not influence the pattern of missingness.

## So what is the mechanism of missingess here? People could withdraw at any point from the survey. However, there

## using the naniar package

gg_miss_var(rawdata, show_pct = TRUE)

vis_miss(rawdata)


rawdata %>%
  select(-income,-race)%>%
gg_miss_upset(nsets=5,nintersects=NA)

gg_miss_fct(x=rawdata,fct=first_dose)
