## Script to clean and prepare the raw dataset for analysis
## Upated on May 8, 2023

rawdata<-read.csv(here("data","Fields_data.csv"),na.strings = c(""))
#clean_data <-read.csv(here("data","clean_dataset.csv"))

## select the variables used in the analysis

## household income
## race
## city
## age group
## month


rawdata <- rawdata %>%
  select(age,age_group,q09_hh_income,q16_race,city,q02_first_dose,complete,time_date_code,q13_hh_size)

#miss_raw<-miss_var_summary(rawdata, show_pct = TRUE)

## summary of missing data in the raw dataset

# miss_raw %>%
#   kableExtra::kbl() %>%
#   kableExtra::kable_styling()

# rates: first dose 

# The dataset had a column called "complete". The data dictionary was not
# clear about what this meant, it seems that observations that are "complete" 
# have complete observations for vaccination status, and all other covariates, except race, with a 4%
# of missing obs.

clean_data<-rawdata %>%
  subset(complete=="complete") 

# miss_var_summary(clean_data,show_pct=TRUE) %>%
#   kableExtra::kbl() %>%
#   kableExtra::kable_styling()

# remove cities entered as "None"

clean_data<-clean_data %>%
  subset(city!="None")

#recode variable names

clean_data <- clean_data %>%
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
    age_group %in% "16_24" ~ "16-34",
    age_group %in% "25_34" ~ "16-34",
    age_group %in% "35_44" ~ "35-54",
    age_group %in% "45_54" ~ "35-54",
    age_group %in% "55_64" ~ "55 and over",
    age_group %in% "65_and_over" ~ "55 and over",
  ))%>%
  mutate(Age_group_ord=as.factor(Age_group_ord))

#fix Race/Ethnicity levels

clean_data<- clean_data %>%
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


clean_data<-left_join(clean_data,municipalities,by="city")


missing_municipalities<-read.csv(here("data","missing_municipalities_updated_May_05_2023.csv"))


#combining geographical regions

clean_data<-clean_data %>%
  left_join(missing_municipalities, by = c("city")) %>%
  mutate(Geographic.area = coalesce(Geographic.area.x,Geographic.area.y)) %>%
  select(-c(Geographic.area.x,Geographic.area.y))

clean_data$Geographic.area<-as.factor(clean_data$Geographic.area)

## what cities are missing geographical region

t1<-clean_data %>%
  filter(is.na(Geographic.area))

## no areas missing, good!

# load the the titles for each region

geographic_areas<-read.csv(here("data","geographic_areas.csv"))

geographic_areas$Geographic_area<-as.factor(geographic_areas$Geographic_area)

geographic_areas$Geographic_area_title <-as.factor(geographic_areas$Geographic_area_title)

#merge the datasets

clean_data <-left_join(clean_data,geographic_areas,by=c("Geographic.area"="Geographic_area")) %>%
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


clean_data<-left_join(clean_data,ltc_data,by="city")


missing_health_regions<-clean_data %>% filter(is.na(LHIN)) %>%
  distinct(city,.keep_all = TRUE)%>%
  select(city,Geographic.area,Geographic_area_title,LHIN,Health_Region)

## keep in mind that Mitchell was coded as Mitchell/Ontario in the dataset, might need to be chagned

mhru<-read.csv(here("data","missing_health_regions_updated_May_05_2023.csv"))

#rename columns
mhru <- mhru %>%
  rename(Geographic_area_title=Geographic.area.title)


clean_data<-clean_data %>%
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

## find cities with missing Health Region

t1<-clean_data %>%
  filter(is.na(Health_Region))%>%
  select(city,Geographic_area,Geographic_area_title,LHIN)

# great, no missing obs!

# recode the response of interest for categorical analysis
clean_data<-clean_data %>%
  mutate(first_dose_m=case_when(
    first_dose=="yes"~1,
    first_dose=="no"~0
  ))

# get month

## get month from the data

clean_data$date <-as_date(clean_data$date)


clean_data<-clean_data %>%
  mutate(Month=format(date,"%B"))

clean_data<-clean_data %>%
  mutate(MonthDay=as.numeric(format(date,"%m.%d")))


clean_data$Month <- as.factor(clean_data$Month)

#remove september as has too few obs.

clean_data <- clean_data%>%
  subset(Month!="September")

clean_data$Month <- fct_relevel(clean_data$Month,"October","November","December","January")

clean_data<-clean_data%>%
  mutate(Month=
           case_when(Month=="October"~ "October 2021",
                     Month=="November"~"November 2021",
                     Month=="December"~"December 2021",
                     Month=="January"~"January 2022"
           ))%>%
  mutate(Month=as.factor(Month))

clean_data$Month <- fct_relevel(clean_data$Month,"October 2021","November 2021","December 2021","January 2022")

# Remove certain Health Regions due to low number of observations: only 254 in North East
# and North West Regions at this point, in contrast with about a thousand from each 
# other region

#get health regions to eliminate
clean_data <-clean_data %>%
  filter(Health_Region!="North East" , Health_Region!="North West")

clean_data<-droplevels(clean_data)

# remove for the time being, obs with missing race or missing HR.
# 262 obs without race, they are from Central, East, Toronto, and West Health Regions


clean_data<-clean_data %>%
  drop_na(Race,Health_Region)

# 6,255 obs

## clean_data for outliers: individuals under 25, hh 1 and income >110k. A total of 
# 19 observations that need to be removed

outliers<-clean_data %>%
  filter(age_group=="16_24" &
           q13_hh_size==1 &
           income=="over_110000")

clean_data<-anti_join(clean_data,outliers)

clean_data$Health_Region <-as.factor(clean_data$Health_Region)

clean_data<- within(clean_data, Age_group_ord <- relevel(Age_group_ord, ref = "16-34"))
clean_data<- within(clean_data, Race <- relevel(Race, ref = "White/Caucasian"))
clean_data<- within(clean_data, Health_Region <- relevel(Health_Region, ref = "Toronto"))
clean_data<- within(clean_data, income_ord <- relevel(income_ord, ref = "60000 and above"))
clean_data<- within(clean_data, Month <- relevel(Month, ref = "October 2021"))
