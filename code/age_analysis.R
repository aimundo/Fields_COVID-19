library(patchwork)

clean_data %>%
  tbl_summary(percent="row",
              by= first_dose,
              include=c(income_ord,
                        Age_group,
                        Health_Region,
                        Race),
              label=list(Health_Region ~ "Health Region",
                         Age_group ~ "Age Group",
                         income_ord="Income")
  ) %>% bold_labels()


clean_data$test<-as.factor(clean_data$age)

clean_data %>%
  tbl_summary(percent="row",
              by= first_dose,
              include=test)%>% 
  bold_labels()

##important: according to the data dictionary, the age of those >75 was
## coded as "98"
ggplot(data=clean_data,
       aes(x=age,y=first_dose_m,group=age))+
  geom_bar(stat="identity")


test2 <- clean_data %>% 
  group_by(age,Age_group,Age_group_ord) %>% 
  count(first_dose_m) %>% 
  mutate(pct = n / sum(n)) 

# Plot 
a<-test2 %>%
  subset(first_dose_m==1 & Age_group_ord=="16_34")%>%
ggplot(aes(x = age, y = pct, fill = age)) +
  geom_smooth(show.legend = FALSE)+
 geom_point(show.legend = FALSE)+
  facet_wrap(~Age_group_ord)

b<-test2 %>%
  subset(first_dose_m==1 & Age_group_ord=="35_54")%>%
  ggplot(aes(x = age, y = pct, fill = age)) +
  geom_smooth(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Age_group_ord)

c<-test2 %>%
  subset(first_dose_m==1 & Age_group_ord=="55_and_over")%>%
  ggplot(aes(x = age, y = pct, fill = age)) +
  geom_smooth(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Age_group_ord)

a+b+c


## try to get percentage of people that said yes per age group and plot it

