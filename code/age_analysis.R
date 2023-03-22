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

ggplot(data=clean_data,aes(x=age,y=first_dose_m,group=age))+geom_bar(stat="identity")


test2 <- clean_data %>% 
  group_by(age) %>% 
  count(first_dose_m) %>% 
  mutate(pct = n / sum(n)) 

# Plot 
ggplot(test2, aes(x = age, y = pct, fill = age)) +
  geom_col(position = "dodge") +
  facet_wrap(~ age) 
