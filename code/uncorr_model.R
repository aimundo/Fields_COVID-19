## Script to create plot from model fitted to uncorrected data

mplot1<-modelplot(m0,draw=FALSE,
                  coef_omit = "Intercept", exponentiate = TRUE) %>%
  mutate(term=str_remove_all(term,"Race|Age_group_ord|Month|Health_Region|income_ord"))

#add column by factor 
mplot1$factor<-c(rep("Age",2),
                 rep("Month",3),
                 rep("Income",2),
                 rep("Race/Ethnicity",8),
                 rep("Health Region",3),
                 rep("Income and\nRace/Ethnicity",16),
                 rep("Race/Ethnicity\nand\nHealth Region",24)
)

#order factors for plot
#mplot1$term_ord <- factor(mplot1$term,ordered=TRUE,levels=mplot1$term)

mplot1$factor <- factor(mplot1$factor,ordered=TRUE,levels=c("Age",
                                                            "Month",
                                                            "Income",
                                                            "Race/Ethnicity",
                                                            "Health Region",
                                                            "Income and\nRace/Ethnicity",
                                                            "Race/Ethnicity\nand\nHealth Region"))

# identify non-significant interaction factors

test<-mplot1%>%
  filter(str_detect(term,':') & p.value>0.05)

#remove non-significant interactions from dataframe
mplot1<-anti_join(mplot1,test)

# fix legend for plotting
mplot1<-mplot1%>%
  mutate(term=str_replace_all(term,':'," and "))

#make plot
uncorr_mod<-mplot1 %>% 
  ggplot(aes(y=term))+
  geom_vline(xintercept = 1, linetype="dashed",color="red") +
  geom_point(aes(x=estimate),size=2)+
  geom_linerange(aes(xmin=conf.low,xmax=conf.high))+
  scale_y_discrete(limits=rev)+
  xlab("")+
  ylab("")+
  facet_wrap(~factor,ncol=1,strip.position = "right",scales = "free_y",labeller = label_wrap_gen(multi_line = TRUE))+
  theme_classic()

