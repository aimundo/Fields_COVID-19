## Script to create plot for corrected model

mplot2<-modelplot(m2,draw=FALSE,coef_omit = "Intercept",exponentiate = TRUE) %>%
  mutate(term=str_remove_all(term,"Race|Age_group_ord|Month|Health_Region|income_ord"))

#add column by factor 
mplot2$factor<-c(rep("Age",2),
                 rep("Month",3),
                 rep("Income",2),
                 rep("Race/Ethnicity",8),
                 rep("Health Region",3),
                 rep("Income and\nRace/Ethnicity",16),
                 rep("Race/Ethnicity\nand\nHealth Region",24)
)

#order factors for plot
#mplot2$term_ord <- factor(mplot2$term,ordered=TRUE,levels=mplot2$term)

mplot2$factor <- factor(mplot2$factor,ordered=TRUE,levels=c("Age",
                                                            "Month",
                                                            "Income",
                                                            "Race/Ethnicity",
                                                            "Health Region",
                                                            "Income and\nRace/Ethnicity",
                                                            "Race/Ethnicity\nand\nHealth Region"))

# remove non-significant interaction factors

test<-mplot2%>%
  filter(str_detect(term,':') & p.value>0.05)

mplot2<-anti_join(mplot2,test)

mplot2<-mplot2%>%
  mutate(term=str_replace_all(term,':'," and "))


corr_mod<-mplot2 %>% 
  ggplot(aes(y=term))+
  geom_vline(xintercept = 1, linetype="dashed",color="red") +
  geom_point(aes(x=estimate),size=2)+
  geom_linerange(aes(xmin=conf.low,xmax=conf.high))+
  scale_y_discrete(limits=rev)+
  xlab("")+
  ylab("")+
  facet_wrap(~factor,ncol=1,strip.position = "right",scales = "free_y")+
  theme_classic()
