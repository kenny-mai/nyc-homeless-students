# Run doe.preprocess first to make sure doe.all is in the R environment
# Check the data
head(doe.all)

# Check percentages of ethnicity
doe.all %>% 
  group_by(eth) %>% 
  tally()

# Check graduation by ethnicity
doe.all %>% 
  group_by(eth) %>% 
  tally(graduate)

# Check college attendance by ethnicity
doe.all %>% 
  group_by(eth) %>% 
  filter(graduate == 1) %>% 
  tally(college)

doe.all %>% 
  group_by(eth) %>% 
  tally(any.iep)

doe.all %>% 
  group_by(hlang) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))  

doe.all %>% 
  mutate(domestic = ifelse())
  group_by(bplace) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))  
