
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("ggplot2")
library("lubridate")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------
mpa.policy.notext.df <- read.csv(file = "WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

# Exploring eurvoc terms --------------------------------------------------

mpa.policy.notext.df %>%
  summarise(n= n_distinct(CELEX)) 
# total 1,104 policy legislation

mpa.policy.notext.df %>%
  summarise(n= n_distinct(labels)) 
# total 1,302 unique label terms 

mpa.policy.notext.df %>%
  group_by(CELEX) %>%
  summarise(n_labels = (n_distinct(labels))) %>%
  summary(n_labels)

# n_labels     
# Min.   : 1.000  
# 1st Qu.: 5.000  
# Median : 6.000  
# Mean   : 6.253  
# 3rd Qu.: 8.000  
# Max.   :16.000 


# exploration code from the online tutorial ------

# what is the prop of directives that are currently enforced?

# which of the older acts are still enforces??
mpa.policy.notext.df %>%
  filter(!is.na(force)) %>% 
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  group_by(year,force) %>%
  summarise(n=n_distinct(CELEX)) %>%
  ggplot(aes(fill=force, x = year, y = n)) +
  geom_bar(position="stack", stat="identity")

# in general a clear increase in leg. documents relating to marine protected area*
# also we see that ones that there are more enforced documents from more recent years than the other way around, 
# but this is not suprising
  
