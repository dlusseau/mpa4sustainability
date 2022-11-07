

# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("dplyr")
library("ggplot2")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------


DK.metadata <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.DKdocmetadata.clean.csv")
SE.metadata <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.SEdocmetadata.clean.csv")

DK.metadata %>%
  group_by(search.term, År) %>%
  summarise(n=n_distinct(url)) %>%
  ungroup() %>%
  mutate(search.term = as.factor(search.term)) %>%
  ggplot(aes(fill=search.term, y=n, x=År)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~search.term,   nrow = 3)+
  ylab("Number of legislations") + 
  xlab("Year")+
  theme(legend.position = "bottom")


SE.metadata %>%
  mutate(year=year(datum)) %>%
  mutate(search.term = as.factor(search.term)) %>%
  group_by(search.term, year) %>%
  summarise(n=n_distinct(doc.id)) %>%
  ungroup() %>%
  ggplot(aes(fill=search.term, y=n, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~search.term,   nrow = 3)+
  ylab("Number of legislations") + 
  xlab("Year")+
  theme(legend.position = "bottom")
