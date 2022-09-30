
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("igraph")
library("widyr")
library("dplyr")
library("tibble")
library("tidyr")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------

Q1.sent.text <- read.csv(file = "WP4/Policy_Interactions/data/05_Q1.cleansent.text.csv")

Q2.sent.text <- read.csv(file = "WP4/Policy_Interactions/data/05_Q2.cleansent.text.csv")


#---------------------------------------------------------------------------
#------------- This is the analysis on the first search query  -------------
#----------------------- "marine protected " -------------------------------
#---------------------------------------------------------------------------




#---------------------------------------------------------------------------
#------------- This is the analysis on the second search query  ------------
#----------------------- 18 terms all diff types of ------------------------
#-------------------------- MPA Designation names --------------------------
#---------------------------------------------------------------------------


## getting familiar with sentiment analysis again

# Lets do sentiment scores: 
library("sentimentr")
library("tidyverse")
library("magrittr")

sent <- 
  mar.protected.text.df.clean.SENT %>%
  distinct(doc_id,MT, .keep_all = TRUE) %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(MT))

plot(sent)
plot(uncombine(sent))

x <- 
  mar.protected.text.df.clean.SENT %>%
  distinct(doc_id,MT, .keep_all = TRUE) %>%
  group_by(MT) %>%
  summarise(n = n_distinct(doc_id)) 


sent.resource.type <- 
  mar.protected.text.df.clean.SENT %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(resource.type))

sent2 <- 
  MPA.DESG.text.df.SENT %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(doc_id))

sent2.resource.type <- 
  MPA.DESG.text.df.SENT %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(resource.type))


