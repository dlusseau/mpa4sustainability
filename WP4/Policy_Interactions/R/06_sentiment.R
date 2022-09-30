
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("sentimentr")
library("tidyverse")
library("magrittr")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------

Q1.sent.text <- read.csv(file = "WP4/Policy_Interactions/data/05_Q1.cleansent.text.csv")

Q2.sent.text <- read.csv(file = "WP4/Policy_Interactions/data/05_Q2.cleansent.text.csv")


#---------------------------------------------------------------------------
#------------- This is the analysis on the first search query  -------------
#----------------------- "marine protected " -------------------------------
#---------------------------------------------------------------------------


Q1.sent.bytheme <- 
  Q1.sent.text %>%
  distinct(doc_id,MT, .keep_all = TRUE) %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(MT))

plot(sent.bytheme)

Q1.sent.bydocument <- 
  Q1.sent.text %>%
  distinct(doc_id,MT, .keep_all = TRUE) %>%
  group_by(MT) %>%
  summarise(n = n_distinct(doc_id)) 

# double check and compare these two outputs... 
# the functions might not take ito account duplicates due to multiple theme associations


Q1.sent.byresourcetype <- 
  Q1.sent.text %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(resource.type))



#---------------------------------------------------------------------------
#------------- This is the analysis on the second search query  ------------
#----------------------- 18 terms all diff types of ------------------------
#-------------------------- MPA Designation names --------------------------
#---------------------------------------------------------------------------


Q2.sent.bydocument <- 
  Q2.sent.text %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(doc_id))

Q2.sent2.resource.type <- 
  Q2.sent.text %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(resource.type))


# Save files ---------------------------------------------------------------------


