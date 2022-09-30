
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries -----------------------------------------------------------

library("dplyr")
library("stringr")
library("tm")
library("corpus")
library("tidyr")
library("lubridate")
library("rvest")
library("readr")

# Define functions ---------------------------------------------------------

# No defined function for this script

# Load data ----------------------------------------------------------------

# Our document-data key
document.key.df <- read.csv(file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

# EU mpa designation term associated text data: 
MPA.DESG.text <- read.csv(file = "WP4/Policy_Interactions/data/01_mpaterms.text.data.dup.csv")

# "marine protected" associated text data:
html_encoding_guess("WP4/Policy_Interactions/data/01_CELEXmpa.text.data.csv")

mar.protected.text <- read_csv(file = "WP4/Policy_Interactions/data/01_CELEXmpa.text.data.csv",
                      locale = locale(encoding = "ISO-8859-1"),
                      show_col_types = FALSE)


# EU mpa characteristics: 
EU.mpachar <- read.csv(file = "WP4/Policy_Interactions/data/01_EU.mpachar.csv")


# Cleaning & Pre-processing the text data -----------------------------------

smaller.key.df <-
  document.key.df %>%
  select(celex,date,force) %>% 
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  distinct()
# I wanted to keep theme for the meta-data in the corpus but a few Celex have 
# multiple themes and they are very different from eachother unlike the countries

EU.mpachar %>%
  group_by(DESIG_ENG) %>%
  summarise(n.mpas = n_distinct(mpa))

EU.mpachar %>%
  group_by(DESIG_ENG) %>%
  summarise(n.countries = n_distinct(PARENT_ISO))

EU.mpachar %>%
  filter(DESIG_ENG == "sites of community importance (habitats directive)" |
         DESIG_ENG == "special areas of conservation (habitats directive)") %>%
  summarise( n = n_distinct(PARENT_ISO))

#----------------------------------------------------------------------------
#----------------- This is the the first search query  ----------------------
#----------------------- "marine protected " --------------------------------
#----------------------------------------------------------------------------

# lets make it into the good df format

n_distinct(mar.protected.text$CELEX)
# 25 documents

mar.protected.text.df <- 
  mar.protected.text %>%
  filter(CELEX != "32021R0092") %>% # same filter out as in 03 Rscript
  select(resource.type,CELEX,url,total.text) %>%
  mutate(resource.type = as.factor(resource.type),
         doc_id = as.factor(CELEX),
         url = as.factor(url)) %>%
  rename("text" = "total.text") %>%
  select(doc_id,text,resource.type,url)

# cleaning and pre-processing text data for sentiment analysis
sent.key <- 
  document.key.df %>%
  select(celex,date,force,labels,MT) %>% 
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  distinct()

mar.protected.text.df.clean.SENT <-
  mar.protected.text.df %>%
  mutate(clean.text = tolower(text),                                       # convert all to lower case
         clean.text = str_replace_all(clean.text,"[:digit:]",""),          # remove numbers
         clean.text = str_replace_all(clean.text, "[^[:graph:]]"," ")) %>% # remove all special characters
  group_by(doc_id) %>%
  distinct(doc_id, .keep_all=TRUE) %>%
  select(doc_id,clean.text,resource.type,url) %>%
  rename("text" = "clean.text") %>%
  ungroup() %>%
  left_join(.,sent.key, by = c("doc_id" = "celex"))

# cleaning and pre-processing text data for a corpus for text mining models
mar.protected.text.df.clean <-
  mar.protected.text.df %>%
  mutate(clean.text = tolower(text),                                   # convert all to lower case
         clean.text = str_replace_all(clean.text,"[:punct:]",""),      # remove punctuation
         clean.text = str_replace_all(clean.text,"[:digit:]",""),      # remove numbers
         clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "), # remove all special characters
         clean.text = removeWords(clean.text,stopwords("en"))) %>%     # remove stop words
  mutate(clean.text = text_tokens(.$clean.text, stemmer = "en")) %>%   # stemming words
  unnest(clean.text) %>%
  group_by(doc_id) %>%
  mutate(clean.text = paste(clean.text, collapse = " ")) %>%
  distinct(doc_id, .keep_all=TRUE) %>%
  select(doc_id,clean.text,resource.type,url) %>%
  rename("text" = "clean.text") %>%
  ungroup() %>%
  as.data.frame()

#possibly add force, date/year and theme so we have it in the meta-data of the corpus
mar.protected.text.df.clean <-
  mar.protected.text.df.clean %>%
  left_join(. , smaller.key.df, by= c("doc_id" = "celex"))

# lets make it into a corpus object
mar.protected.corpus <- DataframeSource(mar.protected.text.df.clean)
mar.protected.corpus <- SimpleCorpus(mar.protected.corpus, control = list(language = "en"))

meta(mar.protected.corpus)

# OK so now we have a cleaned corpus: 

  # convert corpus to a document-term matrix
  # document term matrix: lists word occurances within a document 
  dtm.Q1 <- DocumentTermMatrix(mar.protected.corpus)
  inspect(dtm.Q1)
  
  # convert corpus to a term-document matrix
  # document term matrix: lists word occurances within a document 
  tdm.Q1 <- TermDocumentMatrix(mar.protected.corpus)
  inspect(tdm.Q1)
  

#---------------------------------------------------------------------------
#--------------------- This is the second search query  --------------------
#----------------------- 18 terms all diff types of ------------------------
#-------------------------- MPA Designation names --------------------------
#---------------------------------------------------------------------------

n_distinct(MPA.DESG.text$CELEX)
#180 documents

MPA.DESG.text.df <- 
  MPA.DESG.text %>%
  select(resource.type, CELEX, search.term, force, url,total.text) %>%
mutate(resource.type = as.factor(resource.type),
       search.term = as.factor(search.term),
       force = as.factor(force),
       doc_id = as.factor(CELEX),
       url = as.factor(url)) %>%
  rename("text" = "total.text") %>%
  select(doc_id,text,resource.type,url,search.term,force)

# cleaning and pre-processing text data for sentiment analysis
MPA.DESG.text.df.SENT <-
  MPA.DESG.text.df %>%
  mutate(clean.text = tolower(text),                                       # convert all to lower case
         clean.text = str_replace_all(clean.text,"[:digit:]",""),          # remove numbers
         clean.text = str_replace_all(clean.text, "[^[:graph:]]"," ")) %>% # remove all special characters
  group_by(doc_id) %>%
  distinct(doc_id, .keep_all=TRUE) %>%
  select(doc_id,clean.text,resource.type,url) %>%
  rename("text" = "clean.text") %>%
  ungroup()

# cleaning and pre-processing text data 
MPA.DESG.text.df.clean <-
  MPA.DESG.text.df %>%
  mutate(clean.text = tolower(text),                                   # convert all to lower case
         clean.text = str_replace_all(clean.text,"[:punct:]",""),      # remove punctuation
         clean.text = str_replace_all(clean.text,"[:digit:]",""),      # remove numbers
         clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "), # remove all special characters
         clean.text = removeWords(clean.text,stopwords("en"))) %>%     # remove stop words
  mutate(clean.text = text_tokens(.$clean.text, stemmer = "en")) %>%   # stemming words
  unnest(clean.text) %>%
  group_by(doc_id) %>%
  mutate(clean.text = paste(clean.text, collapse = " ")) %>%
  distinct(doc_id, .keep_all=TRUE) %>%
  select(doc_id,clean.text,resource.type,url,search.term,force) %>%
  rename("text" = "clean.text") %>%
  ungroup() %>%
  as.data.frame()

#possibly add date and theme so we have it in the meta-data of the corpus
MPA.DESG.text.df.clean <-
  smaller.key.df %>%
  select(-force) %>% # already has force info in df
  right_join(. ,MPA.DESG.text.df.clean , by= c("celex" = "doc_id")) %>%
  rename("doc_id" = "celex") %>% 
  select(doc_id,text,resource.type,url,search.term,force, date, year)
  

# lets make it into a corpus object
MPA.DESG.corpus <- DataframeSource(MPA.DESG.text.df.clean)
MPA.DESG.corpus <- SimpleCorpus(MPA.DESG.corpus, control = list(language = "en"))

meta(MPA.DESG.corpus)

# OK so now we have a cleaned corpus: 

# convert corpus to a document-term matrix
# document term matrix: lists word occurances within a document 
dtm.Q2 <- DocumentTermMatrix(MPA.DESG.corpus)
inspect(dtm.Q2)

# convert corpus to a term-document matrix
# document term matrix: lists word occurances within a document 
tdm.Q2 <- TermDocumentMatrix(MPA.DESG.corpus)
inspect(tdm.Q2)



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

