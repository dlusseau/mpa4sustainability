
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

# OK so now we have a cleaned corpus: 


  
library("sentimentr")

# cleaning and pre-processing whole text data for a corpus for topic models
  mar.protected.text.df.clean <-
    mar.protected.text.df %>%
    get_sentences() %>%
    mutate(clean.text = tolower(text),                                   # convert all to lower case
           clean.text = str_replace_all(clean.text,"[:punct:]",""),      # remove punctuation
           clean.text = str_replace_all(clean.text,"[:digit:]",""),      # remove numbers
           clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "), # remove all special characters
           clean.text = removeWords(clean.text,stopwords("en"))) %>%     # remove stop words --> tm package
    mutate(clean.text = text_tokens(.$clean.text, stemmer = "en")) %>%   # stemming words --> corpus package (tm stemming package did not work...was doing something strange)
    unnest(clean.text) %>%                # sentences that become NAs aftere cleaning are removed...
    group_by(element_id, sentence_id) %>%
    mutate(clean.text = paste(clean.text, collapse = " ")) %>%
    distinct(element_id, sentence_id, .keep_all=TRUE) %>%
    ungroup() %>%
    mutate(element_id = as.factor(element_id),
           sentence_id = as.factor(sentence_id)) %>%
    mutate(doc_id2 = paste(element_id, sentence_id, sep = "_")) %>%
    select(doc_id2,clean.text,resource.type,url,doc_id) %>%
    rename("text" = "clean.text",
           "CELEX" = "doc_id",
           "doc_id" = "doc_id2") %>%
    as.data.frame()
  
  
#possibly add force, date/year and theme so we have it in the meta-data of the corpus
  mar.protected.text.df.clean <-
    mar.protected.text.df.clean %>%
    left_join(. , smaller.key.df, by= c("CELEX" = "celex"))
  
    
# lets make it into a corpus object (tm package)
mar.protected.corpus <- DataframeSource(mar.protected.text.df.clean)
mar.protected.corpus <- SimpleCorpus(mar.protected.corpus, control = list(language = "en"))
  
mar.protected.corpus <- corpus(mar.protected.corpus) #should convert it to quanteda package formate since it is the only type I could get a successful conversion to stm
meta(mar.protected.corpus)
docvars(mar.protected.corpus)
ndoc(mar.protected.corpus)

mar.protected.dfm <- dfm(tokens(mar.protected.corpus)) # Create a document feature matrix
Q1.textprocessed <- convert(mar.protected.dfm, to="stm") # convert dfm to stm format corpus
 
docs  <- Q1.textprocessed$documents
vocab <- Q1.textprocessed$vocab
meta  <- Q1.textprocessed$meta
  
Q1.out <- prepDocuments(docs, vocab, meta)

  
  
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

# cleaning and pre-processing whole text data for a corpus for topic models
MPA.DESG.text.df.clean <-
  MPA.DESG.text.df %>%
  get_sentences() %>%
  mutate(clean.text = tolower(text),                                   # convert all to lower case
         clean.text = str_replace_all(clean.text,"[:punct:]",""),      # remove punctuation
         clean.text = str_replace_all(clean.text,"[:digit:]",""),      # remove numbers
         clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "), # remove all special characters
         clean.text = removeWords(clean.text,stopwords("en"))) %>%     # remove stop words
  mutate(clean.text = text_tokens(.$clean.text, stemmer = "en")) %>%   # stemming words
  unnest(clean.text) %>% # sentences that become NAs aftere cleaning are removed...
  group_by(element_id, sentence_id) %>%
  mutate(clean.text = paste(clean.text, collapse = " ")) %>%
  distinct(element_id, sentence_id, .keep_all=TRUE) %>%
  ungroup() %>%
  mutate(element_id = as.factor(element_id),
         sentence_id = as.factor(sentence_id)) %>%
  mutate(doc_id2 = paste(element_id, sentence_id, sep = "_")) %>%
  select(doc_id2,clean.text,resource.type,url,doc_id,search.term,force) %>%
  rename("text" = "clean.text",
         "CELEX" = "doc_id",
         "doc_id" = "doc_id2") %>%
  as.data.frame()

#possibly add date and theme so we have it in the meta-data of the corpus
MPA.DESG.text.df.clean <-
  smaller.key.df %>%
  select(-force) %>% # already has force info in df
  right_join(. ,MPA.DESG.text.df.clean , by= c("celex" = "CELEX")) %>%
  select(doc_id,text,resource.type,url,search.term,force, celex, date, year)

# lets make it into a corpus object (tm package)
MPA.DESG.corpus <- DataframeSource(MPA.DESG.text.df.clean)
MPA.DESG.corpus <- SimpleCorpus(MPA.DESG.corpus, control = list(language = "en"))

MPA.DESG.corpus <- corpus(MPA.DESG.corpus) #should convert it to quanteda package formate since it is the only type I could get a successful conversion to stm
meta(MPA.DESG.corpus)
docvars(MPA.DESG.corpus)
ndoc(MPA.DESG.corpus)

MPA.DESG.dfm <- dfm(tokens(MPA.DESG.corpus)) # Create a document feature matrix
Q2.textprocessed <- convert(MPA.DESG.dfm, to="stm") # convert dfm to stm format corpus

docs <- Q2.textprocessed$documents
vocab <- Q2.textprocessed$vocab
meta <- Q2.textprocessed$meta

Q2.out <- prepDocuments(docs, vocab, meta)


# Save files ---------------------------------------------------------------------

# Q1 cleaned corpus:
saveRDS(Q1.textprocessed, file = "WP4/Policy_Interactions/data/05_Q1.textprocessed.stm")
saveRDS(Q1.out, file = "WP4/Policy_Interactions/data/05_Q1.preptext.stm")


# Q1 cleaned corpus:
saveRDS(Q2.textprocessed, file = "WP4/Policy_Interactions/data/05_Q2.textprocessed.stm")
saveRDS(Q2.out, file = "WP4/Policy_Interactions/data/05_Q2.preptext.stm")


# archival ---------------------


# cleaning and pre-processing text data for sentiment analysis
#sent.key <- 
#  document.key.df %>%
#  select(celex,date,force,labels,MT) %>% 
#  mutate(date = as.Date(date)) %>%
#  mutate(year = year(date)) %>%
#  distinct()

#mar.protected.text.df.clean.SENT <-
#  mar.protected.text.df %>%
#  mutate(clean.text = tolower(text),                                       # convert all to lower case
#         clean.text = str_replace_all(clean.text,"[:digit:]",""),          # remove numbers
#         clean.text = str_replace_all(clean.text, "[^[:graph:]]"," ")) %>% # remove all special characters
#  group_by(doc_id) %>%
#  distinct(doc_id, .keep_all=TRUE) %>%
#  select(doc_id,clean.text,resource.type,url) %>%
#rename("text" = "clean.text") %>%
#  ungroup() %>%
#  left_join(.,sent.key, by = c("doc_id" = "celex"))

# cleaning and pre-processing text data for a corpus for text mining models
#mar.protected.text.df.clean <-
#  mar.protected.text.df %>%
#  mutate(clean.text = tolower(text),                                   # convert all to lower case
#         clean.text = str_replace_all(clean.text,"[:punct:]",""),      # remove punctuation
#         clean.text = str_replace_all(clean.text,"[:digit:]",""),      # remove numbers
#         clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "), # remove all special characters
#         clean.text = removeWords(clean.text,stopwords("en"))) %>%     # remove stop words
#  mutate(clean.text = text_tokens(.$clean.text, stemmer = "en")) %>%   # stemming words
#  unnest(clean.text) %>%
#  group_by(doc_id) %>%
#  mutate(clean.text = paste(clean.text, collapse = " ")) %>%
 # distinct(doc_id, .keep_all=TRUE) %>%
 # select(doc_id,clean.text,resource.type,url) %>%
#  rename("text" = "clean.text") %>%
 # ungroup() %>%
#  as.data.frame()

#possibly add force, date/year and theme so we have it in the meta-data of the corpus
#mar.protected.text.df.clean <-
# mar.protected.text.df.clean %>%
#  left_join(. , smaller.key.df, by= c("doc_id" = "celex"))

# lets make it into a corpus object
#mar.protected.corpus <- DataframeSource(mar.protected.text.df.clean)
#mar.protected.corpus <- SimpleCorpus(mar.protected.corpus, control = list(language = "en"))

#meta(mar.protected.corpus)

# cleaning and pre-processing text data for sentiment analysis
#MPA.DESG.text.df.SENT <-
 # MPA.DESG.text.df %>%
 # mutate(clean.text = tolower(text),                                       # convert all to lower case
 #        clean.text = str_replace_all(clean.text,"[:digit:]",""),          # remove numbers
 #        clean.text = str_replace_all(clean.text, "[^[:graph:]]"," ")) %>% # remove all special characters
 # group_by(doc_id) %>%
 # distinct(doc_id, .keep_all=TRUE) %>%
 # select(doc_id,clean.text,resource.type,url) %>%
 # rename("text" = "clean.text") %>%
 # ungroup()

# cleaning and pre-processing text data 
#MPA.DESG.text.df.clean <-
#  MPA.DESG.text.df %>%
#  mutate(clean.text = tolower(text),                                   # convert all to lower case
#         clean.text = str_replace_all(clean.text,"[:punct:]",""),      # remove punctuation
#         clean.text = str_replace_all(clean.text,"[:digit:]",""),      # remove numbers
#         clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "), # remove all special characters
#         clean.text = removeWords(clean.text,stopwords("en"))) %>%     # remove stop words
#  mutate(clean.text = text_tokens(.$clean.text, stemmer = "en")) %>%   # stemming words
#  unnest(clean.text) %>%
#  group_by(doc_id) %>%
#  mutate(clean.text = paste(clean.text, collapse = " ")) %>%
#  distinct(doc_id, .keep_all=TRUE) %>%
#  select(doc_id,clean.text,resource.type,url,search.term,force) %>%
#  rename("text" = "clean.text") %>%
#  ungroup() %>%
#  as.data.frame()

#possibly add date and theme so we have it in the meta-data of the corpus
#MPA.DESG.text.df.clean <-
#  smaller.key.df %>%
#  select(-force) %>% # already has force info in df
#  right_join(. ,MPA.DESG.text.df.clean , by= c("celex" = "doc_id")) %>%
#  rename("doc_id" = "celex") %>% 
#  select(doc_id,text,resource.type,url,search.term,force, date, year)


# lets make it into a corpus object
#MPA.DESG.corpus <- DataframeSource(MPA.DESG.text.df.clean)
#MPA.DESG.corpus <- SimpleCorpus(MPA.DESG.corpus, control = list(language = "en"))

#meta(MPA.DESG.corpus)

