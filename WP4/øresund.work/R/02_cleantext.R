
# Clear work space -------------------------------------------------------------
rm(list = ls())

# Load libraries ---------------------------------------------------------------

library("readr")
library("dplyr")
library("tidyr")
#library("tibble")
library("tm")
library("corpus")
library("stringr")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------

DKtext.df <- read_csv(file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/01_DK.textDF.csv",
                                 locale = locale(encoding = "ISO-8859-1"),
                                 show_col_types = FALSE)


# Cleaning & Pre-processing the text data --------------------------------------

# lets mak it into the good df format
DKtext.df <- 
  DKtext.df %>%
  as.data.frame(.) %>% 
  select(ID,search.term,country,text) %>%
  mutate(search.term = as.factor(search.term),
         doc_id = as.factor(ID),
         country = as.factor(country)) %>%
  select(doc_id,text,search.term,country)

# cleaning and pre-processing text data 
DKtext.df.clean <-
  DKtext.df %>%
  mutate(clean.text = tolower(text),                                   # convert all to lower case
         clean.text = str_replace_all(clean.text,"[:punct:]",""),      # remove punctuation
         clean.text = str_replace_all(clean.text,"[:digit:]",""),      # remove numbers
         clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "), # remove all special characters
         clean.text = removeWords(clean.text,stopwords("da"))) %>%     # remove danish stop words
  mutate(clean.text = text_tokens(.$clean.text, stemmer = "da")) %>%   # stemming words
  unnest(clean.text) %>%
  group_by(doc_id) %>%
  mutate(clean.text = paste(clean.text, collapse = " ")) %>%
  distinct(doc_id, .keep_all=TRUE) %>%
  select(doc_id,clean.text,search.term,country) %>%
  rename("text" = "clean.text") %>%
  ungroup() %>%
  as.data.frame()

str(DKtext.df.clean)

# lets make it into a corpus object
DK.corpus <- DataframeSource(DKtext.df.clean)
DK.corpus <- SimpleCorpus(DK.corpus, control = list(language = "da"))


# OK so now we have a cleaned corpus: 

# convert corpus to a document-term matrix
# document term matrix: lists word occurances within a document 
dtm <- DocumentTermMatrix(DK.corpus)
inspect(dtm)
#<<DocumentTermMatrix (documents: 20, terms: 3531)>>
#Non-/sparse entries: 9183/61437
#Sparsity           : 87%
#Maximal term length: 66
#Weighting          : term frequency (tf)
#Sample             :
  
# remove sparse terms.. those that occur in only a few documents
inspect(removeSparseTerms(dtm, 0.40))
# so for this terms that have at least a 40 sparse are removed.
# the larger the value the smaller the sparity 
# so sparity = .99, means terms within 1% of the data are kept.
# if sparity = .3, means terms within 70% of the data are kept.


# for our data the sparity is kinda high: 87% of the cells are zero!
# So we should remove terms that have low frequencies: 
# so those terms that are only in 30% of the data lets remove them: 
inspect(removeSparseTerms(dtm, 0.70))

# I assume since we are dealing with gov't documents there is a very distinct writing style,
# thus we should remove terms that appear in almost every document...
inspect(removeSparseTerms(dtm, 0.90))


# convert corpus to a term-document matrix
# document term matrix: lists word occurances within a document 
tdm <- TermDocumentMatrix(DK.corpus)
inspect(tdm)



