
# Clear work space -------------------------------------------------------------
rm(list = ls())

# Load libraries ---------------------------------------------------------------

library("stm")
library("quanteda")
library("readr")
library("dplyr")
library("tidyr")
library("tibble")
library("tm")
library("corpus")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------
mpa.policy.notext.df <- read_csv(file = "WP4/øresund.work/data/01_DK.textDF.csv",
                                 locale = locale(encoding = "ISO-8859-1"),
                                 show_col_types = FALSE)

mpa.policy.notext.df <- 
  as.data.frame(mpa.policy.notext.df) %>% 
  select(ID,search.term,country,,text) %>%
  mutate(search.term = as.factor(search.term),
         doc_id = as.factor(ID),
         country = as.factor(country)) 

str(mpa.policy.notext.df)

# convert to a corpus object: 
x <- corpus(mpa.policy.notext.df,
            text_field = "text",
            meta = list(data.frame(mpa.policy.notext.df[1:2]))
            )

docvars(x) # looks correct
ndoc(x)    # looks correct
str(x)
summary(x) # overview of our corpus


xx <- as_corpus_text(mpa.policy.notext.df$text,
                      names = mpa.policy.notext.df$ID)

mpa.policy.notext.df <- 
  as.data.frame(mpa.policy.notext.df) %>% 
  select(ID,text,search.term,country,text) %>%
  rename("doc_id" = "ID")

please <- DataframeSource(mpa.policy.notext.df)
x <- Corpus(please)
inspect(x)
hi <- meta(x)
# Cleaning & Pre-processing the text data --------------------------------------

docs <- data.frame(doc_id = c("doc_1", "doc_2"),
                   text = c("This is a text.", "This another one."),
                   dmeta1 = 1:2, dmeta2 = letters[1:2],
                   stringsAsFactors = FALSE)
(ds <- DataframeSource(docs))
x <- Corpus(ds)
inspect(x)
meta(x)
# Archival for now -------------------------------------------------------------

# stm package can pre-process Danish and Swedish language bc using tm package :)

da.processed <- textProcessor(documents=mpa.policy.notext.df$text,
                             metadata=mpa.policy.notext.df,
                             language = "da")

vocab <- da.processed$vocab

converted <- convertCorpus(da.processed$documents, 
                           da.processed$vocab, 
                           type = c("Matrix"))
