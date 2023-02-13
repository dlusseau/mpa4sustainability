
# Get topic prediction within whole network

# Clear work space ---------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 
gc() # get some extra space since what we are doing is space intensive

# Load libraries ----------------------------------------------------------

library("dplyr")
library("tidyr")
library("stringr")
library("tm")
library("corpus")
library("quanteda")
library("stm")
library("tibble")
library("sentimentr")

# Load data ---------------------------------------------------------------

# Query 1
Q1C2.edge.text<- read.csv(file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/05.Q1C2.edge.text.csv")
Q1.net2nd.meta<-read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1secondordercit.verticesmetadata.csv") # vertices meta data
#load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q1_stm.Rdata")

# first order citations to filter later 
Q1.net1st.meta<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1firstordercit.verticesmetadata.csv") # vertices meta data

# Query 2
Q2C2.edge.text<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/05.Q2C2.edge.text.csv")
Q2.net2nd.meta<-read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2secondordercit.verticesmetadata.csv") # vertices meta data
#load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q2_stm.Rdata")

# first order citations to filter later 
Q2.net1st.meta<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2firstordercit.verticesmetadata.csv") # vertices meta data

# Make the new text data and align it with the result stm data ------------

# ---------------- ---------------- Query 1 ---------------- ----------------

# Second order citations ----------------
Q1C2.edge.text.preproc <- 
  Q1C2.edge.text %>%
  left_join(.,Q1.net2nd.meta, by = c("CELEX")) %>%
  select(CELEX, resource.type, force, date, total.text) %>%
  mutate(resource.type = as.factor(resource.type),
         force = as.factor(force),
         doc_id = as.factor(CELEX),
         date = as.Date(date)) %>%
  # cleaning and pre-processing whole text data exactly the sme as the corpus for topic models
  get_sentences() %>%
  mutate(clean.text = tolower(total.text),                                # convert all to lower case
           clean.text = str_replace_all(clean.text,"\\μ[:graph:]+"," "),  # remove units that have this special character
           clean.text = str_replace_all(clean.text,"[:graph:]+\\μ"," "),  # remove units that have this special character
           clean.text = str_replace_all(clean.text,"[:punct:]"," "),      # remove punctuation
           clean.text = str_replace_all(clean.text,"[:digit:]"," "),      # remove numbers
           clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "),  # remove all special characters
           clean.text = stripWhitespace(clean.text),                      # strip extra white space away --> tm package
           clean.text = removeWords(clean.text,stopwords("en"))) %>%      # remove stop words
    mutate(clean.text = text_tokens(.$clean.text, stemmer = "en"))        # stemming words

Q1C2.edge.text.preproc2 <- 
  Q1C2.edge.text.preproc %>%
  unnest(clean.text) %>%          # sentences that become NAs aftere cleaning are removed...
    filter(nchar(clean.text)>2) %>% # remove words that are smaller than 2 characters
    group_by(element_id, sentence_id) %>%
    mutate(clean.text = paste(clean.text, collapse = " ")) %>%
    distinct(element_id, sentence_id, .keep_all=TRUE) %>%
    ungroup() %>%
    mutate(element_id = as.factor(element_id),
          sentence_id = as.factor(sentence_id)) %>%
    mutate(doc_id2 = paste(element_id, sentence_id, sep = "_")) %>%
    select(doc_id2,clean.text,resource.type,date,doc_id) %>%
    rename("text" = "clean.text",
           "CELEX" = "doc_id",
           "doc_id" = "doc_id2") %>%
    as.data.frame()


# lets make it into a corpus object (tm package)
Q1C2.textpreproc.corpus <- DataframeSource(Q1C2.edge.text.preproc2)
Q1C2.textpreproc.corpus <- SimpleCorpus(Q1C2.textpreproc.corpus, control = list(language = "en"))

Q1C2.textpreproc.corpus <- corpus(Q1C2.textpreproc.corpus) # should convert it to quanteda package formate since it is the only type I could get a successful conversion to stm
meta(Q1C2.textpreproc.corpus)
docvars(Q1C2.textpreproc.corpus)
ndoc(Q1C2.textpreproc.corpus)

Q1C2.textpreproc.dfm <- dfm(tokens(Q1C2.textpreproc.corpus))   # Create a document feature matrix
Q1C2.textprocessed <- convert(Q1C2.textpreproc.dfm, to="stm") # convert dfm to stm format corpus

docs  <- Q1C2.textprocessed$documents
vocab <- Q1C2.textprocessed$vocab
meta  <- Q1C2.textprocessed$meta

Q1C2.out <- prepDocuments(docs, vocab, meta)
# Removing 23760 of 53849 terms (23760 of 3978448 tokens) due to frequency 
# Removing 207 Documents with No Words 
# Your corpus now has 256911 documents, 30089 terms and 3954688 tokens.


# First order citations ----------------

# now filter out the C2 text for only the first order citation network --> Q1.net1st.meta
Q1C1.edge.text.preproc2 <- 
  Q1C2.edge.text.preproc2 %>% 
  filter(CELEX %in% Q1.net1st.meta$CELEX )

#check 
n_distinct(Q1C1.edge.text.preproc2$CELEX) # has the right number of documents
# 156

# lets make it into a corpus object (tm package)
Q1C1.textpreproc.corpus <- DataframeSource(Q1C1.edge.text.preproc2)
Q1C1.textpreproc.corpus <- SimpleCorpus(Q1C1.textpreproc.corpus, control = list(language = "en"))

Q1C1.textpreproc.corpus <- corpus(Q1C1.textpreproc.corpus) # should convert it to quanteda package formate since it is the only type I could get a successful conversion to stm
meta(Q1C1.textpreproc.corpus)
docvars(Q1C1.textpreproc.corpus)
ndoc(Q1C1.textpreproc.corpus)

Q1C1.textpreproc.dfm <- dfm(tokens(Q1C1.textpreproc.corpus))   # Create a document feature matrix
Q1C1.textprocessed <- convert(Q1C1.textpreproc.dfm, to="stm") # convert dfm to stm format corpus

docs  <- Q1C1.textprocessed$documents
vocab <- Q1C1.textprocessed$vocab
meta  <- Q1C1.textprocessed$meta

Q1C1.out <- prepDocuments(docs, vocab, meta)
# Removing 6702 of 15913 terms (6702 of 1005600 tokens) due to frequency 
# Removing 99 Documents with No Words 
# Your corpus now has 58704 documents, 9211 terms and 998898 tokens

# ---------------- ---------------- Query 2 ---------------- ----------------

# Second order citations ----------------
Q2C2.edge.text.preproc <- 
  Q2C2.edge.text %>%
  left_join(.,Q2.net2nd.meta, by = c("CELEX")) %>%
  select(CELEX, resource.type, force, date, total.text) %>%
  mutate(resource.type = as.factor(resource.type),
         force = as.factor(force),
         doc_id = as.factor(CELEX),
         date = as.Date(date)) %>%
  # cleaning and pre-processing whole text data exactly the sme as the corpus for topic models
  get_sentences() %>%
  mutate(clean.text = tolower(total.text),                                # convert all to lower case
         clean.text = str_replace_all(clean.text,"\\μ[:graph:]+"," "),  # remove units that have this special character
         clean.text = str_replace_all(clean.text,"[:graph:]+\\μ"," "),  # remove units that have this special character
         clean.text = str_replace_all(clean.text,"[:punct:]"," "),      # remove punctuation
         clean.text = str_replace_all(clean.text,"[:digit:]"," "),      # remove numbers
         clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "),  # remove all special characters
         clean.text = stripWhitespace(clean.text),                      # strip extra white space away --> tm package
         clean.text = removeWords(clean.text,stopwords("en"))) %>%      # remove stop words
  mutate(clean.text = text_tokens(.$clean.text, stemmer = "en")) %>%   # stemming words
  unnest(clean.text) %>%          # sentences that become NAs aftere cleaning are removed...
  filter(nchar(clean.text)>2) %>% # remove words that are smaller than 2 characters
  group_by(element_id, sentence_id) %>%
  mutate(clean.text = paste(clean.text, collapse = " ")) %>%
  distinct(element_id, sentence_id, .keep_all=TRUE) %>%
  ungroup() %>%
  mutate(element_id = as.factor(element_id),
         sentence_id = as.factor(sentence_id)) %>%
  mutate(doc_id2 = paste(element_id, sentence_id, sep = "_")) %>%
  select(doc_id2,clean.text,resource.type,date,doc_id) %>%
  rename("text" = "clean.text",
         "CELEX" = "doc_id",
         "doc_id" = "doc_id2") %>%
  as.data.frame()

# lets make it into a corpus object (tm package)
Q2C2.textpreproc.corpus <- DataframeSource(Q2C2.edge.text.preproc)
Q2C2.textpreproc.corpus <- SimpleCorpus(Q2C2.textpreproc.corpus, control = list(language = "en"))

Q2C2.textpreproc.corpus <- corpus(Q2C2.textpreproc.corpus) # should convert it to quanteda package formate since it is the only type I could get a successful conversion to stm
meta(Q2C2.textpreproc.corpus)
docvars(Q2C2.textpreproc.corpus)
ndoc(Q2C2.textpreproc.corpus)

Q2C2.textpreproc.dfm <- dfm(tokens(Q2C2.textpreproc.corpus))   # Create a document feature matrix
Q2C2.textprocessed <- convert(Q2C2.textpreproc.dfm, to="stm") # convert dfm to stm format corpus

docs  <- Q2C2.textprocessed$documents
vocab <- Q2C2.textprocessed$vocab
meta  <- Q2C2.textprocessed$meta

Q2C2.out <- prepDocuments(docs, vocab, meta)
# Removing 34738 of 75124 terms (34738 of 5226756 tokens) due to frequency 
# Removing 357 Documents with No Words 
# Your corpus now has 337123 documents, 40386 terms and 5192018 tokens.

# First order citations ----------------

# now filter out the C2 text for only the first order citation network
Q2C1.edge.text.preproc <- 
  Q2C2.edge.text.preproc %>% 
  filter(CELEX %in% Q2.net1st.meta$CELEX )

#check 
n_distinct(Q2C1.edge.text.preproc$CELEX) # has the right number of documents
# 329

# lets make it into a corpus object (tm package)
Q2C1.textpreproc.corpus <- DataframeSource(Q2C1.edge.text.preproc)
Q2C1.textpreproc.corpus <- SimpleCorpus(Q2C1.textpreproc.corpus, control = list(language = "en"))

Q2C1.textpreproc.corpus <- corpus(Q2C1.textpreproc.corpus) # should convert it to quanteda package formate since it is the only type I could get a successful conversion to stm
meta(Q2C1.textpreproc.corpus)
docvars(Q2C1.textpreproc.corpus)
ndoc(Q2C1.textpreproc.corpus)

Q2C1.textpreproc.dfm <- dfm(tokens(Q2C1.textpreproc.corpus))   # Create a document feature matrix
Q2C1.textprocessed <- convert(Q2C1.textpreproc.dfm, to="stm")  # Convert dfm to stm format corpus

docs  <- Q2C1.textprocessed$documents
vocab <- Q2C1.textprocessed$vocab
meta  <- Q2C1.textprocessed$meta

Q2C1.out <- prepDocuments(docs, vocab, meta)
# Removing 19651 of 38642 terms (19651 of 1799782 tokens) due to frequency 
# Removing 432 Documents with No Words 
# Your corpus now has 111160 documents, 18991 terms and 1780131 tokens.

# Save -------------------------------------------------------------------------------------

write.csv(Q1C2.edge.text.preproc2, file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06.Q1C2.edge.text.cleantext.csv")
write.csv(Q2C2.edge.text.preproc, file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06.Q2C2.edge.text.cleantext.csv")

saveRDS(Q1C2.out, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q2C2.preptext.rds")
saveRDS(Q2C2.out, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q1C2.preptext.rds")
saveRDS(Q1C1.out, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q2C1.preptext.rds")
saveRDS(Q2C1.out, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q1C1.preptext.rds")


