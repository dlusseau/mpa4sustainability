
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
Q1C2.edge.text<- read.csv(file =  "WP4/Policy_Interactions/data/08.Q1C2.edge.text.csv")
Q1.net2nd.meta<-read.csv(file = "WP4/Policy_Interactions/data/03.Q1secondordercit.verticesmetadata.csv") # vertices meta data
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/05.5_Q1_stm.Rdata")
Q1.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/05_Q1.preptext.stm")

#Query 2
Q2C2.edge.text<- read.csv(file = "WP4/Policy_Interactions/data/08.Q2C2.edge.text.csv")
Q2.net2nd.meta<-read.csv(file = "WP4/Policy_Interactions/data/03.Q2secondordercit.verticesmetadata.csv") # vertices meta data
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/05.5_Q2_stm.Rdata")
Q2.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/05_Q2.preptext.stm")

# Make the new text data and align it with the result stm data ------------

# Query 1 ----------------

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
  mutate(clean.text = tolower(total.text),                             # convert all to lower case
           clean.text = str_replace_all(clean.text,"\\μ[:graph:]+",""),  # remove units that have this special character
           clean.text = str_replace_all(clean.text,"[:graph:]+\\μ",""),  # remove units that have this special character
           clean.text = str_replace_all(clean.text,"[:punct:]",""),      # remove punctuation
           clean.text = str_replace_all(clean.text,"[:digit:]",""),      # remove numbers
           clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "), # remove all special characters
           clean.text = removeWords(clean.text,stopwords("en")),         # remove stop words
           clean.text = stripWhitespace(clean.text)) %>%                 # strip extra whote space away --> tm package
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
Q1C2.textpreproc.corpus <- DataframeSource(Q1C2.edge.text.preproc)
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

# **Note from the stm CRAN manual!
# "we don't run prepCorpus here because we don't want to drop any words- we want every word that showed up in the old documents."

Q1.newdocs <- alignCorpus(new=Q1C2.textprocessed, old.vocab=Q1.stm$vocab)
# info on what was done: 
# Removing 14628 Documents with No Words (in our case sentences)
# Your new corpus now has 192222 documents (sentences), 3968 non-zero terms of 3980 total terms in the original set. 
# 58779 terms from the new data did not match.
# This means the new data contained 99.7% of the old terms
# and the old data contained 6.3% of the unique terms in the new data. 
# You have retained 3499112 tokens of the 3956192 tokens you started with (88.4%)


Q1C2.topi.pred <- 
  fitNewDocuments(model=Q1.stm, 
                  documents=Q1.newdocs$documents, 
                  newData=Q1.newdocs$meta,
                  origData=Q1.text$meta)

Q1C2.doctopic.pred <- Q1C2.topi.pred$theta

Q1.doc.names <- as.data.frame(names(Q1.newdocs$documents))

# lets make this into a long df
Q1C2.Doctopic.longdf <-
  Q1C2.doctopic.pred %>%
  as.data.frame() %>%
  rownames_to_column(var = "document_sentence") %>%
  cbind(., Q1.newdocs$meta$CELEX,Q1.doc.names) %>%
pivot_longer(.,
             cols = 2:68,
             names_to = "topic", 
             values_to = "proportion") %>%
  mutate(topic = str_replace_all(topic, "V", "topic"),
         percent.doc = proportion *100) 

n_distinct(Q1C2.Doctopic.longdf$`Q1.newdocs$meta$CELEX`)
# 583

summary(Q1C2.Doctopic.longdf$percent.doc)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1708  0.5688  1.4925  1.4362 99.4407 

# Query 2 ----------------

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
  mutate(clean.text = tolower(total.text),                             # convert all to lower case
         clean.text = str_replace_all(clean.text,"\\μ[:graph:]+",""),  # remove units that have this special character
         clean.text = str_replace_all(clean.text,"[:graph:]+\\μ",""),  # remove units that have this special character
         clean.text = str_replace_all(clean.text,"[:punct:]",""),      # remove punctuation
         clean.text = str_replace_all(clean.text,"[:digit:]",""),      # remove numbers
         clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "), # remove all special characters
         clean.text = removeWords(clean.text,stopwords("en")),         # remove stop words
         clean.text = stripWhitespace(clean.text)) %>%                 # strip extra whote space away --> tm package
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
  select(doc_id2,clean.text,resource.type,url,doc_id) %>%
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

# **Note from the stm CRAN manual!
# "we don't run prepCorpus here because we don't want to drop any words- we want every word that showed up in the old documents."

Q2.newdocs <- alignCorpus(new=Q2C2.textprocessed, old.vocab=Q1.stm$vocab)

Q2C2.topi.pred <- 
  fitNewDocuments(model=Q2.stm, 
                  documents=Q2.newdocs$documents, 
                  newData=Q2.newdocs$meta,
                  origData=Q2.stm$meta)

Q2C2.doctopic.pred <- Q2C2.topi.pred$theta

Q2.doc.names <- as.data.frame(names(Q2.newdocs$documents))

# lets make this into a long df
Q2C2.Doctopic.longdf <-
  Q2C2.doctopic.pred %>%
  as.data.frame() %>%
  rownames_to_column(var = "document_sentence") %>%
  cbind(., Q2.newdocs$meta$CELEX,Q2.doc.names) %>%
  pivot_longer(.,
               cols = 2:68,
               names_to = "topic", 
               values_to = "proportion") %>%
  mutate(topic = str_replace_all(topic, "V", "topic"),
         percent.doc = proportion *100)


n_distinct(Q2C2.Doctopic.longdf$`Q2.newdocs$meta$CELEX`)


# Save -------------------------------------------------------------------------------------

write.csv(Q1C2.Doctopic.longdf, file = "WP4/Policy_Interactions/data/09.Q1C2.Doctopic.longdf.csv", row.names=FALSE)
write.csv(Q2C2.Doctopic.longdf, file = "WP4/Policy_Interactions/data/09.Q2C2.Doctopic.longdf.csv", row.names=FALSE)

saveRDS(Q2.newdocs, file = "WP4/Policy_Interactions/data/09_Q2.newdocs.rds")
saveRDS(Q1.newdocs, file = "WP4/Policy_Interactions/data/09_Q1.newdocs.rds")



