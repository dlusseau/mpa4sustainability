
# Clear work space -------------------------------------------------------------
rm(list = ls())

# Load libraries ---------------------------------------------------------------

library("readr")
library("tidyr")
library("tibble")
library("tm")
library("corpus")
library("stringr")
library("dplyr")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------

setwd("C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/raw_data/DK_policy")

retsinformation.file.list <- list.files(pattern='*.csv')

retsinformation.df <- read_delim(retsinformation.file.list, 
                                 id = "search.term",
                                 delim = ";",
                                 locale = locale(encoding="ISO-8859-1"))

retsinformation.df <-
  retsinformation.df %>%
  mutate(search.term = str_extract_all(search.term,"\\w+\\."),
         search.term = str_replace_all(search.term,"[:punct:]+",""))


DK.text <- readRDS(file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/DK.text.list.1")

#  lets make it into a df to use -----------------------------------------------

DK.text.df <- as.data.frame(cbind(DK.text))

DK.text.df2 <- 
  DK.text.df %>% 
  rownames_to_column(., var = "url") %>%
  rename("text" = "DK.text") %>%
  left_join(.,retsinformation.df, by = c("url" = "EliUrl")) %>%
  mutate(country = "DK") %>%
  as_tibble() %>%
  unnest(text,keep_empty = TRUE)

str(DK.text.df2)
# ok so this df has 1363 obs. but the original has 1367, this is because the two problem URLS
# they also have dup rows since they apprear in both the fiskeri and jagt search queries so 1363 obs. is correct since 1367-4=1364

#looking for the other forms of hunting and fishing (fulglejagt, sæljagt, harpunfiskeri)

DK.text.df3 <-
  DK.text.df2 %>%
    mutate(harpun = case_when(search.term == "fiskeri" ~ str_detect(text, "harpun")),
           sæl    = case_when(search.term == "jagt" ~ str_detect(text, "sæl")),
           fugle = case_when(search.term == "jagt" ~ str_detect(text, "fugle")))

DK.text.df3 %>%
  group_by(search.term) %>%
  summarise(n=n_distinct(url))

# fiskeri      1010
# jagt          345
# sotrafik        8
    
DK.text.df3 %>%
  filter(search.term == "fiskeri" & harpun == "TRUE") # 3 out of 1010 fisheries documents mention harpun

DK.text.df3 %>%
  filter(search.term == "jagt" & sæl == "TRUE") # 138 out of 345 hunting documents mention seal

DK.text.df3 %>%
  filter(search.term == "jagt" & fugle == "TRUE") # 164 out of 345 hunting documents mention bird

DK.text.df3 %>%
  filter(search.term == "jagt" & fugle == "TRUE" & sæl == "TRUE") # 35 out of 345 hunting documents both mention bird and seal


# Cleaning & Pre-processing the text data --------------------------------------

# cleaning and pre-processing text data 
DKtext.df.clean <-
  DK.text.df3 %>%
  mutate(clean.text = tolower(text),                                  # convert all to lower case
         clean.text = str_replace_all(clean.text,"[:punct:]",""),     # remove punctuation
         clean.text = str_replace_all(clean.text,"[:digit:]",""),     # remove numbers
         clean.text = str_replace_all(clean.text, "[^[:alnum:]]"," "),# remove all special characters
         clean.text = removeWords(clean.text,stopwords("da"))) %>%    # remove danish stop words
  mutate(clean.text = text_tokens(.$clean.text, stemmer = "da")) %>%  # stemming words
  unnest(clean.text) %>%
  group_by(url,search.term) %>%
  mutate(clean.text = paste(clean.text, collapse = " ")) %>%
  distinct(doc_id, .keep_all=TRUE) %>%
  select(doc_id,clean.text,search.term,country) %>%
  rename("text" = "clean.text") %>%
  ungroup() %>%
  as.data.frame()

str(DKtext.df.clean)

# lets mak it into the good df format
DKtext.df <- 
  DKtext.df %>%
  as.data.frame(.) %>% 
  select(ID,search.term,country,text) %>%
  mutate(search.term = as.factor(search.term),
         doc_id = as.factor(ID),
         country = as.factor(country)) %>%
  select(doc_id,text,search.term,country)


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



