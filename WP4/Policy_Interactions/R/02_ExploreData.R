
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("ggplot2")
library("lubridate")
library("tidytext")
library("wordcloud")
library("tidytext")
library("dplyr")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------
mpa.policy.notext.df <- read.csv(file = "WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

# Exploring general things --------------------------------------------------

mpa.policy.notext.df %>%
  summarise(n= n_distinct(CELEX)) 
# total 1,104 policy legislation

mpa.policy.notext.df %>%
  summarise(n= n_distinct(labels)) 
# total 1,302 unique label terms 

mpa.policy.notext.df %>%
  group_by(CELEX) %>%
  summarise(n_labels = (n_distinct(labels))) %>%
  summary(n_labels)
# n_labels     
# Min.   : 1.000  
# 1st Qu.: 5.000  
# Median : 6.000  
# Mean   : 6.253  
# 3rd Qu.: 8.000  
# Max.   :16.000 

# Legislation numbers over times and which are enforced?
mpa.policy.notext.df %>%
  filter(!is.na(force)) %>% 
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  group_by(year,force) %>%
  summarise(n=n_distinct(CELEX)) %>%
  ggplot(aes(fill=force, x = year, y = n)) +
  geom_bar(position="stack", stat="identity")

# In general a clear increase in leg. documents relating to marine protected area*
# also we see that ones that there are more enforced documents from more recent years than the other way around, 
# but this is not suprising...

# # ok lets further investigate those tha are enforced and not:
mpa.policy.notext.df  %>%
  filter(!is.na(force)) %>% 
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  group_by(year,force) %>%
  filter(force == "false") 
# 289 not enfored anymore 

mpa.policy.notext.df  %>%
  filter(!is.na(force)) %>% 
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  group_by(year,force) %>%
  filter(force == "true")
# 515 enforced

# big uptick in legislation starting in 2006. is there a text theme difference between those enforced from 2006 to present?
titled.2006 <- 
  mpa.policy.notext.df %>%
  filter(!is.na(force)) %>% 
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date))  %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  filter(between(year, "2006", "2022"),
         force == "true") %>% 
  mutate(title = map_chr(work,elx_fetch_data,"title")) %>% 
  as_tibble()

titled.2005 <- 
  mpa.policy.notext.df %>%
  filter(!is.na(force)) %>% 
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date))  %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  filter(between(year, "1966", "2005"),
         force == "true") %>% 
  mutate(title = map_chr(work,elx_fetch_data,"title")) %>% 
  as_tibble()

# lets get a wordcloud to get a generalization of what they are possibly about:

## THIS IS PURELY EXPLORATORY... havent got rid of any stopwords etc...
## this code and exploration was inspired from the online tutorial of eurlex
titled.2005 %>% 
  select(CELEX,title) %>% 
  unnest_tokens(word, title) %>% 
  count(CELEX, word, sort = TRUE) %>% 
  filter(!grepl("\\d", word)) %>% 
  bind_tf_idf(word, CELEX, n) %>% 
  with(wordcloud(word, tf_idf, max.words = 50, scale = c(1.8,0.1)))
# this seems to be more about protection of specific things 

titled.2006 %>% 
  select(CELEX,title) %>% 
  unnest_tokens(word, title) %>% 
  count(CELEX, word, sort = TRUE) %>% 
  filter(!grepl("\\d", word)) %>% 
  bind_tf_idf(word, CELEX, n) %>% 
  with(wordcloud(word, tf_idf, max.words = 50, scale = c(1.8,0.1)))
# this seems to be more about foreign policy and programs

# Exploring Eurovoc terms -----------------------------------------------

# Eurovoc Term Co-occurrences: 

mpa.table <- as.data.frame(table(mpa.policy.notext.df$CELEX,mpa.policy.notext.df$labels))

document.term_matrix <-
  mpa.table  %>%
  pivot_wider(
    names_from = Var1,
    values_from = Freq  ) %>%
  as.matrix()

# stuck on term co-occurrence:

mpa.table <- as.data.frame(table(mpa.policy.notext.df$CELEX,mpa.policy.notext.df$labels))

mpa.table <- 
  mpa.table %>%
  rename(CELEX = Var1,
         label = Var2)

# https://cran.r-project.org/web/packages/tidytext/vignettes/tidying_casting.html
#document-term matrix
dtm <-
  mpa.table %>%
  cast_dtm(CELEX, label, Freq)

#term-document matrix
tdm <-
  mpa.table %>%
  cast_tdm(CELEX, label, Freq)

# https://ladal.edu.au/coll.html#3_Visualizing_Collocations 
# convert dtm into sparse matrix
mpadtm <- Matrix::sparseMatrix(i = tdm$i, 
                                   j = tdm$j, 
                                   x = tdm$v, 
                                   dims = c(tdm$nrow, tdm$ncol),
                                   dimnames = dimnames(tdm))

# calculate co-occurrence counts
mpadtm.coocurrences <- t(mpadtm) %*% mpadtm
mpadtm.coocurrences1 <- crossprod(mpadtm)
# convert into matrix
mpa.collocates <- as.matrix(mpadtm.coocurrences1)

diag(mpa.collocates) <- 0

x <- as.data.frame(colSums(mpadtm.coocurrences1))
# no terms with 0 co-occurances
