
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("ggplot2")
library("lubridate")
library("tidytext")
library("wordcloud")
library("tidytext")
library("dplyr")
library("patchwork")

# Define functions --------------------------------------------------------
#testing 
# Load data ---------------------------------------------------------------
mpa.policy.notext.df <- read.csv(file = "WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

key.df <- read.csv(file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")


# Exploring general things --------------------------------------------------

mpa.policy.notext.df %>%
  summarise(n= n_distinct(CELEX)) 
# total 1,094 policy legislation

mpa.policy.notext.df %>%
  summarise(n= n_distinct(labels)) 
# total 1,299 unique label terms 

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
  filter(!is.na(force)) %>% #filtering out leg that is deemed N.A
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  group_by(year,force) %>%
  summarise(n=n_distinct(CELEX)) %>%
  ggplot(aes(fill=force, x = year, y = n)) +
  geom_bar(position="stack", stat="identity") +
  ylab("Number of legislations") + 
  xlab("Year")+
  theme_minimal()+ 
  theme(legend.position = "bottom")+
  scale_fill_discrete(name = "Legislation enforced", labels = c("No", "Yes"))

# In general a clear increase in leg. documents relating to marine protected area*
# also we see that ones that there are more enforced documents from more recent years than the other way around, 
# but this is not suprising...


# Legislation numbers over times by resource/leg. type?
mpa.policy.notext.df %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  group_by(year,resource.type) %>%
  summarise(n=n_distinct(CELEX)) %>%
  ggplot(aes(color=resource.type, x = year, y = n)) +
  geom_line()  +
  ylab("Number of legislations") + 
  xlab("Year")+
  theme_minimal()+ 
  theme(legend.position = "bottom")+
  scale_color_discrete(name = "Type of legislation")

# 4 documents do not have a date associated??
# looks like the
x <- 
  mpa.policy.notext.df %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  filter(is.na(year)) 

unique(x$CELEX)
# 20 of them don't have any data associated from the SPARQL key... 
# this means the online term pull got data but not the eurlex packages...

mpa.policy.notext.df %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  filter(is.na(force)) %>%
  group_by(resource.type,force) %>%
  summarise(n=n_distinct(CELEX))

#resource.type force     n
# DEC           NA        4
# DIR           NA        3
# OPIN          NA      277  "An "opinion" is an instrument that allows the institutions 
#                             to make a statement in a non-binding fashion, in other words without 
#                             imposing any legal obligation on those to whom it is addressed.
#                             An opinion is not binding."
# RECO          NA       11   is non-binding, but some can be enforced or not? see below...
# REG           NA       14

mpa.policy.notext.df %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  filter(!is.na(force)) %>%
  group_by(resource.type,force) %>%
  summarise(n=n_distinct(CELEX))

#   resource.type force     n
#   DEC           false    39
#   DEC           true    202
#   DIR           false    56
#   DIR           true     59
#   RECO          false     4
#   RECO          true      7
#   REG           false   181
#   REG           true    237

# O.K lets further investigate those that are enforced and not:
mpa.policy.notext.df  %>%
  filter(!is.na(force)) %>% 
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  group_by(year,force) %>%
  filter(force == "false") 
# 280 not enforced anymore 

mpa.policy.notext.df  %>%
  filter(!is.na(force)) %>% 
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  group_by(year,force) %>%
  filter(force == "true")
# 505 enforced

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

# lets get a word cloud to get a generalization of what they are possibly about:

## THIS IS PURELY EXPLORATORY... haven't got rid of any stopwords etc...
## this code and exploration was inspired from the online tutorial of eurlex
titled.2005 %>% 
  select(CELEX,title) %>% 
  unnest_tokens(word, title) %>% 
  count(CELEX, word, sort = TRUE) %>% 
  filter(!grepl("\\d", word)) %>% 
  bind_tf_idf(word, CELEX, n) %>% 
  with(wordcloud(word, tf_idf, max.words = 50, scale = c(1.8,0.1)))

# this seems to be more about protection of specific things 
# f.x. documentation, protected, species, scheme, regulation, etc...

titled.2006 %>% 
  select(CELEX,title) %>% 
  unnest_tokens(word, title) %>% 
  count(CELEX, word, sort = TRUE) %>% 
  filter(!grepl("\\d", word)) %>% 
  bind_tf_idf(word, CELEX, n) %>% 
  with(wordcloud(word, tf_idf, max.words = 50, scale = c(1.8,0.1)))

# this seems to be more about foreign policy and programs
# f.x. documentation, tariff, initative, fund, CFSP (common foreign security policy)

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
