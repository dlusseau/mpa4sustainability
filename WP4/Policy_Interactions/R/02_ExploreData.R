
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
library("igraph")
library("widyr")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------

mpa.policy.notext.df <- read.csv(file = "WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

key.df <- read.csv(file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

# Exploring document info over time --------------------------------------------------

mpa.policy.notext.df %>%
  summarise(n= n_distinct(CELEX)) 
# total 25 policy legislation
unique(mpa.policy.notext.df$CELEX)

mpa.policy.notext.df %>%
  summarise(n= n_distinct(labels)) 
# total 104 unique label terms 

mpa.policy.notext.df %>%
  group_by(CELEX) %>%
  summarise(n_labels = (n_distinct(labels))) %>%
  summary(n_labels)
# n_labels     
# Min.   : 1.00  
# 1st Qu.: 7.00  
# Median : 9.00  
# Mean   : 8.12  
# 3rd Qu.:10.00  
# Max.   :11.00 

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

# Earliest document is 1984 which uses "marine protected area" the title is:
# 84/132/EEC: Council Decision of 1 March 1984 on the conclusion of the Protocol concerning Mediterranean specially protected areas

# Legislation numbers over times by resource/leg. type:
mpa.policy.notext.df %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  group_by(year,resource.type) %>%
  summarise(n=n_distinct(CELEX)) %>%
  ggplot(aes(fill=resource.type, x = year, y = n)) +
  geom_bar(position="stack", stat="identity") +
  ylab("Number of legislations") + 
  xlab("Year")+
  theme_minimal()+ 
  theme(legend.position = "bottom")+
  scale_color_discrete(name = "Type of legislation")

# 1 document does not have a date associated??
mpa.policy.notext.df %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  filter(is.na(year)) 
#  resource.type      CELEX                                                     url work type date force eurovoc citationcelex labels year
#           REG 32021R0092 http://publications.europa.eu/resource/celex/32021R0092 <NA> <NA> <NA>  <NA>    <NA>          <NA>   <NA>   NA
# ok online it says "This act has been changed. Current consolidated version CELEX: 02021R0092-20220416"
# so maybe lets pull this one from the large key and replace it... key doesnt have it....
# based on its new CELEX code it is not a type of legislation... 0: Consolidated acts so remove...

mpa.policy.notext.df %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  filter(is.na(force)) %>%
  group_by(resource.type,force) %>%
  summarise(n=n_distinct(CELEX))

#resource.type force     n
#OPIN          NA        8
#RECO          NA        1
#REG           NA        1 --> this is the one with the prev. problem so makes sense *Reg cannot be NA tho bc they are binding.
# "An "opinion" is an instrument that allows the institutions 
#                             to make a statement in a non-binding fashion, in other words without 
#                             imposing any legal obligation on those to whom it is addressed.
#                             An opinion is not binding."
# RECO     is non-binding

# Exploring document type and text -------------------------------------------

# Does the text within the document vary in themes depending on the type of legislation? 
titled.DEC <- 
  mpa.policy.notext.df %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  filter(resource.type=="DEC") %>% 
  mutate(title = map_chr(work,elx_fetch_data,"title")) %>% 
  as_tibble()

titled.DIR <- 
  mpa.policy.notext.df %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  filter(resource.type=="DIR") %>% 
  mutate(title = map_chr(work,elx_fetch_data,"title")) %>% 
  as_tibble()

titled.OPIN <- 
  mpa.policy.notext.df %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  filter(resource.type=="OPIN") %>% 
  mutate(title = map_chr(work,elx_fetch_data,"title")) %>% 
  as_tibble()

titled.RECO <- 
  mpa.policy.notext.df %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  filter(resource.type=="RECO") %>% 
  mutate(title = map_chr(work,elx_fetch_data,"title")) %>% 
  as_tibble()

titled.REG <- 
  mpa.policy.notext.df %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  filter(resource.type=="REG") %>% 
  filter(!is.na(work)) %>% #removing the problem document
  mutate(title = map_chr(work,elx_fetch_data,"title")) %>% 
  as_tibble()

# lets get a word cloud to get a generalization of what they are possibly about:

## THIS IS PURELY EXPLORATORY... haven't got rid of any stopwords etc...
## this code and exploration was inspired from the online tutorial of eurlex
titled.DEC %>% 
  select(CELEX,title) %>% 
  unnest_tokens(word, title) %>% 
  count(CELEX, word, sort = TRUE) %>% 
  filter(!grepl("\\d", word)) %>% 
  bind_tf_idf(word, CELEX, n) %>% 
  with(wordcloud(word, tf_idf, max.words = 50, scale = c(1.8,0.1)))

titled.DIR %>% 
  select(CELEX,title) %>% 
  unnest_tokens(word, title) %>% 
  count(CELEX, word, sort = TRUE) %>% 
  filter(!grepl("\\d", word)) %>% 
  bind_tf_idf(word, CELEX, n) %>% 
  with(wordcloud(word, tf_idf, max.words = 50, scale = c(1.8,0.1)))

titled.OPIN %>% 
  select(CELEX,title) %>% 
  unnest_tokens(word, title) %>% 
  count(CELEX, word, sort = TRUE) %>% 
  filter(!grepl("\\d", word)) %>% 
  bind_tf_idf(word, CELEX, n) %>% 
  with(wordcloud(word, tf_idf, max.words = 50, scale = c(1.8,0.1)))

#cant do it because there is only one...
titled.RECO %>% 
  select(CELEX,title) %>% 
  unnest_tokens(word, title) %>% 
  count(CELEX, word, sort = TRUE) %>% 
  filter(!grepl("\\d", word)) %>% 
  bind_tf_idf(word, CELEX, n) %>% 
  with(wordcloud(word, tf_idf, max.words = 50, scale = c(1.8,0.1)))

titled.REG %>% 
  select(CELEX,title) %>% 
  unnest_tokens(word, title) %>% 
  count(CELEX, word, sort = TRUE) %>% 
  filter(!grepl("\\d", word)) %>% 
  bind_tf_idf(word, CELEX, n) %>% 
  with(wordcloud(word, tf_idf, max.words = 50, scale = c(1.8,0.1)))


# Exploring Eurovoc terms -----------------------------------------------

# Eurovoc Term Co-occurrences: 
mpa.policy.notext.df.cleaned <-
  mpa.policy.notext.df %>%
  distinct(CELEX,labels, .keep_all = TRUE)
  
mpa.table <- as.data.frame(table(mpa.policy.notext.df.cleaned$CELEX,mpa.policy.notext.df.cleaned$labels))

mpa.table <- 
  mpa.table %>%
  rename(CELEX = Var1,
         label = Var2)

head(mpa.policy.notext.df.cleaned)

label.pairs <- 
  mpa.policy.notext.df.cleaned %>%
  pairwise_count(labels,CELEX, sort=TRUE)

#david\s code help
label.pairs$all<-apply(apply(cbind(as.character(label.pairs$item1),as.character(label.pairs$item2)),1,sort),2,function(x) paste(x,collapse="."))
#this should be the four columns in alphabetical order collapsed and separated by a dot

#duplicated should work on this
label.pairs.sub<-label.pairs[!duplicated(label.pairs$all),]

ggplot(label.pairs.sub, aes(item1, item2, fill= n)) + 
  geom_tile() +
  scale_fill_gradient(low="grey", high="red") +
  ylab("")+
  xlab("")+
  theme_bw()

#maybe a network plot is a better visualization for this data:
term.pairs_matrix <-
  label.pairs.sub  %>%
  select(-all) %>%
  pivot_wider(
    names_from = item1,
    values_from = n)%>%
  column_to_rownames(.,var = "item2")

term.pairs_matrix[is.na(term.pairs_matrix)] <- 0
term.pairs_matrix <- as.matrix(term.pairs_matrix)

xx<- 
  label.pairs.sub %>%
  select(-all)

n <-label.pairs.sub$n*1.15
n1 <-label.pairs.sub$n*.45

network <- graph_from_data_frame(xx, directed = FALSE)

l=layout_nicely(network) 

plot(network,
     layout=l,
     edge.width=n,
     edge.color="grey",
     vertex.size=.5,
     vertex.label.cex=n1,
     vertex.shape="none")
# the layout needs to be fixed but the jist is there....

# archival code --------------------------------
# maybe need these data objects for a specific analysis function in the future: 
# https://cran.r-project.org/web/packages/tidytext/vignettes/tidying_casting.html

#document-term matrix
dtm <-
  mpa.table %>%
  cast_dtm(CELEX, label, Freq)

#term-document matrix
tdm <-
  mpa.table %>%
  cast_tdm(label, CELEX, Freq)

sparse.matrix <- 
  mpa.table %>%
  cast_sparse(.,CELEX, label, Freq)

