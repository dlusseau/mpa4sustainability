
# extracting the citation network text to later get topic prediction within 

# Clear work space ---------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 
gc()
# Load libraries ----------------------------------------------------------

library("dplyr")
library("eurlex")
library("tibble")
library("purrr")
library("igraph")

# Load data ---------------------------------------------------------------

# ------------ Query 1 (Q1) --------- #

# -- citations -- #
Q1.net2nd<-read.csv("WP4/Policy_Interactions/data/03.Q1secondordercit.edgelist.csv")              # edge list
Q1.net2nd.meta<-read.csv("WP4/Policy_Interactions/data/03.Q1secondordercit.verticesmetadata.csv") # vertices meta data
Q1.net2nd.graph<-readRDS("WP4/Policy_Interactions/data/03.Q1secondordercit.network.rds")          # the network object 

# ----------- Query 2 (Q2) ---------- #

# -- citations -- #
Q2.net2nd<-read.csv("WP4/Policy_Interactions/data/03.Q2secondordercit.edgelist.csv")              # edge list
Q2.net2nd.meta<-read.csv("WP4/Policy_Interactions/data/03.Q2secondordercit.verticesmetadata.csv") # vertices meta data
Q2.net2nd.graph<-readRDS("WP4/Policy_Interactions/data/03.Q2secondordercit.network.rds")          # the network object 

# EU mpa designation term associated text data: 
MPA.DESG.text <- read.csv(file = "WP4/Policy_Interactions/data/01_mpaterms.text.data.dup.csv")

# Get text for all of the network vertices --------------------------------

# Will only do this for the second order citation network since 
# it can then just be filtered later to only include first order ciation network

# Query one ---------------------

# Citation text --------

# check which "to" documents we already have text from (using Q2 text data since it contains all for Q1 pulls and more)
MPA.DESG.text.cut <-
  MPA.DESG.text %>%
  select(CELEX,total.text) %>%
  distinct(., .keep_all = TRUE) # since some of the search terms were duplicated rows since text mentions both

# to of the edge df
toQ1.textdf <- 
  Q1.net2nd %>%
  select(to) %>%
  filter(to %in% MPA.DESG.text.cut$CELEX) %>%
  left_join(.,MPA.DESG.text.cut, by = c("to"="CELEX")) %>%
  distinct(., .keep_all = TRUE) %>% # since some cite multiple documents. 
  rename("CELEX" = "to")
# 24
toQ1.NOtextdf <- 
  Q1.net2nd %>%
  select(to) %>%
  filter(! to %in% MPA.DESG.text.cut$CELEX) %>%
  distinct(., .keep_all = TRUE) %>% # since some cite multiple documents. 
  rename("CELEX" = "to")
# 546
# 24 + 546 = 570

Q1.net2nd %>%
  select(to) %>%
  distinct(., .keep_all = TRUE)  
# math checks out there is 570 citations celex

# Citer text ----------

# "from" column of the edge df 

fromQ1.textdf <- 
  Q1.net2nd %>%
  select(from) %>%
  filter(from %in% MPA.DESG.text.cut$CELEX) %>%
  left_join(.,MPA.DESG.text.cut, by = c("from"="CELEX")) %>%
  distinct(., .keep_all = TRUE) %>% # since some cite multiple documents. 
  rename("CELEX" = "from")
# 28

fromQ1.NOtextdf <- 
  Q1.net2nd %>%
  select(from) %>%
  filter(! from %in% MPA.DESG.text.cut$CELEX) %>%
  distinct(., .keep_all = TRUE) %>% # since some cite multiple documents. 
  rename("CELEX" = "from")
# 90

# 28 + 90 = 118

Q1.net2nd %>%
  select(from) %>%
  distinct(., .keep_all = TRUE) 
# math checks out there is 118 citers (celex)

# join the to and from columns and remove duplicates
Q1edge.text <-
  rbind(fromQ1.textdf, toQ1.textdf) %>%
  distinct(., .keep_all = TRUE) # 37 celex text

gc() # clear up some space before the text pulling

Q1edge.NOtextsec1 <- 
  rbind(fromQ1.NOtextdf, toQ1.NOtextdf) %>%
  distinct(., .keep_all = TRUE) %>% # 546 celex
  slice(0:273) %>% # 273
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX)) %>% 
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  mutate(text = map_chr(url, elx_fetch_data, "text")) %>% 
  as_tibble() %>%
  mutate(total.text = paste0(.$title,.$text)) %>%
  select(CELEX, total.text)

gc() # clear up some space before the text pulling

Q1edge.NOtextsec2 <- 
  rbind(fromQ1.NOtextdf, toQ1.NOtextdf) %>%
  distinct(., .keep_all = TRUE) %>% # 546 celex
  slice(274:546) %>% # 515
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX)) %>% 
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  mutate(text = map_chr(url, elx_fetch_data, "text")) %>% 
  as_tibble() %>%
  mutate(total.text = paste0(.$title,.$text)) %>%
  select(CELEX, total.text)


Q1.edge.text <-
  rbind(Q1edge.text,Q1edge.NOtextsec1,Q1edge.NOtextsec2) #546 + 37 = 583 --> all checks out this is the total number of vertices in the network 

V(Q1.net2nd.graph) # 583 vertices
edges <- degree(Q1.net2nd.graph)
sum(edges)
# Query two ---------------------

# Citation text --------

# check with "to" documents we already have text from

# to of the edge df
toQ2.textdf <- 
  Q2.net2nd %>%
  select(to) %>%
  filter(to %in% MPA.DESG.text.cut$CELEX) %>%
  left_join(.,MPA.DESG.text.cut, by = c("to"="CELEX")) %>%
  distinct(., .keep_all = TRUE) %>% # since some cite multiple documents. 
  rename("CELEX" = "to")
# 36
toQ2.NOtextdf <- 
  Q2.net2nd %>%
  select(to) %>%
  filter(! to %in% MPA.DESG.text.cut$CELEX) %>%
  distinct(., .keep_all = TRUE) %>% # since some cite multiple documents. 
  rename("CELEX" = "to")
# 1029

# 36 + 1029 = 1065

Q2.net2nd %>%
  select(to) %>%
  distinct(., .keep_all = TRUE) %>% # since some cite multiple documents. 
  dim()
# math checks out there is 1065 observations (celex)

# Citer text ----------

#from of the edge df 
fromQ2.textdf <- 
  Q2.net2nd %>%
  select(from) %>%
  filter(from %in% MPA.DESG.text.cut$CELEX) %>%
  left_join(.,MPA.DESG.text.cut, by = c("from"="CELEX")) %>%
  distinct(., .keep_all = TRUE) %>% # since some cite multiple documents. 
  rename("CELEX" = "from")
# 85

fromQ2.NOtextdf <- 
  Q2.net2nd %>%
  select(from) %>%
  filter(! from %in% MPA.DESG.text.cut$CELEX) %>%
  distinct(., .keep_all = TRUE) %>% # since some cite multiple documents. 
  rename("CELEX" = "from")
# 227

# 85 + 227 = 312

Q2.net2nd %>%
  select(from) %>%
  distinct(., .keep_all = TRUE) %>% # since some cite multiple documents. 
  dim()
# math checks out there is 312 observations (celex)


# join the to and from columns and remove duplicates
Q2edge.text <-
  rbind(fromQ2.textdf, toQ2.textdf) %>%
  distinct(., .keep_all = TRUE) # 97 celex

gc() # clear up some space before the text pulling

# too big cut in half then re-join (section 1 will be rows 1-514, section 2 will be 515-1029)
Q2edge.NOtext.sec1 <- 
  rbind(fromQ2.NOtextdf, toQ2.NOtextdf) %>%
  distinct(., .keep_all = TRUE) %>% # 1029 celex
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX)) %>%
  slice(1:343) %>% # 343
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  mutate(text = map_chr(url, elx_fetch_data, "text")) %>% 
  as_tibble() %>%
  mutate(total.text = paste0(.$title,.$text)) %>%
  select(CELEX, total.text)

gc()

Q2edge.NOtext.sec2 <- 
  rbind(fromQ2.NOtextdf, toQ2.NOtextdf) %>%
  distinct(., .keep_all = TRUE) %>% # 1029 celex
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX)) %>%
  slice(344:686) %>% # 343
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  mutate(text = map_chr(url, elx_fetch_data, "text")) %>% 
  as_tibble() %>%
  mutate(total.text = paste0(.$title,.$text)) %>%
  select(CELEX, total.text)


Q2edge.NOtext.sec3 <- 
  rbind(fromQ2.NOtextdf, toQ2.NOtextdf) %>%
  distinct(., .keep_all = TRUE) %>% # 1029 celex
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX)) %>%
  slice(687:1029) %>% # 343
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  mutate(text = map_chr(url, elx_fetch_data, "text")) %>% 
  as_tibble() %>%
  mutate(total.text = paste0(.$title,.$text)) %>%
  select(CELEX, total.text)
  
Q2.edge.text <-
  rbind(Q2edge.text,Q2edge.NOtext.sec1,Q2edge.NOtext.sec2,Q2edge.NOtext.sec3) #1029 + 97 = 1126 --> all checks out this is the total number of vertices in the network 

V(Q2.net2nd.graph) # 1126 vertices
edges <- degree(Q2.net2nd.graph)
sum(edges)
# Save ---------------------------------------------------------------

write.csv(Q2.edge.text, file = "WP4/Policy_Interactions/data/05.Q2C2.edge.text.csv", row.names=FALSE)
write.csv(Q1.edge.text, file = "WP4/Policy_Interactions/data/05.Q1C2.edge.text.csv", row.names=FALSE)






