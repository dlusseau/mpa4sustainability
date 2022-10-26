
# Clear work space ---------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ----------------------------------------------------------
library("stringr")
library("stm")
library("stminsights")
library("ggraph")
library("tidyverse")

# Load data ---------------------------------------------------------------

# R code to engage in topic modelling of EUR-LEX relevant text

# query 1
Q1C1.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q1C1.preptext.rds")
Q1C1.edgelist<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1firstordercit.edgelist.csv")

Q1C2.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q1C2.preptext.rds")
Q1C2.edgelist<- read.csv(file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1secondordercit.edgelist.csv")

# query 2
Q2C1.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q2C1.preptext.rds")
Q2C1.edgelist<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q2firstordercit.edgelist.csv")

Q2C2.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q2C2.preptext.rds")
Q2C2.edgelist<- read.csv(file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q2secondordercit.edgelist.csv")

# Topic predictions

# query 1
# Q1C1.stm<-stm(Q1C1.text$documents,Q1C1.text$vocab,data=Q1C1.text$meta,K=0,init.type="Spectral")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q1C1_stm.Rdata")
# Q1C2.stm<-stm(Q1C2.text$documents,Q1C2.text$vocab,data=Q1C2.text$meta,K=0,init.type="Spectral")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q1C2_stm.Rdata")

# query 2
# Q2C1.stm<-stm(Q2C1.text$documents,Q2C1.text$vocab,data=Q2C1.text$meta,K=0,init.type="Spectral")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q2C1_stm.Rdata")
# Q2C2.stm<-stm(Q2C2.text$documents,Q2C2.text$vocab,data=Q2C2.text$meta,K=0,init.type="Spectral")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q2C2_stm.Rdata")


# Finding topics within each document ---------------------------------------

#  Query 1 -----------------------------------------------------------------

# ---- first order citations ----- #
Q1C1.DT.matrix <- Q1C1.stm$theta # rows are the text/"document" and columns are the topics, values are the topic proportions
Q1C1.TW.list <- Q1C1.stm$beta # list of log word probabilities for each topic
Q1C1.vocab <- Q1C1.stm$vocab # the vocab within the list above

Q1C1.sentence.names <- as.data.frame(names(Q1C1.text$documents))
# lets make this into a long df
Q1C1.Doctopic.longdf <-
  Q1C1.DT.matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "row.id") %>%
  cbind(., Q1C1.sentence.names, Q1C1.text$meta$CELEX) %>% 
  relocate(c("names(Q1C1.text$documents)"), .after = c(row.id)) %>%
  relocate(c("Q1C1.text$meta$CELEX"), .after = c("names(Q1C1.text$documents)")) %>%
  pivot_longer(.,
               cols = 4:76,
               names_to = "topic", 
               values_to = "proportion") %>%
  mutate(topic = str_replace_all(topic, "V", "topic"),
         percent.doc = proportion *100) %>%
  rename("document_sentence" = "names(Q1C1.text$documents)",
         "CELEX" = "Q1C1.text$meta$CELEX")

# now select the topic with the highest prop. to each sentence
Q1C1max.topic <-
  Q1C1.Doctopic.longdf %>%
  group_by(document_sentence) %>%
  filter(percent.doc == max(percent.doc)) %>%
  filter(percent.doc > 50)

Q1C1topic.no <- 
  Q1C1max.topic %>%
  ungroup() %>%
  group_by(CELEX) %>%
  summarise(n.topics = n_distinct(topic))

summary(Q1C1max.topic)
#  proportion      percent.doc   
#Min.   :0.0343   Min.   : 3.43  
#1st Qu.:0.1170   1st Qu.:11.70  
#Median :0.1772   Median :17.72  
#Mean   :0.2257   Mean   :22.57  
#3rd Qu.:0.2819   3rd Qu.:28.19  
#Max.   :0.9978   Max.   :99.78  

# After setting a filter > 50
#  proportion      percent.doc   
# Min.   :0.5001   Min.   :50.01  
# 1st Qu.:0.5466   1st Qu.:54.66  
# Median :0.6212   Median :62.12  
# Mean   :0.6575   Mean   :65.75  
# 3rd Qu.:0.7386   3rd Qu.:73.86  
# Max.   :0.9978   Max.   :99.78
                                             
Q1C1topics <-
  Q1C1max.topic %>%
  ungroup() %>%
  distinct(CELEX,topic)

n_distinct(Q1C1topics$topic) # 56 
sort(unique(Q1C1topics$topic)) 

#lets join the edge list and the topic assignments

Q1C1.edgelist1 <- 
  Q1C1.edgelist %>%
  left_join(.,Q1C1topics, by= c("from" = "CELEX")) %>%
  rename("from.topic" = "topic") %>% 
  left_join(.,Q1C1topics, by= c("to" = "CELEX")) %>%
  rename("to.topic" = "topic")

# label if they discuss the same topics:
Q1C1.edgelist.cat <- 
  Q1C1.edgelist1 %>%
  ungroup() %>%
  mutate(same.topic = case_when(from.topic == to.topic ~ "TRUE",
                                from.topic != to.topic ~ "FALSE")) 

Q1C1YorN <- 
  Q1C1.edgelist.cat %>%
  select(from,to,same.topic) %>%
  distinct()
  filter(same.topic == "TRUE")

Q1C1YorN %>%group_by(from,to)%>% summarise(n=n_distinct(same.topic))

Q1C.labels<-labelTopics(Q1C1.stm,n =50) 

Q1C1.net <- get_network(model = Q1C1.stm,
                         method = 'simple',
                         labels = Q1.labels$prob,
                         cutoff = 0.05,
                         cutiso = FALSE)

ggraph(Q1C1.net, layout = 'fr') +
  geom_edge_link(
    aes(edge_width = weight),
    label_colour = '#fc8d62',
    edge_colour = '#377eb8') +
  geom_node_point(size = 4, colour = 'black')  +
  geom_node_label(
    aes(label = name, size = props),
    colour = 'black',  repel = TRUE, alpha = 0.85) +
  scale_size(range = c(2, 10), labels = scales::percent) +
  labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation') +
  scale_edge_width(range = c(.1, 3)) +
  theme_graph()

#prep <- estimateEffect(1:39 ~ CELEX, Q1C1.stm,meta = Q1C1.text$meta, uncertainty = "Global") #?
#summary(prep, topics = 1:2)
#plot(prep, covariate = "CELEX", topics = c(22,25,17,4),model = Q1C1.stm, method = "pointestimate",n=5)

# ---- second order citations ----- #
Q1C2.DT.matrix <- Q1C2.stm$theta # rows are the text/"document" and columns are the topics, values are the topic proportions
Q1C2.TW.list <- Q1C2.stm$beta # list of log word probabilities for each topic
Q1C2.vocab <- Q1C2.stm$vocab # the vocab within the list above

Q1C2.sentence.names <- as.data.frame(names(Q1C2.text$documents))

# lets make this into a long df
Q1C2.Doctopic.longdf <-
  Q1C2.DT.matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "row.id") %>%
  cbind(., Q1C2.sentence.names, Q1C2.text$meta$CELEX) %>% 
  relocate(c("names(Q1C2.text$documents)"), .after = c(row.id)) %>%
  relocate(c("Q1C2.text$meta$CELEX"), .after = c("names(Q1C2.text$documents)")) %>%
  pivot_longer(.,
               cols = 4:80,
               names_to = "topic", 
               values_to = "proportion") %>%
  mutate(topic = str_replace_all(topic, "V", "topic"),
         percent.doc = proportion *100) %>%
  rename("document_sentence" = "names(Q1C2.text$documents)",
         "CELEX" = "Q1C2.text$meta$CELEX")

# now select the topic with the highest prop. to each sentence
Q1C2max.topic <-
  Q1C2.Doctopic.longdf %>%
  group_by(document_sentence) %>%
  filter(percent.doc == max(percent.doc))%>%
  filter(percent.doc > 50)

Q1C2topic.no <- 
  Q1C2max.topic %>%
  ungroup() %>%
  group_by(CELEX) %>%
  summarise(n.topics = n_distinct(topic))

summary(Q1C2max.topic)
# percent.doc   
# Min.   : 3.185  
# 1st Qu.:11.508  
# Median :17.502  
# Mean   :22.783  
# 3rd Qu.:28.388  
# Max.   :99.872  

# After >50 filter
# Min.   :50.00  
# 1st Qu.:56.19  
# Median :62.60  
# Mean   :66.20  
# 3rd Qu.:75.54  
# Max.   :99.87 

Q1C2topics <-
  Q1C2max.topic %>%
  ungroup() %>%
  distinct(CELEX,topic)

n_distinct(Q1C2topics$topic) # 65 
sort(unique(Q1C2topics$topic)) 

#lets join the edge list and the topic assignments

Q1C2.edgelist <- 
  Q1C2.edgelist %>%
  left_join(.,Q1C2topics, by= c("from" = "CELEX")) %>%
  rename("from.topic" = "topic") %>% 
  left_join(.,Q1C2topics, by= c("to" = "CELEX")) %>%
  rename("to.topic" = "topic")

# label if they discuss the same topics:
Q1C2.edgelist.cat <- 
  Q1C2.edgelist %>%
  ungroup() %>%
  mutate(same.topic = case_when(from.topic == to.topic ~ "TRUE",
                                from.topic != to.topic ~ "FALSE")) 

Q1C2YorN <- 
  Q1C2.edgelist.cat %>%
  select(from,to,same.topic) %>%
  distinct()
filter(same.topic == "TRUE")

#  Query 2 -----------------------------------------------------------------------------------------------------------------------------------------


# ---- first order citations ----- #
Q2C1.DT.matrix <- Q2C1.stm$theta # rows are the text/"document" and columns are the topics, values are the topic proportions
Q2C1.TW.list <- Q2C1.stm$beta # list of log word probabilities for each topic
Q2C1.vocab <- Q2C1.stm$vocab # the vocab within the list above

Q2C1.sentence.names <- as.data.frame(names(Q2C1.text$documents))
# lets make this into a long df
Q2C1.Doctopic.longdf <-
  Q2C1.DT.matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "row.id") %>%
  cbind(., Q2C1.sentence.names, Q2C1.text$meta$CELEX) %>% 
  relocate(c("names(Q2C1.text$documents)"), .after = c(row.id)) %>%
  relocate(c("Q2C1.text$meta$CELEX"), .after = c("names(Q2C1.text$documents)")) %>%
  pivot_longer(.,
               cols = 4:80,
               names_to = "topic", 
               values_to = "proportion") %>%
  mutate(topic = str_replace_all(topic, "V", "topic"),
         percent.doc = proportion *100) %>%
  rename("document_sentence" = "names(Q2C1.text$documents)",
         "CELEX" = "Q2C1.text$meta$CELEX")


# now select the topic with the highest prop. to each sentence
Q2C1max.topic <-
  Q2C1.Doctopic.longdf %>%
  group_by(document_sentence) %>%
  filter(percent.doc == max(percent.doc)) %>%
  filter(percent.doc > 50)

Q2C1topic.no <- 
  max.topic %>%
  ungroup() %>%
  group_by(CELEX) %>%
  summarise(n.topics = n_distinct(topic))

summary(Q2C1max.topic)
#  percent.doc   
# Min.   : 3.547  
# 1st Qu.:12.826  
# Median :19.413  
# Mean   :24.038  
# 3rd Qu.:30.337  
# Max.   :99.517  

# After >50 filter
# Min.   :50.00  
# 1st Qu.:54.83  
# Median :61.18  
# Mean   :65.14  
# 3rd Qu.:73.18  
# Max.   :99.52 

Q2C1topics <-
  Q2C1max.topic %>%
  ungroup() %>%
  distinct(CELEX,topic)

n_distinct(Q2C1topics$topic) # 57
sort(unique(Q2C1topics$topic)) 

#lets join the edge list and the topic assignments

Q2C1.edgelist1 <- 
  Q2C1.edgelist %>%
  left_join(.,Q2C1topics, by= c("from" = "CELEX")) %>%
  rename("from.topic" = "topic") %>% 
  left_join(.,Q2C1topics, by= c("to" = "CELEX")) %>%
  rename("to.topic" = "topic")

# label if they discuss the same topics:
Q2C1.edgelist.cat <- 
  Q2C1.edgelist1 %>%
  ungroup() %>%
  mutate(same.topic = case_when(from.topic == to.topic ~ "TRUE",
                                from.topic != to.topic ~ "FALSE")) 

Q2C1YorN <- 
  Q2C1.edgelist.cat %>%
  select(from,to,same.topic) %>%
  distinct()

Q2C1YorN %>%group_by(from,to)%>% summarise(n=n_distinct(same.topic))


# ---- second order citations ----- #
Q2C2.DT.matrix <- Q2C2.stm$theta # rows are the text/"document" and columns are the topics, values are the topic proportions
Q2C2.TW.list <- Q2C2.stm$beta # list of log word probabilities for each topic
Q2C2.vocab <- Q2C2.stm$vocab # the vocab within the list above

Q2C2.sentence.names <- as.data.frame(names(Q2C2.text$documents))

# lets make this into a long df
Q2C2.Doctopic.longdf <-
  Q2C2.DT.matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "row.id") %>%
  cbind(., Q2C2.sentence.names, Q2C2.text$meta$CELEX) %>% 
  relocate(c("names(Q2C2.text$documents)"), .after = c(row.id)) %>%
  relocate(c("Q2C2.text$meta$CELEX"), .after = c("names(Q2C2.text$documents)")) %>%
  pivot_longer(.,
               cols = 4:67,
               names_to = "topic", 
               values_to = "proportion") %>%
  mutate(topic = str_replace_all(topic, "V", "topic"),
         percent.doc = proportion *100) %>%
  rename("document_sentence" = "names(Q2C2.text$documents)",
         "CELEX" = "Q2C2.text$meta$CELEX")


# now select the topic with the highest prop. to each sentence
Q2C2max.topic <-
  Q2C2.Doctopic.longdf %>%
  group_by(document_sentence) %>%
  filter(percent.doc == max(percent.doc)) %>%
  filter(percent.doc > 50)

Q2C2topic.no <- 
  Q2C2max.topic %>%
  ungroup() %>%
  group_by(CELEX) %>%
  summarise(n.topics = n_distinct(topic))

summary(Q2C2max.topic)
# percent.doc    
#   Min.   : 3.983  
#   1st Qu.:13.366  
#   Median :19.885  
#   Mean   :25.777  
#   3rd Qu.:32.167  
#   Max.   :99.954  

# After filter >50  
#Min.   :50.00  
#1st Qu.:56.91  
#Median :64.40  
#Mean   :67.53  
#3rd Qu.:77.81  
#Max.   :99.95 

Q2C2topics <-
  Q2C2max.topic %>%
  ungroup() %>%
  distinct(CELEX,topic)

n_distinct(Q2C2topics$topic) # 53

#lets join the edge list and the topic assignments

Q2C2.edgelist <- 
  Q2C2.edgelist %>%
  left_join(.,Q2C2topics, by= c("from" = "CELEX")) %>%
  rename("from.topic" = "topic") %>% 
  left_join(.,Q2C2topics, by= c("to" = "CELEX")) %>%
  rename("to.topic" = "topic")

# label if they discuss the same topics:
Q2C2.edgelist.cat <- 
  Q2C2.edgelist %>%
  ungroup() %>%
  mutate(same.topic = case_when(from.topic == to.topic ~ "TRUE",
                                from.topic != to.topic ~ "FALSE")) 

Q2C2YorN <- 
  Q2C2.edgelist.cat %>%
  select(from,to,same.topic) %>%
  distinct()
filter(same.topic == "TRUE")

par(mfrow=c(2,2))
hist(Q1C1max.topic$percent.doc)
hist(Q1C2max.topic$percent.doc)
hist(Q2C1max.topic$percent.doc)
hist(Q2C2max.topic$percent.doc)


# Save -----------------------------------------------------------------------

write.csv(Q1C1.edgelist.cat, file = "WP4/Policy_Interactions/data/08.Q1C1.edgelist.topics.csv", row.names=FALSE)
write.csv(Q1C2.edgelist.cat, file = "WP4/Policy_Interactions/data/08.Q1C2.edgelist.topics.csv", row.names=FALSE)
write.csv(Q2C1.edgelist.cat, file = "WP4/Policy_Interactions/data/08.Q2C1.edgelist.topics.csv", row.names=FALSE)
write.csv(Q2C2.edgelist.cat, file = "WP4/Policy_Interactions/data/08.Q2C2.edgelist.topics.csv", row.names=FALSE)



