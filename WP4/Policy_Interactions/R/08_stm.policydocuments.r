
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
Q1C2.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q1C2.preptext.rds")
# query 2
Q2C1.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q2C1.preptext.rds")
Q2C2.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q2C2.preptext.rds")

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


#Finding topics within each document ---------------------------------------

#  Query 1 -----------------------------------------------------------------

Q1.DT.matrix <- Q1.stm$theta # rows are the text/"document" and columns are the topics, values are the topic proportions
Q1.TW.list <- Q1.stm$beta # list of log word probabilities for each topic
Q1.vocab <- Q1.stm$vocab # teh vocab within the list above

# lets make this into a long df
Q1.Doctopic.longdf <-
  Q1.DT.matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "document_sentence") %>%
  cbind(., Q1.text$meta$CELEX)
  pivot_longer(.,
               cols = 2:68,
               names_to = "topic", 
               values_to = "proportion") %>%
  mutate(topic = str_replace_all(topic, "V", "topic"),
         percent.doc = proportion *100) %>%
  cbind(., Q1.text$meta$CELEX)


Q1.labels<-labelTopics(Q1.stm,n =7) 

Q1.net <- get_network(model = Q1.stm,
                         method = 'simple',
                         labels = Q1.labels$prob,
                         cutoff = 0.05,
                         cutiso = FALSE)

ggraph(Q1.net, layout = 'fr') +
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

prep <- estimateEffect(1:39 ~ CELEX, Q1.stm,meta = Q1.text$meta, uncertainty = "Global") #?
summary(prep, topics = 1:2)
plot(prep, covariate = "CELEX", topics = c(22,25,17,4),model = Q1.stm, method = "pointestimate",n=5)


#  Query 2 -----------------------------------------------------------------------------------------------------------------------------------------

Q2.DT.matrix <- Q2.stm$theta # rows are the text/"document" and columns are the topics, values are the topic proportions
Q2.TW.list <- Q2.stm$beta # list of log word probabilities for each topic
Q2.vocab <- Q2.stm$vocab # teh vocab within the list above

# lets make this into a long df
Q2.Doctopic.longdf <-
  Q2.DT.matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "document_sentence") %>%
  cbind(., Q2.text$meta$celex) %>%
  pivot_longer(.,
               cols = 2:68,
               names_to = "topic", 
               values_to = "proportion") %>%
  mutate(topic = str_replace_all(topic, "V", "topic"),
         percent.doc = proportion *100)


Q2.labels<-labelTopics(Q2.stm, n =3) 

Q2.net <- get_network(model = Q1.stm,
                      method = 'simple',
                      labels = Q1.labels$prob,
                      cutoff = 0.05,
                      cutiso = FALSE)

ggraph(Q2.net, layout = 'fr') +
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

#prep <- estimateEffect(1:39 ~ celex, Q2.stm,meta = Q1.text$meta, uncertainty = "Global") #?
#plot(prep, covariate = "lockdown", topics = c(22,25,17,4),model = paris.stm, labeltype="lift", method = "pointestimate",n=5)





