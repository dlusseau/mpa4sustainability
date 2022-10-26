
# Clear work space ---------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ----------------------------------------------------------

library("igraph")
library("tidyverse")

# Load data ---------------------------------------------------------------

Q1C1.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q1C1.edgelist.topics.csv")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q1C1_stm.Rdata")

Q1C2.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q1C1.edgelist.topics.csv")

Q2C1.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q2C1.edgelist.topics.csv")
Q2C2.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q2C2.edgelist.topics.csv")


########### testing this ##################

from <-
  Q1C1.edgelist.topics %>%
  select(from, from.topic) %>%
  rename("celex" = "from",
         "topic"="from.topic")

to <-
  Q1C1.edgelist.topics %>%
  select(to, to.topic)%>%
  rename("celex" = "to",
         "topic"="to.topic")

edges <- 
  rbind(from, to) %>%
  distinct() %>%
  rename(from = celex,
         to = topic)%>%
  filter(!is.na(to))


celex <- 
  edges %>%
  select(from) %>%
  mutate(type = "celex")%>%
  rename("name" = "from")

topic <- 
  edges %>%
  select(to) %>%
  mutate(type = "topic")%>%
  rename("name" = "to")

vertices <- 
  rbind(celex,topic)%>%
  distinct() %>%
  filter(!is.na(name))


network <- graph_from_data_frame(edges,vertices = vertices, directed = FALSE) 
V(network)


library(RColorBrewer)
col  <- brewer.pal(2, "Set2") 
#col <- col[-1]
V(network)$color <- col[as.numeric(as.factor(V(network)$type))]
# EUdocs are orange, topics  are blue
degree <- degree(network)
sort(degree)
l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(V(network)$type == "topic",V(network)$name,NA),
     vertex.color= V(network)$color,
     vertex.size=  2,
     layout = l)



cloud(Q1C1.stm, topic = 32, scale= c(2,2))
cloud(Q1C1.stm, topic = 59, scale= c(2,2))
cloud(Q1C1.stm, topic = 17, scale= c(2,2))
cloud(Q1C1.stm, topic = 33)
cloud(Q1C1.stm, topic = 13)
cloud(Q1C1.stm, topic = 67)
cloud(Q1C1.stm, topic = 70)
cloud(Q1C1.stm, topic = 49)
cloud(Q1C1.stm, topic = 38)


# archival --------------

###########################################

Q1C1.same.topics <- 
  Q1C1.edgelist.topics %>%
  ungroup%>%
  filter(same.topic == TRUE)%>%
  select(-from.topic) %>%
  group_by(from, to, same.topic) %>%
  summarise(n.topics = n_distinct(to.topic))


Q1C1.NOTsame.topics <- 
  Q1C1.edgelist.topics %>%
  ungroup%>%
  filter(same.topic == FALSE)%>%
  select(-from.topic) %>%
  group_by(from, to, same.topic) %>%
  summarise(n.topics = n_distinct(to.topic))



new.edgelist <-
  rbind(Q1C1.same.topics,Q1C1.NOTsame.topics)




network <- graph_from_data_frame(new.edgelist, directed = TRUE) 
V(network)

E(network)$color[E(network)$same.topic == TRUE] <- 'green'
E(network)$color[E(network)$same.topic == FALSE] <- 'red'


l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=NA,
     edge.width =E(network)$n.topics/2,
     edge.color= E(network)$color,
     vertex.size= 2,
     layout = l)

     