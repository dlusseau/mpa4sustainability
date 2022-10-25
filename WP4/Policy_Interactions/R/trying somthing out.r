
# Clear work space ---------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ----------------------------------------------------------

library("igraph")
library("tidyverse")

# Load data ---------------------------------------------------------------

Q1C1.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q1C1.edgelist.topics.csv")
Q1C2.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q1C1.edgelist.topics.csv")

Q2C1.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q2C1.edgelist.topics.csv")
Q2C2.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q2C2.edgelist.topics.csv")

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
  group_by(from, to, NOTsame.topic) %>%
  summarise(No.topics = n_distinct())
  

new.edgelist <-
  rbind(Q1C1.same.topics,Q1C1.NOTsame.topics)




network <- graph_from_data_frame(new.edgelist, directed = TRUE) 
V(network)

E(network)$color[E(network)$same.topic == TRUE] <- 'green'
E(network)$color[E(network)$same.topic == FALSE] <- 'red'


l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=NA,
     edge.width =E(network)$n.topics/6,
     edge.color= E(network)$color,
     vertex.size= 2,
     layout = l)

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
         to = topic)


celex <- 
  edges %>%
  select(from) %>%
  mutate(color = "#E69F00")%>%
  rename("name" = "from")

topic <- 
  edges %>%
  select(to) %>%
  mutate(color = "#56B4E9")%>%
  rename("name" = "to")

vertices <- 
  rbind(celex,topic)%>%
  distinct


network <- graph_from_data_frame(edges,vertices = vertices, directed = FALSE) 
V(network)


library(RColorBrewer)
col  <- brewer.pal(3, "Set2") 
col <- col[-1]
V(network)$color <- col[as.numeric(as.factor(V(network)$color))]
# DK doc are the red ones...
degree <- degree(network)

l <- layout.fruchterman.reingold(network)

plot(network,
          vertex.label=NA,
     vertex.color= V(network)$color,
     vertex.size= 2,
     layout = layout.circle)
     