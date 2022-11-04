
# Clear work space ---------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ----------------------------------------------------------

library("igraph")
library("tidyverse")
library("stm")

# Load data ---------------------------------------------------------------

Q1C1.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q1C1.edgelist.topics.csv")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q1C1_stm.Rdata")

Q1C2.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q1C2.edgelist.topics.csv")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q1C2_stm.Rdata")

Q2C1.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q2C1.edgelist.topics.csv")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q2C1_stm.Rdata")

Q2C2.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/08.Q2C2.edgelist.topics.csv")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q2C2_stm.Rdata")


########### Query 1 Citation 1 ##################

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
degree <- degree(network)
sort(degree)
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#    0%  25%  50%  75%  95% 100% 
#    1    2    3    5   14  103  

sort(degree)
l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(degree(network) >=14 & V(network)$type == "topic",V(network)$name,NA),
     vertex.color= V(network)$color,
     vertex.size=  2,
     layout = l)


# Topics >= 95% percentile for network degree numbers
cloud(Q1C1.stm, topic = 16) # EU theme (councile parliament)
cloud(Q1C1.stm, topic = 56) # hmm this seems to be a theme about "time/deadlines"
cloud(Q1C1.stm, topic = 36) # fisheries vessel managment theme
cloud(Q1C1.stm, topic = 17) # technology and research theme
cloud(Q1C1.stm, topic = 45) # tuna theme
cloud(Q1C1.stm, topic = 48) # hmm maybe about marine boundaries or location discriptors
cloud(Q1C1.stm, topic = 12) # proceedure and ruling theme
cloud(Q1C1.stm, topic = 7)  # biodiversity theme... **documentbreak is odd look into this --> this is not in the text but a break line in the text code
cloud(Q1C1.stm, topic = 26) # waste treatment/managment theme
cloud(Q1C1.stm, topic = 39) # hmm... names of countries/govermnets
cloud(Q1C1.stm, topic = 53) # marine species theme
  

# all other topics 
cloud(Q1C1.stm, topic = 1) 
cloud(Q1C1.stm, topic = 2) 
cloud(Q1C1.stm, topic = 3) 
cloud(Q1C1.stm, topic = 4) 
cloud(Q1C1.stm, topic = 5) # strange... 
cloud(Q1C1.stm, topic = 6) # climate change and food production (agricultiure/farm/aquaculture)
cloud(Q1C1.stm, topic = 7) 
cloud(Q1C1.stm, topic = 8) 
cloud(Q1C1.stm, topic = 9) 


########### Query 1 Citation 2 ##################

from <-
  Q1C2.edgelist.topics %>%
  select(from, from.topic) %>%
  rename("celex" = "from",
         "topic"="from.topic")

to <-
  Q1C2.edgelist.topics %>%
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
degree <- degree(network)
sort(degree)
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#    0%  25%  50%  75%  95% 100% 
#    1    1    2    3   12  496   


sort(degree)
l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(degree(network) >=12 & V(network)$type == "topic",V(network)$name,NA),
     vertex.color= V(network)$color,
     vertex.size=  2,
     layout = l)


# Topics >= 95% percentile for network degree numbers
cloud(Q1C2.stm, topic = 3) # EU theme (councile parliament) same as Q1C1
cloud(Q1C2.stm, topic = 11) # research and development theme
cloud(Q1C2.stm, topic = 10) # fishing vessel manament theme
cloud(Q1C2.stm, topic = 64) # marine specices theme
cloud(Q1C2.stm, topic = 15) # quota allocation/regulations
cloud(Q1C2.stm, topic = 65)
cloud(Q1C2.stm, topic = 49) # data processing and protection
cloud(Q1C2.stm, topic = 34) # animal health hints of food quality/safety
cloud(Q1C2.stm, topic = 18) # hmm this seems to be a theme about "time/deadlines"
cloud(Q1C2.stm, topic = 44) # production/proccessing theme
cloud(Q1C2.stm, topic = 46) # hmm maybe about marine boundaries or location/geography discriptors
cloud(Q1C2.stm, topic = 23)
cloud(Q1C2.stm, topic = 35) # organic matter/content/ingredient/nutrients theme
cloud(Q1C2.stm, topic = 42)
cloud(Q1C2.stm, topic = 68)
cloud(Q1C2.stm, topic = 67) # french words...
cloud(Q1C2.stm, topic = 66) # managment/conservation/protected area
cloud(Q1C2.stm, topic = 48) # language/translation theme
cloud(Q1C2.stm, topic = 53) # fisheries regulations more specifically - catch limits and gear reg.
cloud(Q1C2.stm, topic = 45) # skin theme - term of toxins/sensitivity
cloud(Q1C2.stm, topic = 41) # transportation focus on international transport
cloud(Q1C2.stm, topic = 37) # communication access/services/infrastructure (i.e. internet)
cloud(Q1C2.stm, topic = 1)  # substance classifications (i.e. hazardous, toxic, dangerous, concentration)
cloud(Q1C2.stm, topic = 40) # water quality/managment
cloud(Q1C2.stm, topic = 21) # 
cloud(Q1C2.stm, topic = 6)  # energy resources (i.e. gas, oil, carbon, petrolium)
cloud(Q1C2.stm, topic = 24) # managing bodies  (i.e. depatments/insitutes/offices/nations)
cloud(Q1C2.stm, topic = 17) # chemical substances/qualities


########### Query 2 Citation 1 ##################

from <-
  Q2C1.edgelist.topics %>%
  select(from, from.topic) %>%
  rename("celex" = "from",
         "topic"="from.topic")

to <-
  Q2C1.edgelist.topics %>%
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
degree <- degree(network)
sort(degree)
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#    0%   25%     50%   75%    95% 100% 
#  1.00   2.00   3.00   6.00  14.55 107.00 


sort(degree)
l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(degree(network) >=14.55  & V(network)$type == "topic",V(network)$name,NA),
     vertex.color= V(network)$color,
     vertex.size=  2,
     layout = l)


# Topics >= 95% percentile for network degree numbers
cloud(Q2C1.stm, topic = 64) # EU theme (councile parliament) same as Q1C1
cloud(Q2C1.stm, topic = 55) # fisheries regulations (catch and gear limits)
cloud(Q2C1.stm, topic = 62) # marine species theme
cloud(Q2C1.stm, topic = 14) # conventions/agreements/contracts/parties
cloud(Q2C1.stm, topic = 43) # waste types and manament
cloud(Q2C1.stm, topic = 68)
cloud(Q2C1.stm, topic = 30) # research and development


########### Query 2 Citation 2 ##################

from <-
  Q2C2.edgelist.topics %>%
  select(from, from.topic) %>%
  rename("celex" = "from",
         "topic"="from.topic")

to <-
  Q2C2.edgelist.topics %>%
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
degree <- degree(network)
sort(degree)
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#    0%   25%     50%   75%    95%    100% 
#     1    2        3    5     14    315


sort(degree)
l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(degree(network) >=14  & V(network)$type == "topic",V(network)$name,NA),
     vertex.color= V(network)$color,
     vertex.size=  2,
     layout = l)


# Topics >= 95% percentile for network degree numbers
cloud(Q2C2.stm, topic = 53) # EU theme (councile parliament) same as all the other networks
cloud(Q2C2.stm, topic = 16) # public offic topic
cloud(Q2C2.stm, topic = 26) # rulings/decisions/proceedures theme
cloud(Q2C2.stm, topic = 49) # research and development
cloud(Q2C2.stm, topic = 50) # fisheries vessel managment
cloud(Q2C2.stm, topic = 15) 
cloud(Q2C2.stm, topic = 67) # marine species (larger emph on tuna than the others)
cloud(Q2C2.stm, topic = 21) # quota alloccation and regulations
cloud(Q2C2.stm, topic = 46) # animal production, animal health, food safety aswell a little?
cloud(Q2C2.stm, topic = 17) # fisheries/stock conservation (scientific advice/managment? as well)
cloud(Q2C2.stm, topic = 60) # hmm maybe about marine boundaries or location/geography discriptors
cloud(Q2C2.stm, topic = 23)
cloud(Q2C2.stm, topic = 55) # water quality and managment
cloud(Q2C2.stm, topic = 58) # infrastructure (transportation and communication related)
cloud(Q2C2.stm, topic = 47) # ingredients/food properties/nutrition/quality?
cloud(Q2C2.stm, topic = 40) # costs,aid,value, price, etc.. hard to pinpoint an exact topic
cloud(Q2C2.stm, topic = 23)
cloud(Q2C2.stm, topic = 37) # product information and design? i can see trade mark, sedign, application, description, origin 
cloud(Q2C2.stm, topic = 25) # german, french words??
cloud(Q2C2.stm, topic = 34) # financal theme
cloud(Q2C2.stm, topic = 35) # waste materials/types of waste
cloud(Q2C2.stm, topic = 44) # data protection and processing
cloud(Q2C2.stm, topic = 51) 
cloud(Q2C2.stm, topic = 24) 

cloud(Q2C2.stm, topic = 21) # this loooks almost identical to topic number 21 but with other countries associated


# archival --------------

###########################################

# just wanted to try out this idea but looks very very funky and messy...

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

     