
# Clear work space ---------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ----------------------------------------------------------

library("igraph")
library("tidyverse")
library("stm")


# Load data ---------------------------------------------------------------

Q1C1.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/09.Q1C1.edgelist.topics.csv")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q1C1_stm.Rdata")

Q1C1.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q1C1.preptext.rds")
Q1C2.text<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/06_Q1C2.preptext.rds")

Q1C2.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/09.Q1C2.edgelist.topics.csv")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q1C2_stm.Rdata")

Q2C1.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/09.Q2C1.edgelist.topics.csv")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q2C1_stm.Rdata")

Q2C2.edgelist.topics<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/09.Q2C2.edgelist.topics.csv")
load(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q2C2_stm.Rdata")

#######################################################################################################################################
## Note for David: I did not have enough time to really go through all this in depth before I leave however here is what I started ####
#######################################################################################################################################

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
#    1    2    4    6   18  119 

sort(degree)
l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(degree(network) >=18 & V(network)$type == "topic",V(network)$name,NA),
     vertex.color= V(network)$color,
     vertex.size=  2,
     layout = l)


# Topics >= 95% percentile for network degree numbers
#topic7        topic58        topic41        topic23 
#18             18             19             20 
#topic57        topic31        topic55        topic56        topic30 
#21             22             22             25             44 
#topic10        topic52 
#88            119 
cloud(Q1C1.stm, topic = 52) # Content declaration: i.e. fertilizer and nutrient contents
cloud(Q1C1.stm, topic = 10) # EU council and parliament
cloud(Q1C1.stm, topic = 30) # responsible authority, administration, office
cloud(Q1C1.stm, topic = 56) # irrelevant. it is a time span/ date topic
cloud(Q1C1.stm, topic = 55) # marine species
cloud(Q1C1.stm, topic = 31) # research, development, technology
cloud(Q1C1.stm, topic = 57) # article procedure and adoption?
cloud(Q1C1.stm, topic = 23) # waste contents, managment, and treatment 
cloud(Q1C1.stm, topic = 41) # geographical location topic
cloud(Q1C1.stm, topic = 58) # journal and scientific study findings?? -NOT CLEAR
cloud(Q1C1.stm, topic = 7)  # fishing vessel and vessel managment

# all other topics...
cloud(Q1C1.stm, topic = 1) # nhc
cloud(Q1C1.stm, topic = 2) #  
cloud(Q1C1.stm, topic = 3) #  
cloud(Q1C1.stm, topic = 4) #   
cloud(Q1C1.stm, topic = 5) #  

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
# 1.0   1.0   2.0   4.0  10.9 632.0 


sort(degree)
l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(degree(network) >=12 & V(network)$type == "topic",V(network)$name,NA),
     vertex.color= V(network)$color,
     vertex.size=  2,
     layout = l)


# Topics >= 95% percentile for network degree numbers


#topic21         topic1      
#12             12              
#topic17        topic37        topic18        topic33 
#13             13             14             15 
#topic22         topic5        topic50        topic32        topic12 
#15             16             18             20             22 
#topic14             topic10        topic49         topic7 
#25                      30             31             35 
#topic41        topic25        topic23        topic28        topic42 
#39             44             47             48             51 
#topic46         topic8        topic31        topic53         topic9 
#53             60             68             75             95 
#topic11        topic15        topic27 
#116            147            632 
cloud(Q1C2.stm, topic = 27) # EU theme (councile parliament) same as Q1C1
cloud(Q1C2.stm, topic = 15) # project/ programme development
cloud(Q1C2.stm, topic = 11) 
cloud(Q1C2.stm, topic = 9) 
cloud(Q1C2.stm, topic = 53) 
cloud(Q1C2.stm, topic = 31) # animal health and safety  
cloud(Q1C2.stm, topic = 8)  # fishing vessels and fishing areas
cloud(Q1C2.stm, topic = 46) # fishing quoteas and other catch regulations
cloud(Q1C2.stm, topic = 42) # energy and fuel use
cloud(Q1C2.stm, topic = 28) 
cloud(Q1C2.stm, topic = 23) 
cloud(Q1C2.stm, topic = 25) 
cloud(Q1C2.stm, topic = 41) # waste/ organic materical managment and treatment
cloud(Q1C2.stm, topic = 7)  # personal data, GDPDR
cloud(Q1C2.stm, topic = 49) # financial institutes
cloud(Q1C2.stm, topic = 10) # compund/solution sensitivities
cloud(Q1C2.stm, topic = 14) # calculated/estimated value/price
cloud(Q1C2.stm, topic = 12) # authroization of foodstuff
cloud(Q1C2.stm, topic = 32) # receartional and cultural information
cloud(Q1C2.stm, topic = 50) # operational and equipment systems
cloud(Q1C2.stm, topic = 5)  # land-based infrastructure 
cloud(Q1C2.stm, topic = 22) # oil and gas extraction
cloud(Q1C2.stm, topic = 33) # financial payments, aid, grants, budgets
cloud(Q1C2.stm, topic = 18) 
cloud(Q1C2.stm, topic = 37) # emissions pollution
cloud(Q1C2.stm, topic = 17) # 
cloud(Q1C2.stm, topic = 1)  # content categories
cloud(Q1C2.stm, topic = 21) 


#need to make the text dataframe that only contains what was passed through
findThoughts(Q1C2.stm,texts=Q1C2.text$documents,topics = 5,n=3)


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
#      1    2     3     6      15  111


sort(degree)
l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(degree(network) >=14.55  & V(network)$type == "topic",V(network)$name,NA),
     vertex.color= V(network)$color,
     vertex.size=  2,
     layout = l)


# Topics >= 95% percentile for network degree numbers
#   topic31    topic62 
#    15         15 
# topic13    topic68    topic51    topic42    topic58    topic60    topic52 
#   18         20         21         21         22         45        111 
cloud(Q2C1.stm, topic = 52) # EU theme (councile parliament) 
cloud(Q2C1.stm, topic = 60) # catch inspection?? not sure
cloud(Q2C1.stm, topic = 58) # fishing vessel and vessel managment
cloud(Q2C1.stm, topic = 42) # 
cloud(Q2C1.stm, topic = 51) # energy emissions (efficency and pollution)
cloud(Q2C1.stm, topic = 68) # geographical location topic
cloud(Q2C1.stm, topic = 13) # product information and market
cloud(Q2C1.stm, topic = 62) # fisheries species (emph Tuna )
cloud(Q2C1.stm, topic = 31) # catch regulations


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
#     1    2       3     4     12     346

sort(degree)
l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(degree(network) >=12  & V(network)$type == "topic",V(network)$name,NA),
     vertex.color= V(network)$color,
     vertex.size=  2,
     layout = l)


# Topics >= 95% percentile for network degree numbers
#   topic26        topic55 
#    14             16 
#topic16        topic31        topic18        topic20             topic33 
#17             18             21             22                     29 
#topic44        topic12        topic57        topic21        topic56        topic35 
#31             32             34             34             36             39 
#topic54        topic51        topic47         topic3        topic11        topic30 
#39             42             42             49             63             65 
#topic29        topic37        topic53        topic60        topic17        topic48 
#65             65             65             81             89            136 
#topic43        topic14 
#188            346 
cloud(Q2C2.stm, topic = 14) # EU and public interests 
cloud(Q2C2.stm, topic = 43) # irrelv. addressed to topic
cloud(Q2C2.stm, topic = 48) # Eu council and parliament
cloud(Q2C2.stm, topic = 17) # irrelv. 
cloud(Q2C2.stm, topic = 60) # research and policy development
cloud(Q2C2.stm, topic = 53) # animal health and safety
cloud(Q2C2.stm, topic = 37) # finalncial institutions and payments/costs
cloud(Q2C2.stm, topic = 29) # fishing vessel and vessel managment
cloud(Q2C2.stm, topic = 30) # fisheries species
cloud(Q2C2.stm, topic = 11) # irrelevant. it is a time span/ date topic
cloud(Q2C2.stm, topic = 3)  # comittee proposals and policy
cloud(Q2C2.stm, topic = 47) # product market
cloud(Q2C2.stm, topic = 51) # no idea...
cloud(Q2C2.stm, topic = 54) # content and quality information
cloud(Q2C2.stm, topic = 35) # waste management
cloud(Q2C2.stm, topic = 56) # geographical location topic
cloud(Q2C2.stm, topic = 21) # Data protection, GDPDR
cloud(Q2C2.stm, topic = 57) # energy and emissions
cloud(Q2C2.stm, topic = 12) # processing and manufacturing
cloud(Q2C2.stm, topic = 44) # not really sure... pursuant (in accordance to...) 
cloud(Q2C2.stm, topic = 33) # employee/workers social characteristics
cloud(Q2C2.stm, topic = 20) # catch regulations 
cloud(Q2C2.stm, topic = 18) # fishing area managment
cloud(Q2C2.stm, topic = 31) # network and transport infrastructure
cloud(Q2C2.stm, topic = 16) # compund/solution sensitivities
cloud(Q2C2.stm, topic = 55) # Oil and gas extraction
cloud(Q2C2.stm, topic = 26) # programme financing and suport



     