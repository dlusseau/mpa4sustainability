

# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("dplyr")
library("stringr")
library("igraph")
library("patchwork")
library("viridis")           
require("graphics")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------

DKEUlinks <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.DKEUlinks.csv")
SEEUlinks <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02SE.EU.links.csv")




# seal hunting word clouds
# term df
DK.huntinglabels.seals <-
  DKEUlinks %>%
  filter(search.term == "jagt") %>%
  distinct(url,EU.link.CELEX, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(sæl == "TRUE") %>%
  filter(!is.na(EU.link.CELEX)) %>%
  group_by(labels) %>%
  summarise(n.docs = n_distinct(url)) %>%
  mutate(hunting = "seal")%>%
  mutate(country = "dk")

DK.huntinglabels.bird <-
  DKEUlinks %>%
  filter(search.term == "jagt") %>%
  distinct(url,EU.link.CELEX, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(fugle == "TRUE") %>%
  filter(!is.na(EU.link.CELEX)) %>%
  group_by(labels) %>%
  summarise(n.docs = n_distinct(url)) %>%
  mutate(hunting = "bird")%>%
  mutate(country = "dk")

SE.huntinglabels.seals <-
  SEEUlinks %>%
  filter(search.term == "jakt") %>%
  distinct(doc.id,celex, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(seal == "TRUE") %>%
  filter(!is.na(celex)) %>%
  group_by(labels) %>%
  summarise(n.docs = n_distinct(doc.id)) %>%
  mutate(hunting = "seal")%>%
  mutate(country = "se")

SE.huntinglabels.bird <-
  SEEUlinks %>%
  filter(search.term == "jakt") %>%
  distinct(doc.id,celex, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(birdhunt == "TRUE") %>%
  filter(!is.na(celex)) %>%
  group_by(labels) %>%
  summarise(n.docs = n_distinct(doc.id)) %>%
  mutate(hunting = "bird")%>%
  mutate(country = "se")


spec.hunting.labels <- rbind(DK.huntinglabels.seals, DK.huntinglabels.bird, SE.huntinglabels.bird, SE.huntinglabels.seals)



cluster.labs <- c(
  "bird"="Eurovoc descriptors for legislation which mentons bird", 
  "seal"="Eurovoc descriptors for legislation which mentons seal"
  )


spec.hunting.labels %>%
  group_by(hunting,country) %>%
  slice_max(n.docs, n=20) %>%
ggplot(.,
  aes(
    label = labels, size = n.docs,
    x = country, color = country  )
) +
  geom_text_wordcloud_area(show.legend = TRUE) +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~hunting, ncol = 2, labeller = labeller(hunting = cluster.labs)) +
  scale_color_manual("", 
                     breaks = c("dk", "se"),
                     values=c(dk="#d1050c", se ="#004B87")) +
  guides(size="none") +
  theme(line = element_blank(),
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(4, 'line'),
        strip.text.x = element_text(face="bold", size = 12),
        legend.position = "bottom")
  




# fisheries 
# term df
DK.fishinglabels.com <-
  DKEUlinks %>%
  filter(search.term == "fiskeri") %>%
  distinct(url,EU.link.CELEX, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(kommercielt == "TRUE" |
           erhvervsmæssigt == " TRUE" | 
           erhvervs== "TRUE") %>%
  filter(!is.na(EU.link.CELEX)) %>%
  group_by(labels) %>%
  summarise(n.docs = n_distinct(url)) %>%
  mutate(fisheries = "commercial")%>%
  mutate(country = "dk")

DK.fishinglabels.rec <-
  DKEUlinks %>%
  filter(search.term == "fiskeri") %>%
  distinct(url,EU.link.CELEX, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(rekreativt == "TRUE" | 
           rekreative == "TRUE" |
           lystfiske == "TRUE" |
           fritidsfiske == "TRUE") %>%
  filter(!is.na(EU.link.CELEX)) %>%
  group_by(labels) %>%
  summarise(n.docs = n_distinct(url)) %>%
  mutate(fisheries = "recreational")%>%
  mutate(country = "dk")


  
SE.fishinglabels.com <-
  SEEUlinks %>%
  filter(search.term == "fiske") %>%
  distinct(doc.id,celex, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(commercial == "TRUE") %>%
  filter(!is.na(celex)) %>%
  group_by(labels) %>%
  summarise(n.docs = n_distinct(doc.id)) %>%
  mutate(fisheries = "commercial")%>%
  mutate(country = "se")


SE.fishinglabels.rec <-
  SEEUlinks %>%
  filter(search.term == "fiske") %>%
  distinct(doc.id,celex, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(recreational == "TRUE" |
           angling == "TRUE" |
           angling2 == "TRUE" |
           houseneeds.fishing == "TRUE" |
           houseneeds.fishing2 == "TRUE") %>%
  filter(!is.na(celex)) %>%
  group_by(labels) %>%
  summarise(n.docs = n_distinct(doc.id)) %>%
  mutate(fisheries = "recreational")%>%
  mutate(country = "se")


spec.fisheries.labels <- rbind(DK.fishinglabels.com, DK.fishinglabels.rec, SE.fishinglabels.com, SE.fishinglabels.rec)

spec.fisheries.labels %>%
 # filter(country == "dk") %>%
ggplot(. ,
  aes(
    label = labels, size = n.docs,
    x = fisheries, color = fisheries  )
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~country, nrow = 2)



