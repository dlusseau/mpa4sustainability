

# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("dplyr")
library("stringr")
library("ggplot")
library("patchwork")
library("ggwordcloud")           

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------

DKEUlinks <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.DKEUlinks.csv")
SEEUlinks <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02SE.EU.links.csv")

# HUNTING -------------------

# seal hunting word clouds
# term df

head(DKEUlinks)

DK.huntinglabels.seals <-
  DKEUlinks %>%
  filter(search.term == "jagt") %>%
  distinct(url,EU.link.CELEX, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(sæl == "TRUE" | 
           sæl2 == "TRUE"  & 
           sæl3 == "TRUE"  | 
           sæl4 == "TRUE") %>%
  filter(!is.na(EU.link.CELEX)) %>%
  group_by(labels) %>%
  summarise(n.doc = n_distinct(url)) %>%
  mutate(hunting = "seal")%>%
  mutate(country = "dk")

DKEUlinks %>%
  filter(search.term == "jagt") %>%
  distinct(url,EU.link.CELEX, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(sæl == "TRUE" | 
           sæl2 == "TRUE"  & 
           sæl3 == "TRUE"  | 
           sæl4 == "TRUE") %>%
  filter(!is.na(EU.link.CELEX)) %>%
  summarise(n=n_distinct(EU.link.CELEX))

DK.huntinglabels.bird <-
  DKEUlinks %>%
  filter(search.term == "jagt") %>%
  distinct(url,EU.link.CELEX, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(fugle == "TRUE") %>%
  filter(!is.na(EU.link.CELEX)) %>%
  group_by(labels) %>%
  summarise(n.doc = n_distinct(url)) %>%
  mutate(hunting = "bird")%>%
  mutate(country = "dk")

head(SEEUlinks)


SE.huntinglabels.seals <-
  SEEUlinks %>%
  filter(search.term == "jakt") %>%
  distinct(doc.id,celex, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(seal == "TRUE" | 
         seal2 == "TRUE" |
         seal3 == "TRUE"|
         seal3 == "TRUE") %>%
  filter(!is.na(celex)) %>%
  group_by(labels) %>%
  summarise(n.doc = n_distinct(doc.id)) %>%
  mutate(hunting = "seal")%>%
  mutate(country = "se")

SE.huntinglabels.bird <-
  SEEUlinks %>%
  filter(search.term == "jakt") %>%
  distinct(doc.id,celex, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(birdhunt == "TRUE") %>%
  filter(!is.na(celex)) %>%
  group_by(labels) %>%
  summarise(n.doc = n_distinct(doc.id)) %>%
  mutate(hunting = "bird")%>%
  mutate(country = "se")


seal.hunting.labels <- rbind(DK.huntinglabels.seals, SE.huntinglabels.seals)
bird.hunting.labels <- rbind(DK.huntinglabels.bird, SE.huntinglabels.bird)


cluster.labs <- c(
  "dk"="Denmark", 
  "se"="Sweden"
  )


seal.word.cloud <- 
  seal.hunting.labels %>%
  group_by(hunting,country) %>%
  slice_max(n.doc, n=20) %>%
ggplot(.,
  aes(
    label = labels, size = n.doc,
    x = country, color = country  )
) +
  geom_text_wordcloud_area(show.legend = TRUE) +
  scale_size_area(max_size = 30) +
  theme_minimal() +
  facet_wrap(~country, ncol = 2, labeller = labeller(country = cluster.labs)) +
  scale_color_manual("", 
                     breaks = c("dk", "se"),
                     values=c(dk="#d1050c", se ="#004B87")) +
  ggtitle("(A) Eurovoc descriptors for legislations which mentons seal") +
  guides(size="none") +
  theme(line = element_blank(),
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_blank(),
      #  legend.key.size = uelement_blank(),
        strip.text.x = element_text(face="bold", size = 20),
        legend.position = "none",
      title = element_text(face="bold", size = 25),
                           plot.title = element_text(hjust = 0.5))
seal.word.cloud

cluster.labs <- c(
  "dk"="", 
  "se"=""
)

bird.word.cloud <- 
  bird.hunting.labels %>%
  group_by(hunting,country) %>%
  slice_max(n.doc, n=20) %>%
  ggplot(.,
         aes(
           label = labels, size = n.doc,
           x = country, color = country  )
  ) +
  geom_text_wordcloud_area(show.legend = TRUE) +
  scale_size_area(max_size = 30) +
  theme_minimal() +
  facet_wrap(~country, ncol = 2, labeller = labeller(country = cluster.labs)) +
  scale_color_manual("", 
                     breaks = c("dk", "se"),
                     values=c(dk="#d1050c", se ="#004B87")) +
  ggtitle("(B) Eurovoc descriptors for legislations which mentons bird") +
  guides(size="none") +
  theme(line = element_blank(),
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_blank(),
        #  legend.key.size = uelement_blank(),
        strip.text.x = element_text(face="bold", size = 20),
        legend.position = "none",
        title = element_text(face="bold", size = 25),
        plot.title = element_text(hjust = 0.5))
bird.word.cloud


seal.word.cloud/bird.word.cloud
ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/hunting.wordclouds.png", 
       width = 90, height = 55, units = "cm",
       limitsize = FALSE)

# FISHING -------------------

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
  summarise(n.doc = n_distinct(url)) %>%
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
  summarise(n.doc = n_distinct(url)) %>%
  mutate(fisheries = "recreational")%>%
  mutate(country = "dk")


  
SE.fishinglabels.com <-
  SEEUlinks %>%
  filter(search.term == "fiske") %>%
  distinct(doc.id,celex, labels, .keep_all= TRUE) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  filter(commercial == "TRUE") %>%
  filter(!is.na(celex)) %>%
  group_by(labels) %>%
  summarise(n.doc = n_distinct(doc.id)) %>%
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
  summarise(n.doc = n_distinct(doc.id)) %>%
  mutate(fisheries = "recreational")%>%
  mutate(country = "se")


spec.fisheries.labels <- rbind(DK.fishinglabels.com, DK.fishinglabels.rec, SE.fishinglabels.com, SE.fishinglabels.rec)

cluster.labs <- c(
  "commercial"="(A) Commercial fisheries", 
  "recreational"="(B) Recreational fisheries"
)

spec.fisheries.labels %>%
  group_by(fisheries,country) %>%
  slice_max(n.doc, n=15) %>%
  ggplot(.,
         aes(
           label = labels, size = n.doc,
           x = country, color = country  )
  ) +
  geom_text_wordcloud_area(show.legend = TRUE) +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~fisheries, nrow = 2,labeller = labeller(fisheries = cluster.labs)) + #labeller = labeller(hunting = cluster.labs)) +
  scale_color_manual("", 
                     breaks = c("dk", "se"),
                     values=c(dk="#d1050c", se ="#004B87")) +
  guides(size="none") +
  theme(line = element_blank(),
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(4, 'line'),
        strip.text.x = element_text(face="bold", size = 25),
        legend.position = "none")

ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/fisheries.wordclouds.png", 
       width = 80, height = 55, units = "cm",
       limitsize = FALSE)




