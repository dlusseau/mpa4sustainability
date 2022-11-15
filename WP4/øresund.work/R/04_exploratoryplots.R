

# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("dplyr")
library("ggplot2")
library("lubridate")
library("patchwork")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------


DK.metadata <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.DKdocmetadata.clean.csv")
SE.metadata <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.SEdocmetadata.clean.csv")

dk.labs <- c("fiskeri" ="fiskeri (n=1011)", "jagt"="jagt (n=346)", "søtrafik"="søtrafik (n=8)")

DK.metadata %>%
  group_by(search.term) %>%
  summarise(n=n_distinct(url))

DKplot <- 
  DK.metadata %>%
  group_by(search.term, År) %>%
  summarise(n=n_distinct(url)) %>%
  ungroup() %>%
  mutate(search.term = str_replace_all(search.term, "sotrafik", "søtrafik")) %>%
  mutate(search.term = as.factor(search.term)) %>%
  ggplot(aes(fill=search.term, y=n, x=År)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_x_continuous(breaks=seq(1886,2024,8), expand = c(0.02, .5)) +
  scale_y_continuous(breaks=seq(0,140,20), expand = c(0.02, .5)) + 
  theme_bw() +
  facet_wrap(~search.term,   nrow = 3,
             labeller = labeller(search.term = dk.labs))+
  ylab("Number of legislations") + 
  xlab(" ")+
  theme(legend.position = "bottom")+ 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        strip.text.x = element_text(face="bold", size = 12),
        title = element_text(face="bold", size = 12),
        legend.position = "none") + 
  labs(title = "Danish Legislation")

se.labs <- c("fiske" ="fiske (n=257)", "jakt"="jakt (n=93)", "sjöfart"="sjöfart (n=199)")

SE.metadata %>%
  group_by(search.term) %>%
  summarise(n=n_distinct(doc.id))

SEplot <- 
  SE.metadata %>%
  mutate(year=year(datum)) %>%
  mutate(search.term = as.factor(search.term)) %>%
  group_by(search.term, year) %>%
  summarise(n=n_distinct(doc.id)) %>%
  ungroup() %>%
  mutate(search.term = str_replace_all(search.term, "sjofart", "sjöfart")) %>%
  ggplot(aes(fill=search.term, y=n, x=year)) + 
  geom_bar(position="stack", stat="identity")  + 
  scale_x_continuous(breaks=seq(1798,2024,8), expand = c(0.001, .5)) +
  scale_y_continuous(breaks=seq(0,20,5), expand = c(0.001, .5)) +
  theme_bw() +
  facet_wrap(~search.term,   nrow = 3,
             labeller = labeller(search.term = se.labs))+
  ylab(" ") + 
  xlab(" ")+
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        strip.text.x = element_text(face="bold", size = 12),
        title = element_text(face="bold", size = 12),
        legend.position = "none") + 
  labs(title = "Swedish Legislation")


DKplot + SEplot + 
  plot_annotation(tag_levels = 'A')



 
DK.metadata %>%
  group_by(search.term, Ressort) %>%
  summarise(n=n_distinct(url)) %>%
  ungroup() %>%
  mutate(Ressort = as.factor(Ressort)) %>%
  ggplot(aes( y=n, x=Ressort)) + 
  geom_bar(position="stack", stat="identity") +
     facet_wrap(~search.term, ncol = 1) + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        strip.text.x = element_text(face="bold", size = 7),
        title = element_text(face="bold", size = 12),
        legend.position = "none")

SE.metadata %>%
  group_by(search.term, organ) %>%
  summarise(n=n_distinct(doc.id)) %>%
  ungroup() %>%
  mutate(organ = as.factor(organ)) %>%
  ggplot(aes( y=n, x=organ)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~search.term, ncol = 1) + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        strip.text.x = element_text(face="bold", size = 7),
        title = element_text(face="bold", size = 12),
        legend.position = "none")
