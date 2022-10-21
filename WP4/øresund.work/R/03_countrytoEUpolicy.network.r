
# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("dplyr")
library("stringr")
library("igraph")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------

DKEUlinks <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.DKEUlinks.csv")

# DK to EU document network ----------------------------------------------------

DKEUlinks1 <-
  DKEUlinks %>%
  distinct(url,EU.link.CELEX) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  mutate(url = str_replace_all(url, "https://www.retsinformation.dk","")) %>% # just to make the id url shorter
  rename("from" = "url",
         "to" = "EU.link.CELEX") %>%
  filter(!is.na(to))    # remove documents that dont link to an EU CELEX
  

n_distinct(DKEUlinks1$from)
# 206 dk documents link to an EU doc
n_distinct(DKEUlinks1$to)
# 234 EU legal acts link 


eu.vertices <- 
  DKEUlinks1%>%
 # select(-url,-search.term,-harpun,-kommercielt,-erhvervsmæssigt,-erhvervs,-rekreativt,-sæl,-fugle) %>%
  distinct(to) %>%
  rename("vertices" = "to") %>%
  mutate(source = "EU")

dk.vertices <- 
  DKEUlinks1%>%
  #select(url,search.term,harpun,kommercielt,erhvervsmæssigt,erhvervs,rekreativt,sæl,fugle) %>%
  distinct(from) %>%
 # mutate(url = str_replace_all(url, "https://www.retsinformation.dk","")) %>%
  rename("vertices" = "from")%>%
  mutate(source = "DK")

vertices <- rbind(dk.vertices,eu.vertices)

network <- graph_from_data_frame(d=DKEUlinks1, directed = FALSE, vertices = vertices)
print(network, e=TRUE, v=TRUE)

library(RColorBrewer)
col  <- brewer.pal(3, "Set2") 
col <- col[-1]
V(network)$color <- col[as.numeric(as.factor(V(network)$source))]
# DK doc are the red ones...
degree <- degree(network)

l <- layout.fruchterman.reingold(network)
plot(network,
     vertex.label=NA,
     vertex.size= 2,
     layout = l)

sort(degree(network), decreasing = TRUE)
