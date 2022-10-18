
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
         "to" = "EU.link.CELEX")
  

n_distinct(DKEUlinks1$url)
# 445 dk documents link to an EU doc
n_distinct(DKEUlinks1$EU.link.CELEX)
# 235 legal acts link to a dk document


eu.vertices <- 
  DKEUlinks%>%
  select(-url,-search.term,-harpun,-kommercielt,-erhvervsmæssigt,-erhvervs,-rekreativt,-sæl,-fugle) %>%
  distinct(EU.link.CELEX) %>%
  rename("vertices" = "EU.link.CELEX")

dk.vertices <- 
  DKEUlinks%>%
  select(url,search.term,harpun,kommercielt,erhvervsmæssigt,erhvervs,rekreativt,sæl,fugle) %>%
  distinct(url) %>%
  mutate(url = str_replace_all(url, "https://www.retsinformation.dk","")) %>%
  rename("vertices" = "url")

vertices <- rbind(dk.vertices,eu.vertices)

network <- graph_from_data_frame(d=DKEUlinks1, directed = FALSE, vertices = vertices)
print(network, e=TRUE, v=TRUE)


actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))



