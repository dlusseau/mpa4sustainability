
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("dplyr")
library("tibble")
library("lubridate")
library("tidyr")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------

mpa.policy.notext.df <- read.csv(file = "WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")
document.key.df <- read.csv(file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

# Exploring document referencing ------------------------------------------

Dist.citations <-
  mpa.policy.notext.df %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  distinct(CELEX,citationcelex)

citation.table <- as.data.frame(table(Dist.citations$CELEX,Dist.citations$citationcelex))

# Here we will create an incidence matrix:
citation_table <-
  citation.table  %>%
  pivot_wider(
    names_from = Var1,
    values_from = Freq) %>%
  column_to_rownames(.,  var = "Var2") 
  
zero_cit <-
  citation_table %>%
  colSums() %>%
  as.data.frame() %>%
  rename("cit.nmbr" = ".") %>%
  filter(cit.nmbr == 0) %>%
  rownames_to_column(var = "CELEX")

#filter out those that cite zero documents:
citation_table.nozeros <- citation.table[!citation.table$Var1 %in% zero_cit$CELEX,]

n_distinct(citation_table.nozeros$Var1)
# 705 documents
n_distinct(citation.table$Var1)
n_distinct(zero_cit$CELEX)
#1104-399 = 705 so he math adds up 

#filter out those that cite zero documents:
citation_table.nozeros <- citation.table[!citation.table$Var1 %in% zero_cit$CELEX,]



citation_info <- document.key.df[document.key.df$celex %in% citation.table$Var2,]
# O.K. we need to note that the all those cited are not legislation... 
# so we need to filet those one out or not depending on if we only are interested in legislative documents...
citation_info <-
  citation_info %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference")
  
network.attributes <-
  mpa.policy.notext.df %>%
  select(resource.type,CELEX,date,force) %>%
  distinct(CELEX,.keep_all = TRUE) %>%
  mutate(pulled.from = "eurlex.web") %>%
  rbind(.,citation_info)

both.pulls <-
  network.attributes %>%
  group_by(CELEX) %>%
  summarise(n=n()) %>%
  filter(n>1)

network.attributes.both <- network.attributes[network.attributes$CELEX %in% both.pulls$CELEX,]

network.attributes.both <- 
  network.attributes.both %>%
  distinct(CELEX,.keep_all = TRUE) %>%
  mutate(pulled.from= "both")

network.attributes.notboth <- network.attributes[!network.attributes$CELEX %in% both.pulls$CELEX,]

network.attributes.final <-
  rbind(network.attributes.both,network.attributes.notboth)


# now make a matrix with 0 referencing filtered out and non/legislation documents filtered out:
citation_table.nozeros.leg <- citation_table.nozeros[citation_table.nozeros$Var2 %in% network.attributes.final$CELEX,]


citation_table.nozeros.leg1 <-
  citation_table.nozeros.leg  %>%
  pivot_wider(
    names_from = Var1,
    values_from = Freq) %>%
  column_to_rownames(.,  var = "Var2") 

citation_matrix.nozeros.leg2 <-
  as.matrix(citation_table.nozeros.leg1)

str(citation_matrix.nozeros.leg2)
# the rownames are the citationcelexs
# the column names are the pulled eurlex documents


library("igraph")

colnames(citation_matrix.nozeros.leg2) <- colnames(citation_matrix.nozeros.leg2) 
rownames(citation_matrix.nozeros.leg2) <- rownames(citation_matrix.nozeros.leg2)

network <-  graph_from_incidence_matrix(citation_matrix.nozeros.leg2)
plot(network)


# Mentioned documents within EU MPA designations: 
# habitats directive (31992L0043): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:31992L0043 (92/43/EEC)
# birds directive (32009L0147): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:32009L0147
# barcelona convention (21976A0216(01)): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=celex%3A21976A0216%2801%29
# ospar (21998A0403(01)): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=celex%3A21998A0403%2801%29
# cartagena convention (22002A0731(01)): https://eur-lex.europa.eu/legal-content/en/ALL/?uri=CELEX:22002A0731(01)
# helcom (52021PC0534): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:52021PC0534

MPA.desigCELEX <- c("31992L0043",
                    "32009L0147",
                    "21976A0216(01)",
                    "21998A0403(01)",
                    "22002A0731(01)",
                    "52021PC0534")

MPA.desigName <- c("habitats directive",
                    "birds directive",
                    "barcelona convention",
                    "ospar",
                    "cartagena convention",
                    "helcom")

Desig.Celex <- tibble(
  CELEX = MPA.desigCELEX,
  Name = MPA.desigName
)


testing <- 
  citation_table.nozeros.leg[citation_table.nozeros.leg$Var2 %in% Desig.Celex$CELEX,]

testing <- 
  testing %>%
  group_by(Var1) %>%
  mutate(total = sum(Freq)) %>%
  filter(total>0) %>%
  select(-total)

testing <-
  testing  %>%
  pivot_wider(
    names_from = Var1,
    values_from = Freq) %>%
  column_to_rownames(.,  var = "Var2") 

testing2 <-
  as.matrix(testing)

str(testing2)
# the rownames are the citationcelexs
# the column names are the pulled eurlex documents


library("igraph")

colnames(testing2) <- colnames(testing2) 
rownames(testing2) <- rownames(testing2)

network <-  graph_from_incidence_matrix(testing2)
plot(network)


