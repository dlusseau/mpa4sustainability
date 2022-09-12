
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
# 700 documents
#1094-394 = 700 so he math adds up 

citation.table.nozeros <-
  citation_table.nozeros  %>%
  pivot_wider(
    names_from = Var1,
    values_from = Freq) %>%
  column_to_rownames(.,  var = "Var2") 

citation_matrix.nozeros <-
  as.matrix(citation.table.nozeros)

str(citation_matrix.nozeros)
#  int [1:2919, 1:700] 0 0 0 0 0 0 0 0 0 0 ...
#- attr(*, "dimnames")=List of 2
#..$ : chr [1:2919] "11951K098" "11957A031" "11957A097" "11957A101" ...
# the rownames are the citationcelexs

#..$ : chr [1:700] "31972R0574" "31977D0415" "31977R2500" "31977R2714" ...
# the column names are the pulled eurlex documents
library("igraph")

colnames(citation_matrix.nozeros) <- colnames(citation_matrix.nozeros) 
rownames(citation_matrix.nozeros) <- rownames(citation_matrix.nozeros)

network <-  graph_from_incidence_matrix(citation_matrix.nozeros)
plot(network)

citation_info <- document.key.df[document.key.df$celex %in% citation.table$Var2,]
# O.K. we need to note that the all those cited are not legislation... 
# so we need to filet those one out or not depending on if we only are interested in legislative documents...
citation_info <-
  citation_info %>% 
  distinct(celex, .keep_all = TRUE)

network.attributes <-
  mpa.policy.notext.df %>%
  select(resource.type,CELEX,date,force) %>%
  distinct(CELEX,.keep_all = TRUE) %>%
  mutate(Document.pull = "eurlex.web")





# Mentioned documents: 
# habitats directive (31992L0043): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:31992L0043 (92/43/EEC)
# birds directive (32009L0147): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:32009L0147
# barcelona convention (21976A0216(01)): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=celex%3A21976A0216%2801%29
# ospar (21998A0403(01)): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=celex%3A21998A0403%2801%29
# cartagena convention (22002A0731(01)): https://eur-lex.europa.eu/legal-content/en/ALL/?uri=CELEX:22002A0731(01)
# helcom (52021PC0534): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:52021PC0534






