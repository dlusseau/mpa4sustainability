
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("dplyr")
library("tibble")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------

mpa.policy.notext.df <- read.csv(file = "WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

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

citation_matrix <-
  as.matrix(citation_table)

str(citation_matrix)
# int [1:2919, 1:1094] 0 0 0 0 0 0 0 0 0 0 ...
#- attr(*, "dimnames")=List of 2
#..$ : chr [1:2919] "11951K098" "11957A031" "11957A097" "11957A101" ...
# the rownames are the citationcelexs

#..$ : chr [1:1094] "22021A0216(01)" "22021A0216(02)" "31966R0136" "31970R1526" ...
# the column names are the pulled eurlex documents
library("igraph")

colnames(citation_matrix) <- colnames(citation_matrix) 
rownames(citation_matrix) <- rownames(citation_matrix)

network <-  graph_from_incidence_matrix(citation_matrix)
plot(network)

