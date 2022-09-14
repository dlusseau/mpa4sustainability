
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

# (1) We want a matrix of the MPA documents (rows) v.s. documents they cite (columns)

# Note: the citation data from eurlex does not provide how many times the document was referenced/cited within the document.
# Only that the document was cited (thus citation frequency is 1)

Doc.citations <-
  mpa.policy.notext.df %>%
  filter(!is.na(work)) %>% #filtering out the document that is an issue (all data missing) --> reference 02.Rscript
  distinct(CELEX,citationcelex) # make sure no duplicate rows bc of multiple labeles/themes

citation.table <- as.data.frame(table(Doc.citations$CELEX,Doc.citations$citationcelex))

citation.table <-
  citation.table %>%
  rename("Document"="Var1",
         "Citation"="Var2")

# Lets create an incidence matrix:
citation_matrix <-
  citation.table  %>%
  pivot_wider(
    names_from = Citation,
    values_from = Freq) %>%
  column_to_rownames(.,  var = "Document") 
  
zero_cit <-
  citation_matrix %>%
  rowSums() %>%
  as.data.frame() %>%
  rename("cit.nmbr" = ".") %>%
  filter(cit.nmbr == 0) %>%
  rownames_to_column(var = "CELEX")
# 5 documents have no citations

#Eurlex data/attributes about the citations
citation_info <- document.key.df[document.key.df$celex %in% citation.table$Citation,]

citation_info <-
  citation_info %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference")
# NOTE: there is a total of 208 citation documents but only 134 have eurolex data associated... this is bc our key is only leg. documents...
# so depending on if we only are interested in legislative documents or not we should or shouldnt remove them
# for now keeping all of the citations regardless of the type of document 
# I will add them into this DF but will put resource.type as non-leg and NA for other fields. 

citation_info1 <- 
  citation.table %>%
  distinct(Citation) %>%
  anti_join(.,citation_info, by=c("Citation"="CELEX"))%>%
  mutate(date = NA,
         force = NA,
         resource.type = "OTHER", 
         pulled.from = "reference") %>% 
  rename("CELEX" = "Citation") %>%
  rbind(.,citation_info) #combine non.leg with the leg data 
#hmm seems like celex numbers that start with a 5 are here but opinions are 5 aswell....
  
network.attributes <-
  mpa.policy.notext.df %>%
  select(resource.type,CELEX,date,force) %>%
  distinct(CELEX,.keep_all = TRUE) %>%
  mutate(pulled.from = "eurlex.web") %>%
  rbind(.,citation_info1)

both.pulls <-
  network.attributes %>%
  group_by(CELEX) %>%
  summarise(n=n()) %>%
  filter(n>1) 
# 4 documents were pulled as an MPA leg and cited within others

network.attributes.both <- network.attributes[network.attributes$CELEX %in% both.pulls$CELEX,]

network.attributes.both <- 
  network.attributes.both %>%
  distinct(CELEX,.keep_all = TRUE) %>%
  mutate(pulled.from= "both")

network.attributes.notboth <- network.attributes[!network.attributes$CELEX %in% both.pulls$CELEX,]

network.attributes.final <-
  rbind(network.attributes.both,network.attributes.notboth)

network.attributes.final <-
  network.attributes.final %>%
  select(CELEX,resource.type,date,force,pulled.from)

network <- graph_from_data_frame(d=citation.table, vertices = network.attributes.final, directed = FALSE)

V(network)$color <- V(network)$pulled.from

plot(network)
#hmm this looks wrong

library("igraph")

colnames(citation_matrix) <- colnames(citation_matrix) 
rownames(citation_matrix) <- rownames(citation_matrix)

network <-  graph_from_incidence_matrix(citation_matrix, 
                                        directed = TRUE,
                                        mode = c("out"))
plot(network,
     vertex.size=10,
     vertex.label.cex=.5,
     )


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


# Archival code ---------------------------------------

# (Maybe we want to do this but for now will keep them in)
#filter out those that cite zero documents:
#citation_table.nozeros <- citation.table[!citation.table$Var1 %in% zero_cit$CELEX,]
#n_distinct(citation_table.nozeros$Var1)
#n_distinct(citation.table$Var1)
#n_distinct(zero_cit$CELEX)
#filter out those that cite zero documents:
#citation_table.nozeros <- citation.table[!citation.table$Var1 %in% zero_cit$CELEX,]




