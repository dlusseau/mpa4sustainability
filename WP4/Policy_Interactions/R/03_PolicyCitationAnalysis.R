
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("dplyr")
library("tibble")
library("lubridate")
library("tidyr")
library("stringr")
library("igraph")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------

mpa.policy.notext.df <- read.csv(file = "WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

document.key.df <- read.csv(file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

#MPA.textdata<- read.csv(file = "WP4/Policy_Interactions/data/01_CELEXmpa.text.data.csv")


# Exploring document citations ------------------------------------------

# (1) We want a matrix of the MPA documents (rows) v.s. documents they cite (columns)

Doc.citations <-
  mpa.policy.notext.df %>%
  distinct(CELEX,citationcelex) %>% # make sure no duplicate rows bc of multiple labeles/themes
  filter(!is.na(citationcelex)) 

n_distinct(Doc.citations$CELEX)
# [1] 933
n_distinct(Doc.citations$citationcelex)
# [1] 3222


#Eurlex data/attributes about the citations

# NOTE: there is a total of 3,223 unique citation documents but only 1959 have eurolex data associated... this is bc our key is only leg. documents...
# so depending on if we only are interested in legislative documents or not we should or shouldnt remove them
# for now keeping all of the citations regardless of the type of document 
# I will add them into this DF but will put resource.type as non-leg and NA for other fields. 

leg.citation_info <- 
  document.key.df %>%
  filter(celex %in% Doc.citations$citationcelex) %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference")


nonleg.citation_info <- 
  Doc.citations %>%
  distinct(citationcelex) %>%
  anti_join(.,leg.citation_info, by=c("citationcelex" = "CELEX"))%>% # joining those that are non-leg
   mutate(date = NA,
         force = NA,
         resource.type = "OTHER", 
         pulled.from = "reference")%>%
  rename(CELEX = citationcelex)  %>%
  select(resource.type,CELEX,date,force,pulled.from)


citation.info <-  rbind(nonleg.citation_info,leg.citation_info) #combine non.leg with the leg data 

network.attributes <-
  mpa.policy.notext.df %>%
  filter(CELEX %in% Doc.citations$CELEX) %>% 
  select(resource.type,CELEX,date,force) %>%
  distinct(CELEX,.keep_all = TRUE) %>%
  mutate(pulled.from = "eurlex.web") %>%
  rbind(.,citation.info) # %>%

both.pulls <-
  network.attributes %>%
  group_by(CELEX) %>%
  summarise(n=n()) %>%
  filter(n>1) 
# documents pulled as an MPA leg and cited within others

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
  select(CELEX,resource.type,date,force,pulled.from) %>%
  mutate(color = 
           case_when(
             pulled.from == "eurlex.web" ~ "#0cb702",
             pulled.from == "both" ~ "#f8766d",
             pulled.from == "reference" ~ "#00a9ff" )) %>%
  mutate(shape = 
           case_when(
             resource.type == "DIR" ~ "circle",
             resource.type == "REG" ~ "circle",
             resource.type == "DEC" ~ "circle",
             resource.type == "RECO" ~ "circle",
             resource.type == "OPIN" ~ "circle",
             resource.type == "OTHER" ~ "square"  ))

n_distinct(network.attributes.final$CELEX)


Doc.citations <-
  Doc.citations %>%
  rename("to"="CELEX",
         "from"="citationcelex")

n_distinct(Doc.citations$to)
# 933 
n_distinct(Doc.citations$from)
# 3,222  
933+3222
#4155
docs <- unique(Doc.citations$to)
cit <-  unique(Doc.citations$from)
xx <- as.data.frame(c(docs,cit))
xx <- distinct(xx)

network <- graph_from_data_frame(d=Doc.citations, directed = TRUE, vertices = network.attributes.final)
print(network, e=TRUE, v=TRUE)

l <- layout.fruchterman.reingold(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

#labels <- network.attributes.final[1:4,1]
#labels <- c("Marine Strategy Framework Dir.","Reg. on European Maritime and Fisheries Fund",
#"Reg. on the CFP", "Opinion on an integrated EU policy for the Arctic")

#labels2 <- rep(NA,time=224)
#labels3 <- c(labels,labels2)
#V(network)$label <- labels3 


plot(network,
     edge.width=.5,
     vertex.size=3,
     vertex.label=NA,
     vertex.label.cex=1,
     edge.arrow.size=.5,
     edge.arrow.width=.5,
     )
# blue are documents referenced within text
# green are those pulled from out MPA eurlex search
# red/pink are those that were pulled in the MPA search and also referenced within other documents pulled
  # Marine Strategy Framework Directive (32008L0056) 
  # Regulation on European Maritime and Fisheries Fund and repealing Council Regulations (32014R0508)
  # Regulation on the Common Fisheries Policy, amending Council Regulations (32013R1380)
  # Opinion on An integrated European Union policy for the Arctic (52016AE4426)

# total 25 documents were pulled from eurlex web 

# Second order citations ---------------------------------------------------------------------------

# lets find out what do the citations cite?





# Archival code ---------------------------------------

# (Maybe we want to do this but for now will keep them in)
#filter out those that cite zero documents:
#citation_table.nozeros <- citation.table[!citation.table$Var1 %in% zero_cit$CELEX,]
#n_distinct(citation_table.nozeros$Var1)
#n_distinct(citation.table$Var1)
#n_distinct(zero_cit$CELEX)
#filter out those that cite zero documents:
#citation_table.nozeros <- citation.table[!citation.table$Var1 %in% zero_cit$CELEX,]




# Lets create an incidence matrix:
#citation_matrix <-
#  citation.table  %>%
#  pivot_wider(
#    names_from = from,
#    values_from = Freq) %>%
#  column_to_rownames(.,  var = "to") 

#plot(graph_from_incidence_matrix(citation_matrix))


#zero_cit <-
#  citation_matrix %>%
# rowSums() %>%
# as.data.frame() %>%
# rename("cit.nmbr" = ".") %>%
#  filter(cit.nmbr == 0) %>%
#  rownames_to_column(var = "CELEX")
# These documents have no citations

#citation.table <- as.data.frame(table(Doc.citations$CELEX,Doc.citations$citationcelex))

#citation.table <-
#  citation.table %>%
#  rename("to"="Var1",
#         "from"="Var2")


