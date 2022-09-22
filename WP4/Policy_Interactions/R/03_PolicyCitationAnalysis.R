
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

# No defined function for this script

# Load data ---------------------------------------------------------------

mpa.policy.notext.df <- read.csv(file = "WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

document.key.df <- read.csv(file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

#MPA.textdata<- read.csv(file = "WP4/Policy_Interactions/data/01_CELEXmpa.text.data.csv")

#---------------------------------------------------------------------------
#------------- This is the analysis on the first search query  -------------
#----------------------- "marine protected " -------------------------------
#---------------------------------------------------------------------------

# Exploring document citations ------------------------------------------

# Document 32021R0092 is No longer in force: This act has been changed. Current consolidated version: 16/04/2022 
# the new version is not categories as one of the five legeslation types 
# so we will remove this instead of replace with the current version
mpa.policy.notext.df <-
  mpa.policy.notext.df %>%
  filter(CELEX != "32021R0092")

n_distinct(mpa.policy.notext.df$CELEX)
# 24

Doc.citations <-
  mpa.policy.notext.df %>%
  distinct(CELEX,citationcelex) %>% # make sure no duplicate rows bc of multiple labeles/themes
  filter(!is.na(citationcelex)) 

n_distinct(Doc.citations$CELEX)
# 19 document cite another document 
n_distinct(Doc.citations$citationcelex)
# 208 documents are cited


#Eurlex data/attributes about the citations

# NOTE: there is a total of 208 unique citation documents but only 134 have eurolex data associated... 
# this is bc our key is only leg. documents...
# Since we are only interested in legislative documents we will only keep citations that are legislation categorized 

leg.citation_info <- 
  document.key.df %>%
  filter(celex %in% Doc.citations$citationcelex) %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference")

Doc.citations <- 
  Doc.citations %>%
  filter(citationcelex %in% leg.citation_info$CELEX) 
  
citation.info <-  leg.citation_info

n_distinct(Doc.citations$CELEX)
# 2 documents cited but they were non-leg so they are now filtered out :)

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
# 3 documents pulled as an MPA leg are also cited by other legislation 
# These are:
# 32008L0056 --> Marine Strategy Framework Directive
#	32013R1380 --> CFP, amending CRs
#	32014R0508 --> European Maritime and Fisheries Fund & repealing CRs

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
#148
#dimentions add up bc 19+134=153, 153-8+4=149

Doc.citations <-
  Doc.citations %>%
  rename("to"="CELEX",
         "from"="citationcelex")

n_distinct(Doc.citations$to)
# 17
n_distinct(Doc.citations$from)
#  134
17+208
#225
docs <- unique(Doc.citations$to)
cit <-  unique(Doc.citations$from)
xx <- as.data.frame(c(docs,cit))
xx <- distinct(xx)

network <- graph_from_data_frame(d=Doc.citations, directed = TRUE, vertices = network.attributes.final)
print(network, e=TRUE, v=TRUE)

l <- layout.fruchterman.reingold(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

labels <- network.attributes.final[1:3,1]
labels <- c("Marine Strategy Framework Dir.",
            "Reg. on CFP, amending CRs",
            "Reg. European Maritime and Fisheries Fund & repealing CRs")

# 32008L0056 --> Marine Strategy Framework Directive
#	32013R1380 --> CFP, amending CRs
#	32014R0508 --> European Maritime and Fisheries Fund & repealing CRs

labels2 <- rep(NA,time=222)
labels3 <- c(labels,labels2)
V(network)$label <- labels3 


plot(network,
     edge.width=.5,
     vertex.size=3,
     vertex.label=NA,
     vertex.label.cex=1,
     edge.arrow.size=.5,
     edge.arrow.width=2,
     )
# blue are documents referenced within text
# green are those pulled from out MPA eurlex search
# red/pink are those that were pulled in the MPA search and also referenced within other documents pulled

# Second order citations ---------------------------------------------------------------------------

# lets find out what do the citations cite?
# OK to have a indirect citation network we need to make the edge list 
# We will need to make a new to|from df and then rbind them this will add the third layer

# Doc.citations what we will rbind to

indir.citation_info <- 
  Doc.citations %>%
  select(from) %>%
  left_join(.,document.key.df, by = c("from" = "celex"))

n_distinct(Doc.citations$from)
n_distinct(indir.citation_info$from)
# no changes :)

Doc.citations.2 <-
  indir.citation_info %>%
  distinct(from,citationcelex) %>% # make sure no duplicate rows bc of multiple labeles/themes
  filter(!is.na(citationcelex)) %>%
  rename("to" = "from",
         "from" = "citationcelex")

#now make sure they are only legislation documents 

leg.citation_info2 <- 
  document.key.df %>%
  filter(celex %in% Doc.citations.2$from) %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference2")

Doc.citations.2 <- 
  Doc.citations.2 %>%
  filter(from %in% leg.citation_info2$CELEX) 

citation.info2 <-  leg.citation_info2

#network.attributes2 <-
#  mpa.policy.notext.df %>%
#  filter(CELEX %in% Doc.citations.2$to) %>% 
#  select(resource.type,CELEX,date,force) %>%
#  distinct(CELEX,.keep_all = TRUE) %>%
#  mutate(pulled.from = "eurlex.web") %>%
#  rbind(.,citation.info2) # %>%

#both.pulls2 <-
 # network.attributes2 %>%
 # group_by(CELEX) %>%
 # summarise(n=n()) %>%
 # filter(n>1) 
# 3 documents pulled as an MPA leg are also cited by other legislation 
# These are the same as last time...
# 32008L0056 --> Marine Strategy Framework Directive
#	32013R1380 --> CFP, amending CRs
#	32014R0508 --> European Maritime and Fisheries Fund & repealing CRs

#network.attributes.both2 <- network.attributes2[network.attributes2$CELEX %in% both.pulls2$CELEX,]

#network.attributes.both2 <- 
 # network.attributes.both2 %>%
 # distinct(CELEX,.keep_all = TRUE) %>%
 # mutate(pulled.from= "both")

#network.attributes.notboth2 <- network.attributes2[!network.attributes2$CELEX %in% both.pulls2$CELEX,]

#network.attributes.final2 <-
#  rbind(network.attributes.both2,network.attributes.notboth2)

network.attributes.final2 <-
  citation.info2 %>%
  select(CELEX,resource.type,date,force,pulled.from) %>%
  mutate(color = 
           case_when(
             pulled.from == "reference2" ~ "purple" )) %>%
  mutate(shape = 
           case_when(
             resource.type == "DIR" ~ "circle",
             resource.type == "REG" ~ "circle",
             resource.type == "DEC" ~ "circle",
             resource.type == "RECO" ~ "circle",
             resource.type == "OPIN" ~ "circle",
             resource.type == "OTHER" ~ "square"  ))

n_distinct(network.attributes.final2$CELEX)
#515




Doc.citations3 <- rbind(Doc.citations,Doc.citations.2)

network.attributes.final3 <- rbind(network.attributes.final,network.attributes.final2)

network.attributes.final3 <- 
  network.attributes.final3 %>%
  distinct(CELEX, .keep_all = TRUE)

n_distinct(Doc.citations3$to)
# 118
n_distinct(Doc.citations3$from)
#  570
118+570
#688
docs <- unique(Doc.citations3$to)
cit <-  unique(Doc.citations3$from)
xx <- as.data.frame(c(docs,cit))
xx <- distinct(xx)

network <- graph.data.frame(d=Doc.citations3, directed = TRUE, vertices = network.attributes.final3)
print(network, e=TRUE, v=TRUE)

l <- layout.fruchterman.reingold(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

labels <- network.attributes.final3[1:3,1]
labels <- c("Marine Strategy Framework Dir.",
            "Reg. on CFP, amending CRs",
            "Reg. European Maritime and Fisheries Fund & repealing CRs")

# 32008L0056 --> Marine Strategy Framework Directive
#	32013R1380 --> CFP, amending CRs
#	32014R0508 --> European Maritime and Fisheries Fund & repealing CRs

labels2 <- rep(NA,time=688)
labels3 <- c(labels,labels2)
V(network)$label <- labels3 

plot(network,
     edge.width=.5,
     vertex.size=2,
     vertex.label=NA,
     vertex.label.cex=1,
     edge.arrow.size=.5,
     edge.arrow.width=1,
     layout=l
)

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


