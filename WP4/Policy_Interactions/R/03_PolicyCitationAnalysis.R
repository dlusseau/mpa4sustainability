
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

MPA.textdata<- read.csv(file = "WP4/Policy_Interactions/data/01_CELEXmpa.text.data.csv")

EU.mpa.char<- read.csv(file = "WP4/Policy_Interactions/data/01_EU.mpachar.csv")

# Exploring document citations ------------------------------------------

# (1) We want a matrix of the MPA documents (rows) v.s. documents they cite (columns)

# Note: the citation data from eurlex does not provide how many times the document was referenced/cited within the document.
# Only that the document was cited (thus citation frequency is 1)

Doc.citations <-
  mpa.policy.notext.df %>%
  filter(CELEX !="32021R0092") %>%
  filter(!is.na(work)) %>% #filtering out the document that is an issue (all data missing) --> reference 02.Rscript
  distinct(CELEX,citationcelex) # make sure no duplicate rows bc of multiple labeles/themes

citation.table <- as.data.frame(table(Doc.citations$CELEX,Doc.citations$citationcelex))

citation.table <-
  citation.table %>%
  rename("to"="Var1",
         "from"="Var2")

# Lets create an incidence matrix:
citation_matrix <-
  citation.table  %>%
  pivot_wider(
    names_from = from,
    values_from = Freq) %>%
  column_to_rownames(.,  var = "to") 
  
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
  distinct(from) %>%
  anti_join(.,citation_info, by=c("from"="CELEX"))%>%
  mutate(date = NA,
         force = NA,
         resource.type = "OTHER", 
         pulled.from = "reference") %>% 
  rename("CELEX" = "from") %>%
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
  filter(CELEX !="32021R0092") %>%
  select(CELEX,resource.type,date,force,pulled.from) %>%
  mutate(color = 
           case_when(
             pulled.from == "eurlex.web" ~ "#0cb702",
             pulled.from == "both" ~ "#f8766d",
             pulled.from == "reference" ~ "#00a9ff" ))

citation.table.xx <- 
  citation.table %>%
  filter(Freq>0)

n_distinct(citation.table.xx$to)
n_distinct(citation.table.xx$from)
19+208

network <- graph_from_data_frame(d=citation.table.xx, directed = TRUE, vertices = network.attributes.final)
print(network, e=TRUE, v=TRUE)

l <- layout_nicely(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(network,
     edge.width=.5,
     vertex.size=7,
     vertex.label=NA,
    # vertex.label.cex=1,
     edge.arrow.size=.75,
     edge.arrow.width=.75,
     rescale=F,
     layout=l*1.2, # trying this layout based on pdf above...
     )
# blue are documents referenced within text
# green are those pulled from out MPA eurlex search
# red/pink are those that were pulled in the MPA search and also referenced within other documents pulled

# Exploring MPA designation types referenced in EU text ------------------------------------------

# OK THIS IS THE SECOND PART SPECIFICALLY LOOKING AT THE TEXT...

mpa.DES.key <- 
  EU.mpa.char %>%
  select(mpa,DESIG_ENG)

# First lets search for the specific documents in mention:

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

# lets pull out the documents that specifically reference these above
doc.referenced <-
  citation.table.xx %>% # to-from citation table built above
  filter(from %in% Desig.Celex$CELEX) %>%
  left_join(.,Desig.Celex, by=c("from"="CELEX")) %>%
  select(to,from,Name) %>%
  #mutate(Name. = Name) %>%
  rename("from.name" = "Name")
 # select(to,from,Name)
  
  
# total search terms:
search_terms <- c("ramsar site", 
                  "wetland of international importance",
                  "world heritage site (natural or mixed)", 
                  "unesco-mab biosphere reserve",
                  "specially protected areas of mediterranean importance",
                  "barcelona convention",
                  "sites of community importance",
                  "habitats directive",
                  "special areas of conservation",
                  "habitats directive",
                  "special protection area",
                  "birds directive",
                  "baltic sea protected area",
                  "helcom",                                          
               # "marine protected area", --> removing this one bc this was the original search term...
                  "ospar",
                  "specially protected area",
                  "cartagena convention")

text.ref.list<-list()

for (i in 1:length(search_terms)) {  
  text.ref.list[[i]]<- 
    MPA.textdata %>%
    select(-c(title,text)) %>%
    mutate(references = str_extract_all(total.text, search_terms[[i]])) %>%
    select(-c(total.text))
}

# Lets give each list the name based on the resource type: 
text.ref.list <- structure(text.ref.list, names=search_terms)

text.df <- bind_rows(text.ref.list, .id = "search.term" )

referenced.df <- unnest(text.df, references) 
# here number of rows indicated how many times the search term was written within the document 
# if that is unimportant to us then we should remve duplicate rows.
# Documents where the search terms appeared: 
#"32008L0056" --> marine strategy framework directive
#"32013L0030" -->  safety of offshore oil and gas operations and amending Directive
#"32013R1380" --> Common Fisheries Policy, amending Council Regulations
#"31984D0132"--> Protocol concerning Mediterranean specially protected areas

# O.K. if e combine the document ref and the term ref then the number of times referenced doesnt matter:
referenced.df1 <- 
  referenced.df %>%
  distinct(.keep_all = TRUE) %>%
  rename("to"="CELEX") %>%
  mutate(from="celex.unk") %>%
  select(to,from,references) %>%
  rename("from.name"="references")

total.MPA.ref <- 
  rbind(referenced.df1,doc.referenced) %>%
  mutate(from.name = str_trim(from.name, side = "both"))

# mpa.DES.key
# We have 10 different unique designations: 
#[1] "ramsar site, wetland of international importance"                            
#[2] "world heritage site (natural or mixed)"                                      
#[3] "unesco-mab biosphere reserve"                                                
#[4] "specially protected areas of mediterranean importance (barcelona convention)"
#[5] "sites of community importance (habitats directive)"                          
#[6] "special areas of conservation (habitats directive)"                          
#[7] "special protection area (birds directive)"                                   
#[8] "baltic sea protected area (helcom)"                                          
#[9] "marine protected area (ospar)"                                               
#10] "specially protected area (cartagena convention)" 

head(mpa.DES.key)

mpa.DES.key1 <- 
  mpa.DES.key %>%
  mutate(site1 = str_extract(DESIG_ENG,".+\\,"),
         site2 = str_extract(DESIG_ENG,"\\(.+\\)"),
         site3 = str_extract(DESIG_ENG,".+\\("),
         site4 = str_extract(DESIG_ENG,"\\,.+"),
         site5 = str_extract(DESIG_ENG,"unesco-mab biosphere reserve")) %>%
  mutate(site1 = str_replace_all(site1,"[:punct:]+",""),
         site2 = str_replace_all(site2,"[:punct:]+",""),
         site3 = str_replace_all(site3,"[:punct:]+",""),
         site4 = str_replace_all(site4,"[:punct:]+","")) %>%
  select(-DESIG_ENG) %>%
  pivot_longer(
    cols = starts_with("site"),
    names_to = "search",
    values_to = "term",
    values_drop_na = TRUE
  ) %>% 
  select(mpa,term) %>%
  mutate(term = str_trim(term, side = "both"))

unique(mpa.DES.key1$term)
#18 search terms here bc still inclusing "marine protected area"

#join the MPA ref to the actual MPAs

MPA.links <- 
  mpa.DES.key1 %>%
  left_join(.,total.MPA.ref, by = c("term"="from.name")) %>%
  select(to,from,term,mpa)

n_distinct(mpa.DES.key1$mpa)
#3604
n_distinct(MPA.links$mpa)
#3604

# 1,373 MPAs dont have a document reference these are:
not.linked <-
  MPA.links %>%
  filter(is.na(to)) %>%
  distinct(mpa)
#this is bc 12 search terms never popped up in any documents:
x <- MPA.links %>%
  filter(is.na(to))

unique(x$term)
#[1] "ramsar site"                                          
#[2] "wetland of international importance"                  
#[3] "natural or mixed"                                     
#[4] "world heritage site"                                  
#[5] "unesco-mab biosphere reserve"                         
#[6] "barcelona convention"                                 
#[7] "specially protected areas of mediterranean importance"
#[8] "sites of community importance"                        
#[9] "helcom"                                               
#[10] "baltic sea protected area"                            
#[11] "marine protected area"                                
#[12] "cartagena convention"

# OK lets try to make a document-mpa link network:

MPA.links.table <- 
  MPA.links %>%
  filter(!is.na(to)) %>% # filter out mpas that dont link to documents
  select(-from) %>% 
  rename("from" = "mpa") %>%
  select(to, from, term) %>% # note: to is the celex of the document, from is the mpa id number
  distinct(to, from, .keep_all = TRUE) #duplicated links bc search terms overlap... do getting rid of that

n_distinct(MPA.links.table$from)
#3160 
n_distinct(MPA.links.table$to)
#7

celex.info <-
  network.attributes.final %>%
  filter(CELEX %in% MPA.links.table$to) %>%
  rename("id" = "CELEX") %>%
  select(id) %>%
  mutate(data.type = "EU.Leg")


Net.attributes <-
  mpa.DES.key %>%
  filter(mpa %in% MPA.links.table$from ) %>% # 3604-1373 =2231 obj. dim. add up
  mutate(data.type = "MPA") %>%
  rename("id" = "mpa") %>%
  select(id,data.type) %>%
  rbind(.,celex.info)

  
network <- graph_from_data_frame(d=MPA.links.table, directed = FALSE, vertices = Net.attributes)
print(network, e=TRUE, v=TRUE)

l <- layout.fruchterman.reingold(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(network,
     edge.width=.5,
     vertex.size=1,
     vertex.label=NA,
     #  vertex.label.cex=.75,
     edge.arrow.size=.75,
     edge.arrow.width=.75,
     rescale=F,
     layout=l # trying this layout based on pdf above...
)

# ok instead maybe a better approach is document to designation type?
# instead of the actual mpa id...
# will try this out below

MPA.links.table2 <- 
  MPA.links %>%
  filter(!is.na(to)) %>% # filter out mpas that dont link to documents
  select(-from) %>% 
  rename("from" = "mpa") %>%
  select(to, from, term) %>% # note: to is the celex of the document, from is the mpa id number
  distinct(to,term)%>% 
  rename("from" = "term")

n_distinct(MPA.links.table2$to)
#7 celex
n_distinct(MPA.links.table2$from)
# 6 mps types 

Net.attributes2 <-
  MPA.links.table2 %>%
  distinct(from)%>% 
  mutate(data.type = "MPA.term") %>%
  rename("id" = "from") %>%
  select(id,data.type) %>%
  rbind(.,celex.info) %>%
  mutate(color = 
           case_when(
             data.type == "MPA.term" ~ "#0cb702",
             data.type == "EU.Leg" ~ "#f8766d" ))

network2 <- graph_from_data_frame(d=MPA.links.table2, directed = FALSE, vertices = Net.attributes2)
print(network2, e=TRUE, v=TRUE)

l <- layout_nicely(network2)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(network2,
     edge.width=.5,
     vertex.size=10,
     vertex.label=1,
     vertex.label.cex=.75,
     edge.arrow.size=.75,
     edge.arrow.width=.75,
     rescale=F,
     layout=l # trying this layout based on pdf above...
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




