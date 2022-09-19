
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------

EU.mpa.char<- read.csv(file = "WP4/Policy_Interactions/data/01_EU.mpachar.csv")

# Exploring MPA designation types referenced in EU text --------------------



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
# add a ? at the end 

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

l <- layout.fruchterman.reingold(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(network,
     edge.width=.5,
     vertex.size=5,
     vertex.label=NA,
     #  vertex.label.cex=.75,
     edge.arrow.size=.75,
     edge.arrow.width=.75,
     rescale=F,
     layout=l*1.1 # trying this layout based on pdf above...
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

