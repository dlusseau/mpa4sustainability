
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("igraph")
library("widyr")
library("dplyr")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------

EU.mpa.char<- read.csv(file = "WP4/Policy_Interactions/data/01_EU.mpachar.csv")

# EU mpa directives search: 
EU.mpa.termsearch<- read.csv(file = "WP4/Policy_Interactions/data/01_EUmpa.searchterm.CELEX.csv")


document.key.df <- read.csv(file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

#---------------------------------------------------------------------------
#------------- This is the analysis on the second search query  ------------
#----------------------- 18 terms all diff types of ------------------------
#-------------------------- MPA Designation names --------------------------
#---------------------------------------------------------------------------


# lets join the new mpa search documents to their associated document data
EU.mpa.termsearch.data <-
  EU.mpa.termsearch %>%
  left_join(., document.key.df, by = c("CELEX"="celex", "resource.type")) %>%
  select(-work,-type) # we dont need this info anymore

# check
n_distinct(EU.mpa.termsearch$CELEX)
#[1] 180
n_distinct(EU.mpa.termsearch.data$CELEX)
#[1] 180
# The check looks good but remember the dim are now larger due to some documents having multiple terms-labels etc. 

# how many unique label terms?
n_distinct(EU.mpa.termsearch.data$labels)
# 403

# how many unique thems?
n_distinct(EU.mpa.termsearch.data$MT)
# 80
unique(EU.mpa.termsearch.data$MT) #curious about looking at them:

# Exploring document citations ---------------------------------------------

# this citation network code is the same as 03 Rscript just different data. 

MPA.citations <-
  EU.mpa.termsearch.data %>%
  distinct(CELEX,citationcelex) %>% # make sure no duplicate rows bc of multiple labeles/themes
  filter(!is.na(citationcelex)) 

n_distinct(MPA.citations$CELEX)
# [1] 95 documents cited somthing
n_distinct(MPA.citations$citationcelex)
# [1] 515 total number of citation documents


leg.citation_info <- 
  document.key.df %>%
  filter(celex %in% MPA.citations$citationcelex) %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference")
#317 documents cited are legislation

#filtering out citation that are not with legislation:
MPA.citations <- 
  MPA.citations %>%
  filter(citationcelex %in% leg.citation_info$CELEX) 

citation.info <-  rbind(leg.citation_info) #combine non.leg with the leg data 

network.attributes <-
  EU.mpa.termsearch.data %>%
  filter(CELEX %in% MPA.citations$CELEX) %>% 
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
# 387

MPA.citations <-
  MPA.citations %>%
  rename("to"="CELEX",
         "from"="citationcelex")

network <- graph_from_data_frame(d=MPA.citations, directed = TRUE, vertices = network.attributes.final)
print(network, e=TRUE, v=TRUE)


l <- layout.fruchterman.reingold(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(network,
     edge.width=.5,
     vertex.size=3,
     vertex.label=NA,
     vertex.label.cex=1,
     edge.arrow.size=.5,
     edge.arrow.width=1,
     rescale=F,
     layout=l,# trying this layout based on pdf above...
   #  edge.curved=.1
)
# blue are documents referenced within text
# green are those pulled from out MPA eurlex search
# red/pink are those that were pulled in the MPA search and also referenced within other documents pulled

# Second order citations ---------------------------------------------------------------------------

indir.citation_info <- 
  MPA.citations %>%
  select(from) %>%
  left_join(.,document.key.df, by = c("from" = "celex"))

n_distinct(MPA.citations$from)
n_distinct(indir.citation_info$from)

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

# Exploring Eurovoc terms -----------------------------------------------

# Eurovoc Term Co-occurrences: 
cleaned.labels <-
  EU.mpa.termsearch.data %>%
  distinct(CELEX,labels, .keep_all = TRUE)

term.table <- as.data.frame(table(cleaned.labels$CELEX,cleaned.labels$labels))

term.table <- 
  term.table %>%
  rename(CELEX = Var1,
         label = Var2)

head(mpa.policy.notext.df.cleaned)

label.pairs <- 
  cleaned.labels %>%
  pairwise_count(labels,CELEX, sort=TRUE)

#david's code help
label.pairs$all<-apply(apply(cbind(as.character(label.pairs$item1),as.character(label.pairs$item2)),1,sort),2,function(x) paste(x,collapse="."))
#this should be the four columns in alphabetical order collapsed and separated by a dot

#duplicated should work on this
label.pairs.sub<-label.pairs[!duplicated(label.pairs$all),]

#maybe a network plot is a better visualization for this data:
term.pairs_matrix <-
  label.pairs.sub  %>%
  select(-all) %>%
  pivot_wider(
    names_from = item1,
    values_from = n)%>%
  column_to_rownames(.,var = "item2")

term.pairs_matrix[is.na(term.pairs_matrix)] <- 0
term.pairs_matrix <- as.matrix(term.pairs_matrix)

term.pairs<- 
  label.pairs.sub %>%
  select(-all)

attributes1<- 
  label.pairs.sub %>%
  select(-all) %>%
  group_by(item1) %>%
  summarise(sum1 = sum(n)) %>%
  mutate(sum1 = replace_na(sum1,0))

attributes2<- 
  label.pairs.sub %>%
  select(-all) %>%
  group_by(item2) %>%
  summarise(sum2 = sum(n)) %>%
  mutate(sum2 = replace_na(sum2,0))

#Which labels have more than one theme?
more.themes <- 
  EU.mpa.termsearch.data %>%
  distinct(labels,MT) %>%
  group_by(labels) %>%
  mutate(themes = n_distinct(MT)) %>%
  filter(themes > 1)

#STOPPED HERE
# for now I will just keep the location theme since it is the most straight forward 
# will discuss with David. 
remove <- 
  EU.mpa.termsearch.data %>%
  distinct(labels,MT) %>%
  group_by(labels) %>%
  mutate(themes = n_distinct(MT)) %>%
  filter(themes > 1) %>%
  filter(MT!= "7221 Africa"&
         MT!= "7206 Europe") %>%
  select(-themes)

attributes3 <-
  EU.mpa.termsearch.data %>%
  distinct(labels,MT) %>%
  anti_join(.,remove, by = c("labels","MT")) %>%
  mutate(MT=gsub("\\d","",.$MT))


final.attributes <- 
  full_join(attributes1,attributes2, by = c("item1"="item2")) %>%
  mutate(sum2 = replace_na(sum2,0)) %>%
  mutate(sum1 = replace_na(sum1,0)) %>%
  mutate(total.count=sum1+sum2) %>%
  select(-c("sum1","sum2")) %>%
  left_join(.,attributes3, by = c("item1"="labels"))


n <-label.pairs.sub$n*1.15

network <- graph_from_data_frame(d=term.pairs, vertices = final.attributes1, directed = FALSE)

#very helpful document for network vizualizations 
#http://www.kateto.net/wp-content/uploads/2015/06/Polnet%202015%20Network%20Viz%20Tutorial%20-%20Ognyanova.pdf

V(network)$color <- V(network)$color

l <- layout.fruchterman.reingold(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(network,
     edge.width=n,
     edge.color="grey",
     vertex.size=1,
     vertex.label.cex=V(network)$total.count*.02,
     vertex.label.color=V(network)$color,
     vertex.shape="none",
     rescale = TRUE,
     ylim=c(-1,1),xlim=c(-1,1)
     # trying this layout based on pdf above...
)


summary(final.attributes$total.count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    6.00    9.50   19.41   21.00  383.00 

final.attributes1 <-
  final.attributes %>%
  mutate(count.grps = 
           case_when(
             total.count == 1 ~ .1,
             total.count <= 6 ~ .6,
             total.count <= 19.41 ~ 1.1,
             total.count <= 21 ~ 1.6,
             total.count < 383 ~ 2.1.25,
             total.count == 383 ~ 4))


## Archival for now ==============================================================

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

