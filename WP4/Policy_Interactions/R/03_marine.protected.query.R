
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("dplyr")
library("tibble")
library("lubridate")
library("tidyr")
library("stringr")
library("igraph")
library("widyr")

# Define functions --------------------------------------------------------

# No defined function for this script

# Load data ---------------------------------------------------------------

mpa.policy.notext.df <- read.csv(file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

# Our document-data key
document.key.df <- read.csv(file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")


#---------------------------------------------------------------------------
#------------- This is the analysis on the first search query  -------------
#----------------------- "marine protected " -------------------------------
#---------------------------------------------------------------------------

# Exploring document citations ---------------------------------------------

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

mpa.policy.notext.df %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  group_by(resource.type) %>%
  summarise(n=n_distinct(CELEX))

# DEC               6
# DIR               2
# OPIN              8
# RECO              1
# REG               7


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
#dimentions add up bc 17(docs)+134(citations)-3(remove the dup.bc within both) = 148

Doc.citations <-
  Doc.citations %>%
  rename("to"="CELEX",
         "from"="citationcelex")

n_distinct(Doc.citations$to)
# 17
n_distinct(Doc.citations$from)
#  134
17+134-3
#148
docs <- unique(Doc.citations$to)
cit <-  unique(Doc.citations$from)
xx <- as.data.frame(c(docs,cit))
xx <- distinct(xx)
# 148 observations

# ok so both the document citataion df and the network attributes df have the same dimentions 

network <- graph_from_data_frame(d=Doc.citations, directed = TRUE, vertices = network.attributes.final)
print(network, e=TRUE, v=TRUE)

# <- layout.fruchterman.reingold(network)
#l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)


labels <- network.attributes.final[1:3,1]

# 32008L0056 --> Marine Strategy Framework Directive
#	32013R1380 --> CFP, amending CRs
#	32014R0508 --> European Maritime and Fisheries Fund & repealing CRs

labels2 <- rep(NA,time=222)
labels3 <- c(labels,labels2)
V(network)$label <- labels3 

plot(network,
     edge.width=.5,
     vertex.size=5,
  #  vertex.label=NA,
     vertex.label.family = "sans",
     vertex.label.cex=.75,
     edge.arrow.size=.25,
     edge.arrow.width=2,
#    layout = l
     )
legend(x=-1,y=-1.,c("2008L0056: Marine Strategy Framework Directive",
                   "32013R1380: CFP, amending CRs",
                   "32014R0508:European Maritime and Fisheries Fund & repealing CRs"),
       cex=1 )
legend(x=-1,y=-.85,c("Both (result & citation)",
                   "Search result",
                    "Citation"), 
       pch=21,
       col="#777777", 
       pt.bg=unique(V(network)$color), 
       pt.cex=2, cex=1, bty="n", ncol=1)

# blue are documents referenced within text
# green are those pulled from out MPA eurlex search
# red/pink are those that were pulled in the MPA search and also referenced within other documents pulled

edges <- degree(network)
sum(edges)

V(network)

# Second order citations ---------------------------------------------------------------------------

# lets find out what do the citations cite?
# OK to have a indirect citation network we need to make the edge list 
# We will need to make a new to|from df and then rbind them this will add the third layer

# Doc.citations what we will rbind to

indir.citation_info <- 
  Doc.citations %>%
  select(from) %>% #citations
  left_join(.,document.key.df, by = c("from" = "celex")) 

#check
n_distinct(Doc.citations$from)
n_distinct(indir.citation_info$from)
# no changes :)

Doc.citations.2 <-
  indir.citation_info %>%
  distinct(from,citationcelex) %>% # make sure no duplicate rows bc of multiple labeles/themes
  filter(!is.na(citationcelex)) %>% # remove those that have no citations
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
n_distinct(Doc.citations.2$to)
#104 citations cite another document 

citation.info2 <-  leg.citation_info2

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

# some of the citations are both second and first order ciatations so lets make a new label... (color is orange and the name is reference 3)
both.cit2 <-
 network.attributes.final3 %>%
 group_by(CELEX) %>%
 mutate(n=n()) %>%
 filter(n>1) %>%
  filter(CELEX != "32008L0056" &
         CELEX != "32013R1380" &
         CELEX != "32014R0508" &
         CELEX != "32013D1386") %>% # this last one is now a both
  mutate(pulled.from = "reference3") %>%
  mutate(color = 
           case_when(
             pulled.from == "reference3" ~ "orange" )) %>%
  distinct()%>%
  select(-n)

network.attributes.final4 <- network.attributes.final3[!network.attributes.final3$CELEX %in% both.cit2$CELEX,]
# 663-76-76 = 551 math check add up :)

network.attributes.final4 <- rbind(network.attributes.final4,both.cit2)
# 551+76 = 587 
	
network.attributes.final4 <- 
  network.attributes.final4 %>%
  mutate(pulled.from = case_when(CELEX == "32013D1386" ~ "both", # change this one to both since it is second order referenced. 
                                 TRUE ~ pulled.from)) %>%
  mutate(color = case_when(CELEX == "32013D1386" ~ "#f8766d", 
                                 TRUE ~ color)) %>%
  mutate(remove = case_when(CELEX == "32008L0056" & pulled.from == "reference2" ~ "remove", # 
                            CELEX == "32013R1380" & pulled.from == "reference2" ~ "remove",
                            CELEX == "32014R0508" & pulled.from == "reference2" ~ "remove",
                            TRUE ~ "keep")) %>%
  filter(remove == "keep") %>%
  distinct(CELEX, .keep_all = TRUE) # now remove the double 32013D1386

# 587 - 4 = 583

n_distinct(Doc.citations3$to)
# 118 

n_distinct(Doc.citations3$from)
#  570 (134+515-76-4)

docs <- unique(Doc.citations3$to)
cit <-  unique(Doc.citations3$from)
xx <- as.data.frame(c(docs,cit))
xx <- distinct(xx) #583 documents
# ok so both the document citataion df and the network attributes df have the same dimentions 

network <- graph.data.frame(d=Doc.citations3, directed = TRUE, vertices = network.attributes.final4)
print(network, e=TRUE, v=TRUE)

l <- layout.fruchterman.reingold(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)


labels <- network.attributes.final[1:3,1]
labels2 <- network.attributes.final[10,1]

labels3 <- rep(NA,time=579)
labels4 <- c(labels,labels3)
labels4 <- append(labels4,labels2, after = 9)

V(network)$label <- labels4

plot(network,
     edge.width=.5,
     edge.color=adjustcolor("gray", alpha.f = .5),
     vertex.size=3,
  #  vertex.label=NA,
     vertex.label.cex=.65,
     vertex.label.family = "sans",
     edge.arrow.size=.5,
     edge.arrow.width=1,
     layout=l
)

legend(x=-1.3,y=-1.05,c("32008L0056: Marine Strategy Framework Directive",
                        "32014R0508: Reg.on the European Maritime and Fisheries Fund and repealing CR (EC) No 2328/2003, No 861/2006, No 1198/2006 and No 791/2007 and Reg. (EU) No 1255/2011",
                        "32013R1380: Reg. on the CFP, amending CR (EC) No 1954/2003 and 1224/2009 and repealing CR (EC) No 2371/2002 and 639/2004 and CD 2004/585/ECs",
                        "32013D1386: Decision on a General Union Environment Action Programme to 2020 ‘Living well, within the limits of our planet’"),
       cex=1 )
legend(x=-1.3,y=-.82,c("Both (result & citation)",
                       "Search result",
                       "Only first order citation",
                       "Only second order citation",
                       "First and second order citation"), 
       pch=21,
       col="#777777", 
       pt.bg=unique(V(network)$color), 
       pt.cex=2, 
       cex=1, 
       bty="n", 
       ncol=1)

#32016R1624 does cite itself... double checked on EUR-Lex... 

edges <- degree(network)
sum(edges)

V(network)

# Exploring Eurovoc terms -----------------------------------------------

# Eurovoc Term Co-occurrences: 
cleaned.labels <-
  mpa.policy.notext.df %>%
  distinct(CELEX,labels, .keep_all = TRUE)

n_distinct(cleaned.labels$CELEX)
# 24
n_distinct(cleaned.labels$labels)
# 103 label terms
n_distinct(cleaned.labels$MT)
# 29 themes 

label.pairs <- 
  cleaned.labels %>%
  pairwise_count(labels,CELEX, sort=TRUE)

#david's code help
label.pairs$all<-apply(apply(cbind(as.character(label.pairs$item1),as.character(label.pairs$item2)),1,sort),2,function(x) paste(x,collapse="."))
#this should be the four columns in alphabetical order collapsed and separated by a dot

#duplicated should work on this
label.pairs.sub<-label.pairs[!duplicated(label.pairs$all),]

term.pairs<- 
  label.pairs.sub %>%
  select(-all) 

summary(term.pairs)

# for the left label column find the number of times a label is linked to another label.
attributes1<- 
  label.pairs.sub %>%
  select(-all) %>%
  group_by(item1) %>%
  summarise(sum1 = sum(n)) %>%
  mutate(sum1 = replace_na(sum1,0))

# for the right label column find the number of times a label is linked to another label.
attributes2<- 
  label.pairs.sub %>%
  select(-all) %>%
  group_by(item2) %>%
  summarise(sum2 = sum(n)) %>%
  mutate(sum2 = replace_na(sum2,0))


#Which labels have more than one theme?
more.themes <- 
  mpa.policy.notext.df %>%
  distinct(labels,MT) %>%
  group_by(labels) %>%
  mutate(themes = n_distinct(MT)) %>%
  filter(themes > 1)

#STOPPED HERE
# for now I will just keep the location theme since it is the most straight forward 
# will discuss with David. 
remove <- 
  mpa.policy.notext.df %>%
  distinct(labels,MT) %>%
  group_by(labels) %>%
  mutate(themes = n_distinct(MT)) %>%
  filter(themes > 1) %>%
  filter(MT!= "7221 Africa"&
           MT!= "7206 Europe") %>%
  select(-themes)

attributes3 <-
  mpa.policy.notext.df %>%
  distinct(labels,MT) %>%
  anti_join(.,remove, by = c("labels","MT")) %>% # remove the unwanted themes
  mutate(MT=gsub("\\d","",.$MT)) # remove the theme number code


final.attributes <- 
  full_join(attributes1,attributes2, by = c("item1"="item2")) %>%
  mutate(sum2 = replace_na(sum2,0)) %>% # make NAs 0
  mutate(sum1 = replace_na(sum1,0)) %>% # make NAs 0
  mutate(total.count=sum1+sum2) %>% # get the total number of edges
  select(-c("sum1","sum2")) %>%
  left_join(.,attributes3, by = c("item1"="labels"))

n_distinct(final.attributes$MT)

network <- graph_from_data_frame(d=term.pairs, vertices = final.attributes, directed = FALSE)
class(network)


l <- layout.fruchterman.reingold(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

degree(network)
n_distinct(V(network)$MT)

library("viridis")   

colors <- inferno(29)
#colors <- colors[-1:-5]
V(network)$color <- colors[as.numeric(as.factor(V(network)$MT))]

dist <- rep(c(0.18, -0.18), length.out = 103)
#try to jitter the labels a little to avoid overlap 
V(network)$dist <- dist[as.numeric(as.factor(V(network)$name))]


plot(network,
     edge.width=E(network)$n*1,
     edge.color=adjustcolor("gray", alpha.f = .5),
     vertex.size=2,
     vertex.label.cex=(degree(network)/sum(degree(network))*100),
     vertex.label.color=V(network)$color,
     vertex.shape="none",
     rescale = TRUE,
     layout = l,
     vertex.label.dist = V(network)$dist,
     vertex.label.family = "sans"
     
)

legend(x=-.1,y=1.2,unique(V(network)$MT), 
       pch=21,
       col="#777777", 
       pt.bg=unique(V(network)$color), 
       pt.cex=2, 
       cex=1, 
       bty="n", # no box around the legen 
       ncol=2)



edges <- degree(network)
sum(edges)

V(network)

# O.K. so the network viz is more legable 
# I will only plot those that are the median or above edges

I1 <-
  term.pairs %>%
  group_by(item1) %>%
  summarise(n=n())
I2 <-
  term.pairs %>%
  group_by(item2) %>%
  summarise(n=n())

I3 <- full_join(I1,I2, by=c("item1"="item2")) %>%
  mutate(n.x = replace_na(n.x,0),
         n.y = replace_na(n.y,0))%>%
  mutate(edge.number = n.x+n.y )

summary(I3$edge.number)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00    7.00    9.00   11.79   14.50   47.00 

edges.remove <- V(network)[degree(network)<9]
degree(network)
graphNetwork <-  igraph::delete.vertices(network,edges.remove) 
degree(graphNetwork)

n_distinct(V(graphNetwork)$MT)
#17 themes 

library("viridis")   

colors <- inferno(17)
#colors <- colors[-1]
V(graphNetwork)$color <- colors[as.numeric(as.factor(V(graphNetwork)$MT))]

#dist <- seq(-.025,0.25, by=.0024)
dist <- rep(c(0.25, -0.25), length.out = 54)
#try to jitter the labels a little to avoid overlap 
V(graphNetwork)$dist <- dist[as.numeric(as.factor(V(graphNetwork)$name))]


l2 <- layout.fruchterman.reingold(graphNetwork)

plot(graphNetwork,
     edge.width=E(graphNetwork)$n*1,
     edge.color=adjustcolor("gray", alpha.f = .25),
     vertex.size=2,
     vertex.label.cex=(degree(graphNetwork)/sum(degree(graphNetwork))*75), # label size is equiv. to percent of edges associated to the word out of total edges
     vertex.label.color=V(graphNetwork)$color,
     vertex.shape="none",
     rescale = TRUE,
     layout = l2,
     vertex.label.family = "sans",
     vertex.label.dist = V(graphNetwork)$dist
     # trying this layout based on pdf above...
)

legend(x=-.1,y=-.7,unique(V(graphNetwork)$MT), 
       pch=21,
       col="#777777", 
       pt.bg=unique(V(graphNetwork)$color), 
       pt.cex=2, 
       cex=1, 
       bty="n", # no box around the legen 
       ncol=2)



edges <- degree(graphNetwork)
sum(edges)

V(graphNetwork)


