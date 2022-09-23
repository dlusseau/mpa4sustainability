
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("igraph")
library("widyr")
library("dplyr")
library("tibble")
library("tidyr")

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
     #edge.curved=.1
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
#935

Doc.citations3 <- rbind(MPA.citations,Doc.citations.2)

network.attributes.final3 <- rbind(network.attributes.final,network.attributes.final2)

network.attributes.final3 <- 
  network.attributes.final3 %>%
  distinct(CELEX, .keep_all = TRUE)

n_distinct(Doc.citations3$to)
# 312
n_distinct(Doc.citations3$from)
# 1065
312+1065
#1377
docs <- unique(Doc.citations3$to)
cit <-  unique(Doc.citations3$from)
xx <- as.data.frame(c(docs,cit))
xx <- distinct(xx)

network <- graph.data.frame(d=Doc.citations3, directed = TRUE, vertices = network.attributes.final3)
print(network, e=TRUE, v=TRUE)

l <- layout.fruchterman.reingold(network)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(network,
     edge.width=.5,
     vertex.size=2,
     vertex.label=NA,
     vertex.label.cex=1,
     edge.arrow.size=.5,
     edge.arrow.width=1,
     layout=l*1.2,
     edge.curved=.1,
     vertex.label.dist = 0.5,
)


# Exploring Eurovoc terms -----------------------------------------------

# Eurovoc Term Co-occurrences: 
cleaned.labels <-
  EU.mpa.termsearch.data %>%
  distinct(CELEX,labels, .keep_all = TRUE)

n_distinct(cleaned.labels$MT)

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

n_distinct(final.attributes$MT)

network <- graph_from_data_frame(d=term.pairs, vertices = final.attributes, directed = FALSE)
class(network)

#very helpful document for network vizualizations 
#http://www.kateto.net/wp-content/uploads/2015/06/Polnet%202015%20Network%20Viz%20Tutorial%20-%20Ognyanova.pdf


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
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.00    6.00    9.00   13.07   15.00  175.00

#this tutorial was helpful for this vizualization
#https://tm4ss.github.io/docs/Tutorial_5_Co-occurrence.html#4_Visualization_of_co-occurrence
#https://kateto.net/wp-content/uploads/2016/06/Polnet%202016%20R%20Network%20Visualization%20Workshop.pdf
edges.remove <- V(network)[degree(network)<13]
degree(network)
graphNetwork <-  igraph::delete.vertices(network,edges.remove) 
degree(graphNetwork)


library("viridis")   

colors <- inferno(34)
colors <- colors[-1:-5]
V(graphNetwork)$color <- colors[as.numeric(as.factor(V(graphNetwork)$MT))]

dist <- seq(-.025,0.25, by=.0024)
dist <- rep(c(0.175, -0.175), length.out = 113)
#try to jitter the labels a little to avoid overlap 
V(graphNetwork)$dist <- dist[as.numeric(as.factor(V(graphNetwork)$name))]


l2 <- layout.fruchterman.reingold(graphNetwork)

plot(graphNetwork,
     edge.width=V(graphNetwork)$n,
     edge.color=adjustcolor("gray", alpha.f = .25),
     vertex.size=2,
     vertex.label.cex=(degree(graphNetwork)/sum(degree(graphNetwork))*100), # label size is equiv. to percent of edges associated to the word out of total edges
     vertex.label.color=V(graphNetwork)$color,
     vertex.shape="none",
     rescale = TRUE,
     ylim=c(-.8,.85),xlim=c(-.85,.9),
     layout = l2,
     vertex.label.family = "sans",
     vertex.label.dist = V(graphNetwork)$dist
     # trying this layout based on pdf above...
)





