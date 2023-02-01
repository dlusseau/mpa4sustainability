
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
library("ggplot2")

# Define functions --------------------------------------------------------

# No defined function for this script

# Load data ---------------------------------------------------------------

mpa.policy.notext.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

# Our document-data key
document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

#---------------------------------------------------------------------------
#------------- This is the analysis on the first search query  -------------
#----------------------- "marine protected" -------------------------------
#---------------------------------------------------------------------------

# Exploring document citations ---------------------------------------------

n_distinct(mpa.policy.notext.df$CELEX)
# 18

Doc.citations <-
  mpa.policy.notext.df %>%
  distinct(CELEX,citationcelex) %>% # make sure no duplicate rows bc of multiple labeles/themes
  filter(!is.na(citationcelex)) 

n_distinct(Doc.citations$CELEX)
# 18 document cite another document 
n_distinct(Doc.citations$citationcelex)
# 208 documents are cited

mpa.policy.notext.df %>%
  distinct(CELEX, .keep_all = TRUE) %>%
  group_by(resource.type) %>%
  summarise(n=n_distinct(CELEX))

#  resource.type     n
#  DEC               6
#  DIR               2
#  REG              10

mpa.policy.notext.df %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  group_by(year,force) %>%
  summarise(n=n_distinct(CELEX)) %>%
  ggplot(aes(fill=force, x = year, y = n)) +
  geom_bar(position="stack", stat="identity") +
  ylab("Number of legislations") + 
  xlab("Year")+
  theme_minimal()+ 
  theme(legend.position = "bottom")+
  scale_fill_discrete(name = "Legislation currently enforced", labels = c("No", "Yes"))+ 
  scale_x_continuous(breaks = seq(1980, 2024, by = 4)) +
  scale_y_continuous(limits=c(0, 4.5), expand = c(0,0)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/Q1.overtime.png")


#Eurlex data/attributes about the citations

# NOTE: there is a total of 208 unique citation documents but only 146 have eurolex data associated... 
# this is bc our key is only leg. documents...
# Since we are only interested in legislative documents we will only keep citations that are legislation categorized 

leg.citation_info <- 
  document.key.df %>%
  filter(celex %in% Doc.citations$citationcelex) %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference")

leg.citation_info <- 
  leg.citation_info %>%
  mutate(bad.sector = str_detect(CELEX, "^[^3]" )) %>%
  filter(bad.sector=="FALSE")%>%
  select(-bad.sector)
# 2 needed to be removed they were opinions in prepatory documents

Doc.citations <- 
  Doc.citations %>%
  filter(citationcelex %in% leg.citation_info$CELEX) 
  
citation.info <-  leg.citation_info

n_distinct(Doc.citations$CELEX)
# when from 18 to 17 documents that cite since 2 only cited non-leg


network.attributes <-
  mpa.policy.notext.df %>%
  filter(CELEX %in% Doc.citations$CELEX) %>% 
  select(resource.type,CELEX,date,force) %>%
  distinct(CELEX,.keep_all = TRUE) %>%
  mutate(pulled.from = "eurlex.web") %>%
  rbind(.,citation.info) 


# Which citations are from the eurlex search and references?
both.pulls <-
  citation.info %>%
  filter(CELEX %in% Doc.citations$CELEX)

# 5 documents pulled as an MPA leg are also cited by other legislation 
# These are:
# 32008L0056 --> Marine Strategy Framework Directive
#	32013R1380 --> CFP, amending CRs
#	32014R0508 --> European Maritime and Fisheries Fund & repealing CRs
# 32021R1139 --> European Maritime, Fisheries and Aquaculture Fund (2021–2027)
# 32020R0123 --> Fishing opportunities in EU and non-EU waters (2020)

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
#156
#dimentions add up bc 17(docs)+144(citations)-5(remove the dup.bc within both) = 158

# Now we have edge df
# "from"  = Celex (is the first column)
# "to"= citation celex (second column)
Doc.citations <-
  Doc.citations %>%
  rename("from"="CELEX",
         "to"="citationcelex")

n_distinct(Doc.citations$from)
# 17
n_distinct(Doc.citations$to)
#  144
17+144-5
#156
docs <- unique(Doc.citations$to)
cit <-  unique(Doc.citations$from)
xx <- as.data.frame(c(docs,cit))
sum(duplicated(xx))
xx <- distinct(xx)
# 156 observations

# ok so both the document citataion df and the network attributes df have the same dimentions 

network <- graph_from_data_frame(d=Doc.citations, directed = TRUE, vertices = network.attributes.final)
print(network, e=TRUE, v=TRUE)

l <- layout.fruchterman.reingold(network)


labels <- network.attributes.final[1:5,1]

# 32008L0056 --> Marine Strategy Framework Directive
#	32013R1380 --> CFP, amending CRs
#	32014R0508 --> European Maritime and Fisheries Fund & repealing CRs
# 32021R1139 --> European Maritime, Fisheries and Aquaculture Fund (2021–2027)
# 32020R0123 --> Fishing opportunities in EU and non-EU waters (2020)

labels2 <- rep(NA,time=141)
labels3 <- c(labels,labels2)
V(network)$label <- labels3 

png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/query1.1st.order.networkcitations.png",
    width = 1000, height = 1000)


plot(network,
     edge.width=.5,
     vertex.size=5,
     vertex.label=NA,
     vertex.label.family = "sans",
     vertex.label.cex=.75,
     edge.arrow.size=.25,
     edge.arrow.width=2,
     layout = l
     )
#legend(x=-1,y=-1.075,c("2008L0056: Marine Strategy Framework Directive",
  #                     "32013R1380: CFP, amending CRs",
 #                      "32014R0508: European Maritime and Fisheries Fund & repealing CRs",
   #                    "52016AE4426: Opin. of the European Economic & Social Committee on ‘An integrated European Union policy for the Arctic’"),
    #   cex=1 )
#legend(x=-1,y=-.93,c("Both (result & citation)",
 #                    "Search result",
#                     "Citation"), 
#       pch=21,
#       col="#777777", 
#       pt.bg=unique(V(network)$color), 
#       pt.cex=2, cex=1, bty="n", ncol=1)

dev.off()
# blue are documents referenced within text
# green are those pulled from out MPA eurlex search
# red/pink are those that were pulled in the MPA search and also referenced within other documents pulled

edges <- degree(network)
sum(edges) #606

V(network) # 156

network.attributes.final %>%
  group_by(pulled.from) %>%
  summarise(n=n_distinct(CELEX))

# Save files ---------------------------------------------------------------------


# first order citation data:
write.csv(Doc.citations, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1firstordercit.edgelist.csv", row.names=FALSE)
write.csv(network.attributes.final, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1firstordercit.verticesmetadata.csv", row.names=FALSE)
saveRDS(network, file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1firstordercit.network.rds")


# Second order citations ---------------------------------------------------------------------------

# lets find out what do the citations cite
# OK to have a indirect citation network we need to make the edge list 
# We will need to make a new from|to df and then rbind them this will add the third layer

# Doc.citations what we will rbind to
indir.citation_info <- 
  Doc.citations %>%
  select(to) %>% #citations
  left_join(.,document.key.df, by = c("to" = "celex")) 

#check
n_distinct(Doc.citations$to) # 144
n_distinct(indir.citation_info$to) #144
# no changes :)

Doc.citations.2 <-
  indir.citation_info %>%
  distinct(to,citationcelex) %>% # make sure no duplicate rows bc of multiple labeles/themes
  filter(!is.na(citationcelex)) %>% # remove those that have no citations
  rename("from" = "to", # now this has the wrong label since they were the first order citations now they are citing so change to -> from
         "to" = "citationcelex")

n_distinct(Doc.citations.2$from) # celex
#129
n_distinct(Doc.citations.2$to) # citation
#998

#now make sure citations (to) are only legislation documents 
leg.citation_info2 <- 
  document.key.df %>% # this only has legal acts so it is what we filter out of
  filter(celex %in% Doc.citations.2$to) %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference2")

leg.citation_info2 <-
  leg.citation_info2 %>%
  mutate(bad.sector = str_detect(CELEX, "^[^3]" )) %>%
  filter(bad.sector=="FALSE")%>%
  select(-bad.sector)

#remove those that are opinions in sector 5 and a decision in sector 4 (Complementary legislation)
# do the same for the edge list
Doc.citations.2 <- 
  Doc.citations.2 %>%
  filter(to %in% leg.citation_info2$CELEX) 

n_distinct(Doc.citations.2$to)
#628 citations 

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
#628

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
         CELEX != "32014R0508") %>% # These ones are all three levels - A search result, first order citation and a second order citation so remove them for ref 3 labeling
  mutate(pulled.from = "reference3") %>%
  mutate(color = 
           case_when(
             pulled.from == "reference3" ~ "orange" )) %>%
  distinct()%>%
  select(-n)

# remove the all duplicated pulls
network.attributes.final4 <- network.attributes.final3[!network.attributes.final3$CELEX %in% both.cit2$CELEX,]
# 784-106-106 = 572 math check add up :)

#then rejoin the rows that were duplicated but the correct labeling for the network attributes
network.attributes.final4 <- rbind(network.attributes.final4,both.cit2)
# 572+106 = 678 
	
network.attributes.final4 <- 
  network.attributes.final4 %>%
  mutate(remove = case_when(CELEX == "32008L0056" & pulled.from == "reference2" ~ "remove", # 52016AE4426 doesnt need to be removed since it only has one row... (only a reference once)
                            CELEX == "32013R1380" & pulled.from == "reference2" ~ "remove",
                            CELEX == "32014R0508" & pulled.from == "reference2" ~ "remove",
                            TRUE ~ "keep")) %>%
  filter(remove == "keep") %>%
  distinct(CELEX, .keep_all = TRUE) # now remove the double 32013D1386

# 678 - 3 = 675

n_distinct(Doc.citations3$from)
# 128

n_distinct(Doc.citations3$to)
#  663 (144+628-106-3)

docs <- unique(Doc.citations3$from)
cit <-  unique(Doc.citations3$to)
xx <- as.data.frame(c(docs,cit))
xx <- distinct(xx) #721 documents
# ok so both the document citataion df and the network attributes df have the same dimentions 

network2 <- graph.data.frame(d=Doc.citations3, directed = TRUE, vertices = network.attributes.final4)
print(network2, e=TRUE, v=TRUE)

l2 <- layout.fruchterman.reingold(network2)


labels <- network.attributes.final[1:4,1]
labels2 <- network.attributes.final[11,1]

labels3 <- rep(NA,time=578)
labels4 <- c(labels,labels3)
labels4 <- append(labels4,labels2, after = 10)

V(network2)$label <- labels4

png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/query1.2nd.order.networkcitations.png",
    width = 1100, height = 1100)

plot(network2,
     edge.width=.5,
     edge.color=adjustcolor("gray", alpha.f = .65),
     vertex.size=2.5,
     vertex.label=NA ,#V(network2)$label ,
     vertex.label.cex=1,
     vertex.label.family = "sans",
     edge.arrow.size=.5,
     edge.arrow.width=1,
     layout=l2
)


#32016R1624 does cite itself... double checked on EUR-Lex... 
dev.off()

edges <- degree(network2)
sum(edges) #3132

V(network2) #675

network.attributes.final4 %>%
  group_by(pulled.from) %>%
  summarise(n=n_distinct(CELEX))

# both            5
# eurlex.web     12
# reference      33
# reference2    519
# reference3    106

# Save files ---------------------------------------------------------------------


# second order citation data:
write.csv(Doc.citations3, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1secondordercit.edgelist.csv", row.names=FALSE)
write.csv(network.attributes.final4, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1secondordercit.verticesmetadata.csv", row.names=FALSE)
saveRDS(network2, file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1secondordercit.network.rds")


# Exploring Eurovoc terms -----------------------------------------------

# Making a EuroVoc term co-occurance for the seed and citations!

# get the network attributes:

Order1.docs <- read.csv("WP4/Policy_Interactions/data/03.Q1firstordercit.verticesmetadata.csv")
Order2.docs <- read.csv("WP4/Policy_Interactions/data/03.Q1secondordercit.verticesmetadata.csv")

#remove unnec. columns for this 
Order1.docs <-
  Order1.docs %>%
  select(-color,-shape)

Order2.docs <-
  Order2.docs %>%
  select(-color,-shape)

#the key doc only keep celex and eurovoc
celex.label.key <- 
  document.key.df %>%
  distinct(celex,labels)

#now get the network docs eurovoc descriptors 
Order1.docsdescript <- 
  Order1.docs %>%
  left_join(.,celex.label.key, by = c("CELEX" = "celex"))

Order1.docsdescript %>%
  distinct(CELEX,labels) %>% dim()

Order2.docsdescript <- 
  Order2.docs %>%
  left_join(.,celex.label.key, by = c("CELEX" = "celex"))

Order2.docsdescript %>%
  distinct(CELEX,labels) %>% dim()

#check 
n_distinct(Order1.docsdescript$CELEX)
#156 --> all good :)

n_distinct(Order2.docsdescript$CELEX)
# 675 --> all good :)

#how many labels?
n_distinct(Order1.docsdescript$labels)
#425
n_distinct(Order2.docsdescript$labels)
#1218


Order1.label.pairs <- 
  Order1.docsdescript %>%
  pairwise_count(labels,CELEX, sort=TRUE)

Order2.label.pairs <- 
  Order2.docsdescript %>%
  pairwise_count(labels,CELEX, sort=TRUE)

#david's code help
Order1.label.pairs$all<-apply(apply(cbind(as.character(Order1.label.pairs$item1),as.character(Order1.label.pairs$item2)),1,sort),2,function(x) paste(x,collapse="."))
#this should be the four columns in alphabetical order collapsed and separated by a dot
Order2.label.pairs$all<-apply(apply(cbind(as.character(Order2.label.pairs$item1),as.character(Order2.label.pairs$item2)),1,sort),2,function(x) paste(x,collapse="."))

#duplicated should work on this
Order1.label.pairs.sub<-Order1.label.pairs[!duplicated(Order1.label.pairs$all),]

Order2.label.pairs.sub<-Order2.label.pairs[!duplicated(Order2.label.pairs$all),]


Order1.term.pairs<- 
  Order1.label.pairs.sub %>%
  select(-all) 

Order2.term.pairs<- 
  Order2.label.pairs.sub %>%
  select(-all) 

summary(Order1.term.pairs)
summary(Order2.term.pairs)



Order1.final.attributes <- Order1.docsdescript %>% distinct(labels) %>% drop_na()
Order2.final.attributes <- Order2.docsdescript %>% distinct(labels) %>% drop_na()


Order1.EuroVoc.network <- graph_from_data_frame(d=Order1.term.pairs, vertices = Order1.final.attributes, directed = FALSE)
class(Order1.EuroVoc.network)


Order2.EuroVoc.network <- graph_from_data_frame(d=Order2.term.pairs, vertices = Order2.final.attributes, directed = FALSE)
class(Order2.EuroVoc.network)


# Save ------------------------------------------------------------------

# term co-ocurances:
# 1st order citations
write.csv(Order1.term.pairs, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C1term.edgelist.csv", row.names=FALSE)
write.csv(Order1.final.attributes, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C1term.verticesmetadata.csv", row.names=FALSE)
saveRDS(Order1.EuroVoc.network, file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C1.termnetwork.rds")

# 2nd order citations
write.csv(Order2.term.pairs, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C2term.edgelist.csv", row.names=FALSE)
write.csv(Order2.final.attributes, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C2term.verticesmetadata.csv", row.names=FALSE)
saveRDS(Order2.EuroVoc.network, file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C2.termnetwork.rds")

# Archival code for EuroVoc graphics --------------------------------------
# (graphics we actually use are in the network stats rscript
#l <- layout.fruchterman.reingold(network3)
#l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

#degree(network3)
#n_distinct(V(network3)$MT)

#library("viridis")   

#colors <- inferno(29)
#colors <- colors[-1:-5]
#(network3)$color <- colors[as.numeric(as.factor(V(network3)$MT))]

#dist <- rep(c(0.18, -0.18), length.out = 103)
#try to jitter the labels a little to avoid overlap 
#V(network3)$dist <- dist[as.numeric(as.factor(V(network3)$name))]


#plot(network3,
 #    edge.width=E(network3)$n*1,
 #    edge.color=adjustcolor("gray", alpha.f = .5),
 #    vertex.size=2,
 #    vertex.label.cex=(degree(network3)/sum(degree(network3))*100),
#     vertex.label.color=V(network3)$color,
#     vertex.shape="none",
#     rescale = TRUE,
 #    layout = l,
#     vertex.label.dist = V(network)$dist,
#     vertex.label.family = "sans"
     
#)

#legend(x=-.1,y=1.2,unique(V(network3)$MT), 
 #      pch=21,
 #      col="#777777", 
 #      pt.bg=unique(V(network3)$color), 
 #      pt.cex=2, 
 #      cex=1, 
 #      bty="n", # no box around the legen 
 #      ncol=2)



#edges <- degree(network3)
#sum(edges)
#term.pairs%>%
#summarise(total = sum(n))
#V(network3)

# O.K. so the network viz is more legable 
# I will only plot those that are the median or above edges

#I1 <-
#  term.pairs %>%
#  group_by(item1) %>%
#  summarise(n=n())
#I2 <-
#  term.pairs %>%
#  group_by(item2) %>%
#  summarise(n=n())

#I3 <- full_join(I1,I2, by=c("item1"="item2")) %>%
#  mutate(n.x = replace_na(n.x,0),
#         n.y = replace_na(n.y,0))%>%
#  mutate(edge.number = n.x+n.y )

#summary(I3$edge.number)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00    7.00    9.00   11.79   14.50   47.00 

#edges.remove <- V(network3)[degree(network3)<9]
#degree(network3)
#graphNetwork <-  igraph::delete.vertices(network3,edges.remove) 
#degree(graphNetwork)

#n_distinct(V(graphNetwork)$MT)
#17 themes 

#library("viridis")   

#colors <- inferno(17)
#colors <- colors[-1]
#V(graphNetwork)$color <- colors[as.numeric(as.factor(V(graphNetwork)$MT))]

#dist <- seq(-.025,0.25, by=.0024)
#dist <- rep(c(0.25, -0.25), length.out = 54)
#try to jitter the labels a little to avoid overlap 
#V(graphNetwork)$dist <- dist[as.numeric(as.factor(V(graphNetwork)$name))]


#l2 <- layout.fruchterman.reingold(graphNetwork)

#plot(graphNetwork,
 #    edge.width=E(graphNetwork)$n*1,
#     edge.color=adjustcolor("gray", alpha.f = .25),
   #  vertex.size=2,
  #   vertex.label.cex=(degree(graphNetwork)/sum(degree(graphNetwork))*75), # label size is equiv. to percent of edges associated to the word out of total edges
 #    vertex.label.color=V(graphNetwork)$color,
#     vertex.shape="none",
    # rescale = TRUE,
   #  layout = l2,
  #   vertex.label.family = "sans",
#     vertex.label.dist = V(graphNetwork)$dist
     # trying this layout based on pdf above...
#)

#legend(x=-.1,y=-.7,unique(V(graphNetwork)$MT), 
#       pch=21,
#       col="#777777", 
#       pt.bg=unique(V(graphNetwork)$color), 
#       pt.cex=2, 
#       cex=1, 
#       bty="n", # no box around the legen 
#       ncol=2)



#edges <- degree(graphNetwork)
#sum(edges)

#V(graphNetwork)



