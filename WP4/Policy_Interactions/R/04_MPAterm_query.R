
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

EU.mpa.char<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_EU.mpachar.csv")

# EU mpa directives search: 
EU.mpa.termsearch<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_EUmpa.searchterm.CELEX2.csv")


document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

#---------------------------------------------------------------------------
#------------- This is the analysis on the second search query  ------------
#----------------------- 18 terms all diff types of ------------------------
#-------------------------- MPA Designation names --------------------------
#---------------------------------------------------------------------------

n_distinct(EU.mpa.termsearch$CELEX)
#100
# lets join the new mpa search documents to their associated document data
EU.mpa.termsearch.data <-
  EU.mpa.termsearch %>%
  filter(CELEX != "32006R1967R(01)") %>% #removing this celex bc it is a Corrigendum to a regulation that was already pulled and it is tech. not within the legal act types
  select(-work,-type) # we dont need this info anymore

n_distinct(EU.mpa.termsearch.data$CELEX)
#99
EU.mpa.termsearch.data %>%
  #distinct(CELEX, .keep_all = TRUE) %>% 
  group_by(search.term) %>%
  summarise(n= n_distinct(CELEX))

#    search.term                      n
# barcelona convention*?             23
# birds directive*?                  19
# habitats directive*?               22
# helcom*?                            6
# marine protected area*?            18
# ospar*?                            16
# site of community importance*?      4
# sites of community importance*?     7
# special areas of conservation*?    20
# special protection area*?          22
# specially protected area*?          9
# world heritage site*?               2

#compare which mpa titles pulled same as ones that say MPA
mpa.policy.notext.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

x <- mpa.policy.notext.df %>% distinct(CELEX)

xx <-
  EU.mpa.termsearch.data %>%
  filter(search.term == "marine protected area*?") %>%
  distinct(CELEX)

identical(x, xx)

EU.mpa.termsearch.data %>%
  filter(CELEX %in% x$CELEX) %>%
  distinct(search.term,CELEX) %>%
  group_by(search.term) %>%
  summarise(n=n())

n_distinct(EU.mpa.termsearch.data$CELEX)
#[1] 99
# The remember the dim are now larger due to some documents having multiple terms-labels etc. 

EU.mpa.termsearch.data %>%
  group_by(resource.type) %>%
  summarise(n = n_distinct(CELEX)) %>%
  summarise(total = sum(n))
# DEC              41
# DIR              16
# OPIN              2
# RECO              2
# REG              38

# how many unique label terms?
n_distinct(EU.mpa.termsearch.data$labels)
# 281

# how many unique thems?
n_distinct(EU.mpa.termsearch.data$MT)
# 67
unique(EU.mpa.termsearch.data$MT) #curious about looking at them:


EU.mpa.termsearch.data %>%
  #filter(!is.na(force)) %>% #filtering out leg that is deemed N.A
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
  scale_fill_discrete(name = "Legislation currently enforced", labels = c("No", "Yes", "N/A"))+ 
  scale_x_continuous(breaks = seq(1976, 2024, by = 3)) +
  scale_y_continuous(limits=c(0, 15),breaks = seq(0, 15, by = 3) ,expand = c(0,0)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/Q2.overtime.png")

# Legislation numbers over times by resource/leg. type:
EU.mpa.termsearch.data %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  group_by(year,resource.type) %>%
  summarise(n=n_distinct(CELEX)) %>%
  ggplot(aes(fill=resource.type, x = year, y = n)) +
  geom_bar(position="stack", stat="identity") +
  ylab("Number of legislations") + 
  xlab("Year")+
  theme_minimal()+ 
  theme(legend.position = "bottom")+
  scale_color_discrete(name = "Type of legislation")

# Legislation numbers over times by resource/leg. type:
x <- EU.mpa.termsearch.data %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  group_by(year,search.term) %>%
  summarise(n=n_distinct(CELEX)) %>%
  ggplot(aes(fill=search.term, x = year, y = n)) +
  geom_bar(position="stack", stat="identity") +
  ylab("Number of legislations") + 
  xlab("Year")+
  theme_minimal()+ 
  theme(legend.position = "bottom")+
  scale_color_discrete(name = "Type of legislation")

EU.mpa.char %>%
  group_by(DESIG_ENG) %>%
  summarise(n=n_distinct(mpa))
# DESIG_ENG                                                                        n
# 1 baltic sea protected area (helcom)                                             163
# 2 marine protected area (ospar)                                                  441
# 3 ramsar site, wetland of international importance                               237
# 4 sites of community importance (habitats directive)                             481
# 5 special areas of conservation (habitats directive)                            1388
# 6 special protection area (birds directive)                                      843
# 7 specially protected area (cartagena convention)                                  7
# 8 specially protected areas of mediterranean importance (barcelona convention)    24
# 9 unesco-mab biosphere reserve                                                    11
#10 world heritage site (natural or mixed)                                           9

EU.mpa.char %>%
  filter(DESIG_ENG == "sites of community importance (habitats directive)" |
         DESIG_ENG == "special areas of conservation (habitats directive)") %>%
  summarise(n=n_distinct(mpa))
# 1869

EU.mpa.char %>%
  group_by(DESIG_ENG) %>%
  summarise(n=n_distinct(PARENT_ISO))

# DESIG_ENG                                                                        n
# baltic sea protected area (helcom)                                               8
# marine protected area (ospar)                                                   10
# ramsar site, wetland of international importance                                18
# sites of community importance (habitats directive)                              18
# special areas of conservation (habitats directive)                              19
# special protection area (birds directive)                                       23
# specially protected area (cartagena convention)                                  2
# specially protected areas of mediterranean importance (barcelona convention)     4
# unesco-mab biosphere reserve                                                     6
# world heritage site (natural or mixed)                                           4

EU.mpa.char %>%
  filter(DESIG_ENG == "sites of community importance (habitats directive)" |
           DESIG_ENG == "special areas of conservation (habitats directive)") %>%
  summarise(n=n_distinct(PARENT_ISO))
# 22

EU.mpa.char %>%
  summarise(n=n_distinct(mpa))
#3604


# Exploring document citations ---------------------------------------------

  n_distinct(EU.mpa.termsearch.data$CELEX)
# this citation network code is the same as 03 Rscript just different data. 

MPA.citations <-
  EU.mpa.termsearch.data %>%
  distinct(CELEX,citationcelex) %>% # make sure no duplicate rows bc of multiple labeles/themes
  filter(!is.na(citationcelex)) 

n_distinct(MPA.citations$CELEX)
# [1] 84 documents cited somthing
n_distinct(MPA.citations$citationcelex)
# [1] 406 total number of citation documents

# stopped here Jan 3rd will finish up tomorrow on this getting tired and want to make sure I go through everything correctly :)

leg.citation_info <- 
  document.key.df %>%
  filter(celex %in% MPA.citations$citationcelex) %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference")
#278 documents cited are legislation


leg.citation_info <- 
  leg.citation_info %>%
  mutate(bad.sector = str_detect(CELEX, "^[^3]" )) %>%
  filter(bad.sector=="FALSE")%>%
  select(-bad.sector)
# 5 opionons in prep docs removed

#filtering out citation that are not with legislation:
MPA.citations <- 
  MPA.citations %>%
  filter(citationcelex %in% leg.citation_info$CELEX) 

n_distinct(MPA.citations$CELEX)
#76
n_distinct(MPA.citations$citationcelex)
#273
citation.info <-  leg.citation_info 


network.attributes <-
  EU.mpa.termsearch.data %>%
  filter(CELEX %in% MPA.citations$CELEX) %>% 
  select(resource.type,CELEX,date,force) %>%
  distinct(CELEX,.keep_all = TRUE) %>%
  mutate(pulled.from = "eurlex.web") %>%
  rbind(.,citation.info) # %>%


both.pulls <-
  citation.info %>%
  filter(CELEX %in% MPA.citations$CELEX)
#20


network.attributes.both <- network.attributes[network.attributes$CELEX %in% both.pulls$CELEX,]

network.attributes.both <- 
  network.attributes.both %>%
  distinct(CELEX,.keep_all = TRUE) %>%
  mutate(pulled.from= "both")

network.attributes.notboth <- network.attributes[!network.attributes$CELEX %in% both.pulls$CELEX,]

network.attributes.final <-
  rbind(network.attributes.both,network.attributes.notboth)
#20+309 = 329

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
# 329

# Now we have edge df
# "from"  = Celex (is the first column)
# "to"= citation celex (second column)
MPA.citations <-
  MPA.citations %>%
  rename("from"="CELEX",
         "to"="citationcelex")

n_distinct(MPA.citations$from)
# 76
n_distinct(MPA.citations$to)
# 273
273+76-20
#329
docs <- unique(MPA.citations$to)
cit <-  unique(MPA.citations$from)
xx <- as.data.frame(c(docs,cit))
sum(duplicated(xx))
xx <- distinct(xx) #329 documents
# ok vertices df and attribute df dim are the same :) 

network <- graph_from_data_frame(d=MPA.citations, directed = TRUE, vertices = network.attributes.final)
print(network, e=TRUE, v=TRUE)


l <- layout.fruchterman.reingold(network)

labels <- network.attributes.final[1:32,1]

labels2 <- rep(NA,time=349)
labels3 <- c(labels,labels2)
V(network)$label <- labels3 

png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/query2.1st.order.networkcitations.png",
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


dev.off()
# blue are documents referenced within text
# green are those pulled from out MPA eurlex search
# red/pink are those that were pulled in the MPA search and also referenced within other documents pulled
edges <- degree(network)
sum(edges)
#1114
V(network)
#329
network.attributes.final %>%
  group_by( pulled.from) %>%
  summarise(n=n())
#  pulled.from     n
#  both           20
#  eurlex.web     56
#  reference     253

# Save files ---------------------------------------------------------------------


# first order citation data:
write.csv(MPA.citations, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2firstordercit.edgelist.csv", row.names=FALSE)
write.csv(network.attributes.final, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2firstordercit.verticesmetadata.csv", row.names=FALSE)
saveRDS(network, file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2firstordercit.network.rds")



# Second order citations ---------------------------------------------------------------------------

indir.citation_info <- 
  MPA.citations %>%
  select(to) %>%
  left_join(.,document.key.df, by = c("to" = "celex"))


n_distinct(MPA.citations$to)# 273
n_distinct(indir.citation_info$to) # 273

Doc.citations.2 <-
  indir.citation_info %>%
  distinct(to,citationcelex) %>% # make sure no duplicate rows bc of multiple labeles/themes
  filter(!is.na(citationcelex)) %>%
  rename("from" = "to",
         "to" = "citationcelex")

n_distinct(Doc.citations.2$from) # celex
#244
n_distinct(Doc.citations.2$to) # citation
#1385

#now make sure citations (to) are only legislation documents 
leg.citation_info2 <- 
  document.key.df %>%
  filter(celex %in% Doc.citations.2$to) %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference2") # label them as second order references


leg.citation_info2 <- 
  leg.citation_info2 %>%
  mutate(bad.sector = str_detect(CELEX, "^[^3]" )) %>%
  filter(bad.sector=="FALSE")%>%
  select(-bad.sector)

#removed prep opinions and one decision in complementary documents


#do the same to the edge list
Doc.citations.2 <- 
  Doc.citations.2 %>%
  filter(to %in% leg.citation_info2$CELEX) 

n_distinct(Doc.citations.2$from) # celex
# 223
n_distinct(Doc.citations.2$to) # citation
# 917

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
#917

Doc.citations3 <- rbind(MPA.citations,Doc.citations.2) # combine the first order citation edge list with the second order citation edge list

network.attributes.final3 <- rbind(network.attributes.final,network.attributes.final2)

# some of the citations are both second and first order ciatations so lets make a new label... (color is orange and the name is reference 3)
both.cit2 <-
  network.attributes.final3 %>%
  group_by(CELEX) %>%
  mutate(n=n()) %>%
  filter(n>1) %>% # so these are either both or first & second order
  filter(!CELEX %in% both.pulls$CELEX) %>% #remove the twenty that are already both = so goes to 366
  filter(CELEX != "32011R0142" &
         CELEX != "32010D0631" &
         CELEX != "31998D2179") %>% # this three are now a both web result and second order. 
  mutate(pulled.from = "reference3") %>%
  mutate(color = 
           case_when(
             pulled.from == "reference3" ~ "orange" )) %>%
  distinct()%>%
  select(-n)


network.attributes.final4 <- network.attributes.final3[!network.attributes.final3$CELEX %in% both.cit2$CELEX,]
# 1246-179-179= 888 math check add up :)

network.attributes.final4 <- rbind(network.attributes.final4,both.cit2)
# 888+179 = 1067 

both.cit.originaloverlap<-
  network.attributes.final3 %>%
  group_by(CELEX) %>%
  mutate(n=n()) %>%
  filter(n>1) %>%
  filter(pulled.from == "both") %>%
  as.data.frame()

both.cit.originaloverlap <- as.character(both.cit.originaloverlap[1:15,1])

network.attributes.final4 <- 
  network.attributes.final4 %>%
  mutate(pulled.from = case_when(CELEX == "32011R0142" ~ "both", # change this one to both since it is second order referenced. 
                                 CELEX == "32010D0631" ~ "both",
                                 CELEX == "31998D2179" ~ "both",
                                 TRUE ~ pulled.from)) %>%
  mutate(color = case_when(CELEX == "32011R0142" ~ "#f8766d", 
                           CELEX == "32010D0631" ~ "#f8766d", 
                           CELEX == "31998D2179" ~ "#f8766d", 
                           TRUE ~ color)) %>%
  mutate(remove = case_when(CELEX %in% both.cit.originaloverlap & pulled.from == "reference2" ~ "remove",
                            TRUE ~ "keep")) %>%
  filter(remove == "keep") %>%
  distinct(CELEX, .keep_all = TRUE) # now remove the double 32013D1386
# 1067 - 15 - 3 = 1049

xx <-
  network.attributes.final4 %>%
  group_by(CELEX) %>%
  mutate(n=n()) %>%
  filter(n>1)

n_distinct(Doc.citations3$from)
# 279
n_distinct(Doc.citations3$to)
# 996 (273+917-179-15)

docs <- unique(Doc.citations3$from)
cit <-  unique(Doc.citations3$to)
xx <- as.data.frame(c(docs,cit))
xx <- distinct(xx)
# ok so both the document citataion df and the network attributes df have the same dimentions 


network.attributes.final4 %>%
  group_by( pulled.from) %>%
  summarise(n=n())
# both           23
# eurlex.web     53
# reference      74
# reference2    720
# reference3    179

network2 <- graph.data.frame(d=Doc.citations3, directed = TRUE, vertices = network.attributes.final4)
print(network2, e=TRUE, v=TRUE)

l2 <- layout.fruchterman.reingold(network2)

png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/query2.2nd.order.networkcitations.png",
    width = 1200, height = 1000)

plot(network2,
     edge.width=.5,
     edge.color=adjustcolor("gray", alpha.f = .65),
     vertex.size=2.5,
     vertex.label=NA,
     vertex.label.cex=1,
     vertex.label.family = "sans",
     edge.arrow.size=.5,
     edge.arrow.width=1,
     layout=l2
    
)



legend(x=-1.5,y=1.2,c("Both (result & citation)",
                       "Search result",
                       "Only first order citation",
                       "Only second order citation",
                       "First and second order citation"), 
       pch=21,
       col="#777777", 
       pt.bg=unique(V(network2)$color), 
       pt.cex=2.5, 
       cex=2.5, 
       bty="n", 
       ncol=1)

dev.off()


edges <- degree(network2)
sum(edges)
#5146
V(network2)
#1049

# Save files ---------------------------------------------------------------------

# second order citation data:
write.csv(Doc.citations3, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2secondordercit.edgelist.csv", row.names=FALSE)
write.csv(network.attributes.final4, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2secondordercit.verticesmetadata.csv", row.names=FALSE)
saveRDS(network2, file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2secondordercit.network.rds")


# Exploring Eurovoc terms -----------------------------------------------


# Making a EuroVoc term co-occurance for the seed and citations!

# get the network attributes:

Order1.docs <- read.csv("WP4/Policy_Interactions/data/04.Q2firstordercit.verticesmetadata.csv")
Order2.docs <- read.csv("WP4/Policy_Interactions/data/04.Q2secondordercit.verticesmetadata.csv")


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
#329 --> all good :)

n_distinct(Order2.docsdescript$CELEX)
# 1049 --> all good :)

#how many labels?
n_distinct(Order1.docsdescript$labels)
#678
n_distinct(Order2.docsdescript$labels)
#1482


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
write.csv(Order1.term.pairs, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C1term.edgelist.csv", row.names=FALSE)
write.csv(Order1.final.attributes, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C1term.verticesmetadata.csv", row.names=FALSE)
saveRDS(Order1.EuroVoc.network, file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C1.termnetwork.rds")

# 2nd order citations
write.csv(Order2.term.pairs, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C2term.edgelist.csv", row.names=FALSE)
write.csv(Order2.final.attributes, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C2term.verticesmetadata.csv", row.names=FALSE)
saveRDS(Order2.EuroVoc.network, file =  "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C2.termnetwork.rds")

# Archival code for EuroVoc graphics --------------------------------------
# (graphics we actually use are in the network stats rscript

#very helpful document for network vizualizations 
#http://www.kateto.net/wp-content/uploads/2015/06/Polnet%202015%20Network%20Viz%20Tutorial%20-%20Ognyanova.pdf


#l <- layout.fruchterman.reingold(network3)
#l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

#plot(network3,
#     edge.width=E(network3)$n*1,
##     edge.color="grey",
#     vertex.size=1,
#     vertex.label.cex=(degree(network3)/sum(degree(network3))*100),
#     vertex.label.color=V(network3)$color,
#     vertex.shape="none",
#     rescale = TRUE,
#ylim=c(-1,1),xlim=c(-1,1)
     # trying this layout based on pdf above...
#)

# O.K. so the network viz is more legable 
# I will only plot those that are the median or above edges

#edges <- degree(network3)
#sum(edges)

#V(network3)
#I1 <-
 # term.pairs %>%
 # group_by(item1) %>%
#  summarise(n=n())
#I2 <-
#  term.pairs %>%
#  group_by(item2) %>%
#summarise(n=n())

#I3 <- full_join(I1,I2, by=c("item1"="item2")) %>%
#  mutate(n.x = replace_na(n.x,0),
#n.y = replace_na(n.y,0))%>%
#  mutate(edge.number = n.x+n.y )

#summary(I3$edge.number)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.00    6.00    9.00   13.07   15.00  175.00

#this tutorial was helpful for this vizualization
#https://tm4ss.github.io/docs/Tutorial_5_Co-occurrence.html#4_Visualization_of_co-occurrence
#https://kateto.net/wp-content/uploads/2016/06/Polnet%202016%20R%20Network%20Visualization%20Workshop.pdf
#edges.remove <- V(network3)[degree(network3)<9]
#degree(network3)
#graphNetwork <-  igraph::delete.vertices(network3,edges.remove) 
#degree(graphNetwork)

#n_distinct(V(graphNetwork)$MT)

#library("viridis")   

#colors <- inferno(54)
#colors <- colors[-1:-5]
#V(graphNetwork)$color <- colors[as.numeric(as.factor(V(graphNetwork)$MT))]

#dist <- seq(-.025,0.25, by=.0024)
#dist <- rep(c(0.18, -0.18), length.out = 226)
#try to jitter the labels a little to avoid overlap 
#V(graphNetwork)$dist <- dist[as.numeric(as.factor(V(graphNetwork)$name))]


#l2 <- layout.fruchterman.reingold(graphNetwork)

#plot(graphNetwork,
     #edge.width=E(graphNetwork)$n,
    # edge.color=adjustcolor("gray", alpha.f = .5),
     #vertex.size=2,
    # vertex.label.cex=(degree(graphNetwork)/sum(degree(graphNetwork))*125), # label size is equiv. to percent of edges associated to the word out of total edges
   #  vertex.label.color=V(graphNetwork)$color,
  #   vertex.shape="none",
 #    rescale = TRUE,
#     ylim=c(-.8,.85),xlim=c(-.9,.9),
   #  layout = l2,
  #   vertex.label.family = "sans",
 #    vertex.label.dist = V(graphNetwork)$dist
     # trying this layout based on pdf above...
#)

#legend(x=.45,y=-.15,unique(V(graphNetwork)$MT)[-27], 
  #     pch=21,
  #     col="#777777", 
  #     pt.bg=unique(V(graphNetwork)$color), 
  #     pt.cex=2, 
  #     cex=1, 
 #      bty="n", # no box around the legen 
 #      ncol=2)



#edges <- degree(graphNetwork)
#sum(edges)

#V(graphNetwork)






