
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
EU.mpa.termsearch<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_EUmpa.searchterm.CELEX.csv")


document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

#---------------------------------------------------------------------------
#------------- This is the analysis on the second search query  ------------
#----------------------- 18 terms all diff types of ------------------------
#-------------------------- MPA Designation names --------------------------
#---------------------------------------------------------------------------

# lets join the new mpa search documents to their associated document data
EU.mpa.termsearch.data <-
  EU.mpa.termsearch %>%
  filter(CELEX != "32021R0092") %>% #removing this celex for the same reasoning as in 03 Rscript
  filter(CELEX != "32006R1967R(01)") %>% #removing this celex bc it is a Corrigendum to a regulation that was already pulled and it is tech. not within the legal act types
  left_join(., document.key.df, by = c("CELEX"="celex", "resource.type")) %>%
  select(-work,-type) # we dont need this info anymore

EU.mpa.termsearch.data %>%
  #distinct(CELEX, .keep_all = TRUE) %>% 
  group_by(search.term) %>%
  summarise(n= n_distinct(CELEX))

#    search.term                      n
# barcelona convention*?             36
# birds directive*?                  36
# habitats directive*?               58
# helcom*?                           14
# marine protected area*?            24
# ospar*?                            27
# ramsar site*?                       1
# site of community importance*?      4
# sites of community importance*?    10
# special areas of conservation*?    23
# special protection area*?          29
# specially protected area*?         10
# world heritage site*?               2


n_distinct(EU.mpa.termsearch.data$CELEX)
#[1] 178
# The remember the dim are now larger due to some documents having multiple terms-labels etc. 

EU.mpa.termsearch.data %>%
  group_by(resource.type) %>%
  summarise(n = n_distinct(CELEX)) %>%
  summarise(total = sum(n))
# DEC              41
# DIR              16
# OPIN             83
# RECO              3
# REG              35

# how many unique label terms?
n_distinct(EU.mpa.termsearch.data$labels)
# 403

# how many unique thems?
n_distinct(EU.mpa.termsearch.data$MT)
# 80
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
ggsave("WP4/Policy_Interactions/Results/Q2.overtime.png")

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
# baltic sea protected area (helcom)                                             163
# marine protected area (ospar)                                                  441
# ramsar site, wetland of international importance                               237
# sites of community importance (habitats directive)                             481
# special areas of conservation (habitats directive)                            1388
# special protection area (birds directive)                                      843
# specially protected area (cartagena convention)                                  7
# specially protected areas of mediterranean importance (barcelona convention)    24
# unesco-mab biosphere reserve                                                    11
# world heritage site (natural or mixed)                                           9

EU.mpa.char %>%
  filter(DESIG_ENG == "sites of community importance (habitats directive)" |
         DESIG_ENG == "special areas of conservation (habitats directive)") %>%
  summarise(n=n_distinct(mpa))


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
  citation.info %>%
  filter(CELEX %in% EU.mpa.termsearch.data$CELEX)
#32

#both.pulls2 <-
#  network.attributes %>%
#  group_by(CELEX) %>%
#  summarise(n=n()) %>%
#  filter(n>1) 
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
# 381

# Now we have edge df
# "from"  = Celex (is the first column)
# "to"= citation celex (second column)
MPA.citations <-
  MPA.citations %>%
  rename("from"="CELEX",
         "to"="citationcelex")

n_distinct(MPA.citations$from)
# 85

docs <- unique(MPA.citations$to)
cit <-  unique(MPA.citations$from)
xx <- as.data.frame(c(docs,cit))
xx <- distinct(xx) #381 documents
# ok vertices df and attribute df dim are the same :) 

network <- graph_from_data_frame(d=MPA.citations, directed = TRUE, vertices = network.attributes.final)
print(network, e=TRUE, v=TRUE)


l <- layout.fruchterman.reingold(network)
#l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

labels <- network.attributes.final[1:32,1]

labels2 <- rep(NA,time=349)
labels3 <- c(labels,labels2)
V(network)$label <- labels3 

png(file = "WP4/Policy_Interactions/Results/query2.1st.order.networkcitations.png",
    width = 1000, height = 1000)


plot(network,
     edge.width=.5,
     vertex.size=3.5,
     vertex.label.family = "sans",
      vertex.label=NA,
     vertex.label.cex=.765,
     vertex.label.color = "black",
     edge.arrow.size=.5,
     edge.arrow.width=1,
     rescale=TRUE,
     layout=l,# trying this layout based on pdf above...
     #edge.curved=.1
)
#legend(x=-1.7,y=1.2,c("32008L0056: Marine Strategy Framework Directive",
#                        "32000L0060: Directive a framework for Community action in the field of water policy",
#                        "31992L0043: Habitats Directive" ,
#                        "32009L0147: Birds Directive" ,
#                        "32008L0099: Directive on the protection of the environment through criminal law"  ,
#                        "32011L0092: Directive on the assessment of the effects of certain public and private projects on the environment", 
#                        "32009L0031: Directive on the geological storage of carbon dioxide & amending CD 85/337/EEC, EP & CD 2000/60/EC, 2001/80/EC, 2004/35/EC, 2006/12/EC, 2008/1/EC & Reg. (EC) No 1013/2006",  
#                        "32006R1967: Reg. concerning management measures for the sustainable exploitation of fishery resources in the Mediterranean Sea, amending Reg. (EEC) No 2847/93 & repealing Reg. (EC) No 1626/94" ,
#                        "32004R0724: Reg. amending Reg. (EC) No 1406/2002 establishing a European Maritime Safety Agency" ,
#                        "32010R1089: Reg. implementing Directive 2007/2/EC of the European Parliament and of the Council as regards interoperability of spatial data sets and services", 
#                        "31997R0338: Reg. on the protection of species of wild fauna and flora by regulating trade therein"
 #                      ),
#       cex=.65,
#       ncol=1,
#       col="#777777",
#       bty="n", # no box around the legen 
#       
#)
#legend(x=-1.7,y=-1,c("32013R1380: Reg. on the CFP, amending CR (EC) No 1954/2003 and 1224/2009 and repealing CR (EC) No 2371/2002 and 639/2004 and CD 2004/585/ECs",
#                      "32014R0508: Reg.on the European Maritime and Fisheries Fund and repealing CR (EC) No 2328/2003, No 861/2006, No 1198/2006 and No 791/2007 and Reg. (EU) No 1255/2011",
#                      "31999D0800: Barcelona Convention" , 
#                      "31999D0801: Dec. accepting amendments to the Protocol for the protection of the Mediterranean Sea against pollution from land-based sources (Barcelona Convention)", 
#                      "32013D0005: Dec. on the accession of the EU to the Protocol for the Protection of the Mediterranean Sea against pollution resulting from exploration and exploitation of the continental shelf and the seabed and its subsoil",
#                      "32009D0089: Dec. on the signing of the Protocol on Integrated Coastal Zone Management in the Mediterranean to the Convention for the Protection of the Marine Environment and the Coastal Region of the Mediterranean ",  
#                      "32002D1600: Dec. laying down the Sixth Community Environment Action Programme",  
#                      "32000D0340: Dec. approval of the new Annex V to the Convention for the Protection of the Marine Environment of the North-East Atlantic on the protection and conservation of the ecosystems and biological diversity of the maritime area",
#                      "52018AE2960: Opin. of the European Economic and Social Committee on ‘Proposal for a Regulation of the European Parliament and of the Council on the alignment of reporting obligations in the field of environment policy",
#                      "52017AE2820: Opin. of the European Economic and Social Committee on Commission Notice on Access to Justice in Environmental Matters"
#),
#cex=.65,
#ncol=1,
#col="#777777", 
#bty="n", # no box around the legen #

#)



#legend(x=-1,y=-1,c("Both (result & citation)",
 #                    "Search result",
 #                    "First order citation"),
 #      pch=21,
 #      col="#777777", 
 #      pt.bg=unique(V(network)$color), 
  #     pt.cex=2, 
 #      cex=1, 
 #      bty="n", # no box around the legen 
 #      ncol=1)
dev.off()
# blue are documents referenced within text
# green are those pulled from out MPA eurlex search
# red/pink are those that were pulled in the MPA search and also referenced within other documents pulled
edges <- degree(network)
sum(edges)

V(network)

# Save files ---------------------------------------------------------------------


# first order citation data:
write.csv(MPA.citations, file = "WP4/Policy_Interactions/data/03.Q2firstordercit.edgelist.csv", row.names=FALSE)
write.csv(network.attributes.final, file = "WP4/Policy_Interactions/data/03.Q2firstordercit.verticesmetadata.csv", row.names=FALSE)
saveRDS(network, file =  "WP4/Policy_Interactions/data/03.Q2firstordercit.network.rds")



# Second order citations ---------------------------------------------------------------------------

indir.citation_info <- 
  MPA.citations %>%
  select(to) %>%
  left_join(.,document.key.df, by = c("to" = "celex"))


n_distinct(MPA.citations$to)
n_distinct(indir.citation_info$to)

Doc.citations.2 <-
  indir.citation_info %>%
  distinct(to,citationcelex) %>% # make sure no duplicate rows bc of multiple labeles/themes
  filter(!is.na(citationcelex)) %>%
  rename("from" = "to",
         "to" = "citationcelex")

n_distinct(Doc.citations.2$from) # celex
#272
n_distinct(Doc.citations.2$to) # citation
#1442

#now make sure citations (to) are only legislation documents 
leg.citation_info2 <- 
  document.key.df %>%
  filter(celex %in% Doc.citations.2$to) %>% 
  distinct(celex, .keep_all = TRUE) %>%
  select(resource.type,celex,date,force) %>%
  rename(CELEX = celex) %>%
  mutate(pulled.from = "reference2") # label them as second order references

#do the same to the edge list
Doc.citations.2 <- 
  Doc.citations.2 %>%
  filter(to %in% leg.citation_info2$CELEX) 

n_distinct(Doc.citations.2$from) # celex
# 248
n_distinct(Doc.citations.2$to) # citation
# 935

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

Doc.citations3 <- rbind(MPA.citations,Doc.citations.2) # combine the first order citation edge list with the second order citation edge list

network.attributes.final3 <- rbind(network.attributes.final,network.attributes.final2)

# some of the citations are both second and first order ciatations so lets make a new label... (color is orange and the name is reference 3)
both.cit2 <-
  network.attributes.final3 %>%
  group_by(CELEX) %>%
  mutate(n=n()) %>%
  filter(n>1) %>% # so these are either both or first & second order
  filter(!CELEX %in% both.pulls$CELEX) %>% #380- (20 boths thus 40 rows as both and ref2) = 340
  filter(CELEX != "32010D0631" &
         CELEX != "32013D1386" &
         CELEX != "31998D2179") %>% # this three are now a both so in total there are 35 docs that will have duplicates later. 
  mutate(pulled.from = "reference3") %>%
  mutate(color = 
           case_when(
             pulled.from == "reference3" ~ "orange" )) %>%
  distinct()%>%
  select(-n)


network.attributes.final4 <- network.attributes.final3[!network.attributes.final3$CELEX %in% both.cit2$CELEX,]
# 1316-167-167= 982 math check add up :)

network.attributes.final4 <- rbind(network.attributes.final4,both.cit2)
# 982+167 = 1149 

both.cit.originaloverlap<-
  network.attributes.final3 %>%
  group_by(CELEX) %>%
  mutate(n=n()) %>%
  filter(n>1) %>%
  filter(pulled.from == "both") %>%
  as.data.frame()

both.cit.originaloverlap <- as.character(both.cit.originaloverlap[1:20,1])

network.attributes.final4 <- 
  network.attributes.final4 %>%
  mutate(pulled.from = case_when(CELEX == "32010D0631" ~ "both", # change this one to both since it is second order referenced. 
                                 CELEX == "32013D1386" ~ "both",
                                 CELEX == "31998D2179" ~ "both",
                                 TRUE ~ pulled.from)) %>%
  mutate(color = case_when(CELEX == "32010D0631" ~ "#f8766d", 
                           CELEX == "32013D1386" ~ "#f8766d", 
                           CELEX == "31998D2179" ~ "#f8766d", 
                           TRUE ~ color)) %>%
  mutate(remove = case_when(CELEX %in% both.cit.originaloverlap & pulled.from == "reference2" ~ "remove", # 
                           # CELEX == "32013R1380" & pulled.from == "reference2" ~ "remove",
                           # CELEX == "32014R0508" & pulled.from == "reference2" ~ "remove",
                            TRUE ~ "keep")) %>%
  filter(remove == "keep") %>%
  distinct(CELEX, .keep_all = TRUE) # now remove the double 32013D1386
# 1149 - 20 - 3 = 1126

n_distinct(Doc.citations3$from)
# 312
n_distinct(Doc.citations3$to)
# 1065

docs <- unique(Doc.citations3$from)
cit <-  unique(Doc.citations3$to)
xx <- as.data.frame(c(docs,cit))
xx <- distinct(xx)
# ok so both the document citataion df and the network attributes df have the same dimentions 

network2 <- graph.data.frame(d=Doc.citations3, directed = TRUE, vertices = network.attributes.final4)
print(network2, e=TRUE, v=TRUE)

l <- layout.fruchterman.reingold(network2)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

png(file = "WP4/Policy_Interactions/Results/query2.2nd.order.networkcitations.png",
    width = 1200, height = 1000)

plot(network2,
     edge.width=.5,
     edge.color=adjustcolor("gray", alpha.f = .5),
     vertex.size=3,
     vertex.label=NA,
     vertex.label.cex=.65,
     vertex.label.family = "sans",
     edge.arrow.size=.05,
     edge.arrow.width=1,
    # layout=l
    
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

V(network2)

# Save files ---------------------------------------------------------------------

# second order citation data:
write.csv(Doc.citations3, file = "WP4/Policy_Interactions/data/03.Q2secondordercit.edgelist.csv", row.names=FALSE)
write.csv(network.attributes.final4, file = "WP4/Policy_Interactions/data/03.Q2secondordercit.verticesmetadata.csv", row.names=FALSE)
saveRDS(network2, file =  "WP4/Policy_Interactions/data/03.Q2secondordercit.network.rds")


# Exploring Eurovoc terms -----------------------------------------------

# Eurovoc Term Co-occurrences: 
cleaned.labels <-
  EU.mpa.termsearch.data %>%
  distinct(CELEX,labels, .keep_all = TRUE)

n_distinct(cleaned.labels$MT)
# 70 themes
n_distinct(cleaned.labels$labels)
# 403 terms

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


final.attributes <- attributes3

n_distinct(final.attributes$MT)

network3 <- graph_from_data_frame(d=term.pairs, vertices = final.attributes, directed = FALSE)
class(network3)

#very helpful document for network vizualizations 
#http://www.kateto.net/wp-content/uploads/2015/06/Polnet%202015%20Network%20Viz%20Tutorial%20-%20Ognyanova.pdf


l <- layout.fruchterman.reingold(network3)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(network3,
     edge.width=E(network3)$n*1,
     edge.color="grey",
     vertex.size=1,
     vertex.label.cex=(degree(network3)/sum(degree(network3))*100),
     vertex.label.color=V(network3)$color,
     vertex.shape="none",
     rescale = TRUE,
     ylim=c(-1,1),xlim=c(-1,1)
     # trying this layout based on pdf above...
)

# O.K. so the network viz is more legable 
# I will only plot those that are the median or above edges

edges <- degree(network3)
sum(edges)

V(network3)

# Save ------------------------------------------------------------------

# term co-ocurances:
write.csv(term.pairs, file = "WP4/Policy_Interactions/data/03.Q2term.edgelist.csv", row.names=FALSE)
write.csv(final.attributes, file = "WP4/Policy_Interactions/data/03.Q2term.verticesmetadata.csv", row.names=FALSE)
saveRDS(network3, file =  "WP4/Policy_Interactions/data/03.Q2.termnetwork.rds")

# ------------------------------------------------------------------------



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
edges.remove <- V(network3)[degree(network3)<9]
degree(network3)
graphNetwork <-  igraph::delete.vertices(network3,edges.remove) 
degree(graphNetwork)

n_distinct(V(graphNetwork)$MT)

library("viridis")   

colors <- inferno(54)
#colors <- colors[-1:-5]
V(graphNetwork)$color <- colors[as.numeric(as.factor(V(graphNetwork)$MT))]

dist <- seq(-.025,0.25, by=.0024)
dist <- rep(c(0.18, -0.18), length.out = 226)
#try to jitter the labels a little to avoid overlap 
V(graphNetwork)$dist <- dist[as.numeric(as.factor(V(graphNetwork)$name))]


l2 <- layout.fruchterman.reingold(graphNetwork)

plot(graphNetwork,
     edge.width=E(graphNetwork)$n,
     edge.color=adjustcolor("gray", alpha.f = .5),
     vertex.size=2,
     vertex.label.cex=(degree(graphNetwork)/sum(degree(graphNetwork))*125), # label size is equiv. to percent of edges associated to the word out of total edges
     vertex.label.color=V(graphNetwork)$color,
     vertex.shape="none",
     rescale = TRUE,
     ylim=c(-.8,.85),xlim=c(-.9,.9),
     layout = l2,
     vertex.label.family = "sans",
     vertex.label.dist = V(graphNetwork)$dist
     # trying this layout based on pdf above...
)

legend(x=.45,y=-.15,unique(V(graphNetwork)$MT)[-27], 
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






