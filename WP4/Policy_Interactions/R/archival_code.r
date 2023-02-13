
# archival script code and notes --------------

# 01.load.R

# Notes on this function and working through this: 

# We first tried the query term: marine protected area* (no quotation marks) with the function exactly = FALSE
# however everytime we run the funtion call the results are inconsistent. Sometimes there is more or less document results 
# in addition there seems to be no clear pattern of the inconsistency

# For example:
# try without *,
# first try: with exactly=FALSE without * --> 1108 obs 
# second try: with exactly=FALSE without * --> 1108 obs
# third try: with exactly=FALSE without * --> 1108 obs
# (ok this happened on Friday Sep 16th) --> re-did it Monday to officailly make the new df and...
# 1098 results... a second time now it was back to 1108... athird time it was 1088 

# we also tried some other terms f.x.
#"marine protected site"
# first try --> 390 obs
# second try --> 390 obs

#"marine protected" --> friday it was 1,465 and  monday it was 1,485

#also not wild card ? at the end is not useful --> when I use it it produces NA results... I think it is taking it as a literal part of the query term, not a wild card

# O.K. after this whole issue we decided that exactly needs to be = TRUE and "marine protected" to encompass all was of interpreting a protected area within leg. (i.e. site, area, species)

# after saving: 
# All these data were pulled from query, "cleaned", and saved in this script on Sep 22nd, 2022
# this data was updated Jan 3rd to ensure better data cleansing

# script 03_query1_MPA.R ----------------------

# Archival code for EuroVoc graphics 
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

# 04.Q2. Rscript ---------------------

# Archival code for EuroVoc graphics 
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

# 06.total.textdata.prep.r ---------------------------


# Archived ------------------------------------

# **Note from the stm CRAN manual!
# "we don't run prepCorpus here because we don't want to drop any words- we want every word that showed up in the old documents."

# Q1.newdocs <- alignCorpus(new=Q1C2.textprocessed, old.vocab=Q1.stm$vocab)
# info on what was done: 
# Removing 14628 Documents with No Words (in our case sentences)
# Your new corpus now has 192222 documents (sentences), 3968 non-zero terms of 3980 total terms in the original set. 
# 58779 terms from the new data did not match.
# This means the new data contained 99.7% of the old terms
# and the old data contained 6.3% of the unique terms in the new data. 
# You have retained 3499112 tokens of the 3956192 tokens you started with (88.4%)


#Q1C2.topi.pred <- 
#  fitNewDocuments(model=Q1.stm, 
#                  documents=Q1.newdocs$documents, 
#                  newData=Q1.newdocs$meta,
#                  origData=Q1.text$meta)

#Q1C2.doctopic.pred <- Q1C2.topi.pred$theta

#Q1.doc.names <- as.data.frame(names(Q1.newdocs$documents))

# lets make this into a long df
#Q1C2.Doctopic.longdf <-
##  Q1C2.doctopic.pred %>%
#  as.data.frame() %>%
#  rownames_to_column(var = "document_sentence") %>%
#  cbind(., Q1.newdocs$meta$CELEX,Q1.doc.names) %>%
#  pivot_longer(.,
#               cols = 2:68,
#               names_to = "topic", 
#               values_to = "proportion") %>%
#  mutate(topic = str_replace_all(topic, "V", "topic"),
#         percent.doc = proportion *100) 

#n_distinct(Q1C2.Doctopic.longdf$`Q1.newdocs$meta$CELEX`)
# 583

#summary(Q1C2.Doctopic.longdf$percent.doc)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1708  0.5688  1.4925  1.4362 99.4407 


# **Note from the stm CRAN manual!
# "we don't run prepCorpus here because we don't want to drop any words- we want every word that showed up in the old documents."

#Q2.newdocs <- alignCorpus(new=Q2C2.textprocessed, old.vocab=Q1.stm$vocab)
#Removing 18292 Documents with No Words 
#Your new corpus now has 316256 documents, 3971 non-zero terms of 3980 total terms in the original set. 
#93832 terms from the new data did not match.
#This means the new data contained 99.8% of the old terms
#and the old data contained 4.1% of the unique terms in the new data. 
#You have retained 5660154 tokens of the 6472359 tokens you started with (87.5%)

#Q2C2.topi.pred <- 
#  fitNewDocuments(model=Q2.stm, 
#                  documents=Q2.newdocs$documents, 
#                  newData=Q2.newdocs$meta,
#                  origData=Q2.stm$meta)#

#Q2C2.doctopic.pred <- Q2C2.topi.pred$theta

#Q2.doc.names <- as.data.frame(names(Q2.newdocs$documents))

# lets make this into a long df
#Q2C2.Doctopic.longdf <-
#  Q2C2.doctopic.pred %>%
#  as.data.frame() %>%
#  rownames_to_column(var = "document_sentence") %>%
#  cbind(., Q2.newdocs$meta$CELEX,Q2.doc.names) %>%
#  pivot_longer(.,
#               cols = 2:68,
#               names_to = "topic", 
#               values_to = "proportion") %>%
#  mutate(topic = str_replace_all(topic, "V", "topic"),
#         percent.doc = proportion *100)


#n_distinct(Q2C2.Doctopic.longdf$`Q2.newdocs$meta$CELEX`)


#write.csv(Q1C2.Doctopic.longdf, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/09.Q1C2.Doctopic.longdf.csv", row.names=FALSE)
#write.csv(Q2C2.Doctopic.longdf, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/09.Q2C2.Doctopic.longdf.csv", row.names=FALSE)

#07.networkstats.R --------------------------------------------


# archival wordcloud cluster plots ----------------

#as.data.frame(cbind(V(Q1.network.updated1)$color, V(Q1.network.updated1)$membership)) %>%
#    mutate(V2 = as.numeric(V2)) %>%
#   distinct(., .keep_all=TRUE) %>%
#   right_join(.,Q1.terms.meta2, by = c("V2" = "membership")) %>%
#   left_join(.,degrees, by = c("labels"= "rowname")) %>%
#    mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
#    mutate(V2= as.factor(V2)) %>%
#    ggplot(., aes(label = labels, 
#                  size = degree,
# x=V2,
#                  color = V2)) +
#    geom_text_wordcloud(shape = "circle", eccentricity = 1) +
#    scale_size_area(max_size = 12) +
#    theme_minimal() +
#    theme(strip.text.x = element_text(size = 15, face = "bold")) + 
#    scale_color_manual(breaks = c("Cluster 1", "Cluster 2", "Cluster 3",
#                         "Cluster 4", "Cluster 5", "Cluster 6",
#                          "Cluster 7"),
#                values=c("#1B9E77", 
#                          "#D95F02", 
#                           "#7570B3", 
#                            "#E7298A", 
#                             "#66A61E", 
#                              "#E6AB02", 
#                               "#A6761D"))
#ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/Q1.termclusters.png")

#as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
#    mutate(V2 = as.numeric(V2)) %>%
#    distinct(., .keep_all=TRUE) %>%
#    right_join(.,Q2.terms.meta2, by = c("V2" = "membership")) %>%
#  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
#   mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
#    mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
#                                  "Cluster 4", "Cluster 5", "Cluster 6",
#                                   "Cluster 7","Cluster 8","Cluster 9",
#                                    "Cluster 10","Cluster 11", "Cluster 12", "Cluster 13",
#                                     "Cluster 14", "Cluster 15", "Cluster 16",
#                                      "Cluster 17","Cluster 18","Cluster 19",
#                                       "Cluster 20", "Cluster 21"))) %>%
#    ggplot(., aes(label = labels, 
#                  size = degree,
#                  color = V22)) +
#    geom_text_wordcloud(shape = "circle",eccentricity = 1) +
#    scale_size_area(max_size = 50) +
#    theme_minimal() +
#    theme(line = element_blank(),
#          text = element_blank(),
#          title = element_blank()) + 
#    scale_color_manual(breaks = c("Cluster 1", "Cluster 2","Cluster 3",
#                            "Cluster 4", "Cluster 5", "Cluster 6",
#                             "Cluster 7","Cluster 8","Cluster 9",
#                              "Cluster 10","Cluster 11", "Cluster 12", "Cluster 13",
#                               "Cluster 14", "Cluster 15", "Cluster 16",
#                                "Cluster 17", "Cluster 18","Cluster 19",
#                                 "Cluster 20", "Cluster 21"),
# clusters not in the network... 2, 3, 13, 14, 15 but will still need a color that is not present in the network for the word clouds 

# values=c("#E6AB02", #1
#           "black",   #2
#            "purple",  #3
#             "#A6CEE3", #4
#              "#666666", #5
#               "#1F78B4", #6 
#                "#FB9A99", #7
#                 "#B2DF8A", #8
#                  "#1B9E77", #9
#                   "#66A61E",#10
#                    "#7570B3",#11 
#                      "#FF7F00",#12 
#                     "#00cccc",#13 
#                       "#cc3333",#14
#                        "#ff6600",#15
#                         "#E7298A",#16
#                           "#D95F02",#17
#                          "#6A3D9A",#18
#                            "#CAB2D6",#19
##                              "#E31A1C",#20
#                               "#A6761D" #21
#                      ))

#ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/Q2.termclusters2.png",
#       width = 35, height = 35)



# cluster plot wordcloud

#as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
#  mutate(V2 = as.numeric(V2)) %>%
#  distinct(., .keep_all=TRUE) %>%
#  right_join(.,Q2.terms.meta2, by = c("V2" = "membership")) %>%
#  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
#  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
#  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
#                                     "Cluster 4", "Cluster 5", "Cluster 6",
#                                     "Cluster 7","Cluster 8","Cluster 9"))) %>%
#  ggplot(., aes(label = labels, 
#                size = degree,
#                color = V22,
#                x=V22)) +
#  geom_text_wordcloud(shape = "circle",eccentricity = 1) +
#  scale_size_area(max_size = 37) +
#  theme_minimal() +
#  theme(line = element_blank(),
#        text = element_blank(),
#        title = element_blank()) + 
#  facet_wrap(~V22, ncol = 3,scales = "free")+
#  scale_color_manual(breaks = c("Cluster 1", "Cluster 2","Cluster 3",
#                                "Cluster 4", "Cluster 5", "Cluster 6",
#                                "Cluster 7","Cluster 8","Cluster 9"),
# cluster 5not in the network...but will still need a color that is not present in the network for the word clouds 

##                     values=c("#E6AB02", #1
#                              "#1B9E77",   #2
#                              "#D95F02",  #3
#                              "#E7298A", #4
#                              "black", #5
#                              "#66A61E", #6 
#                              "#7570B3", #7
#                              "#A6761D", #8
#                              "#666666" #9
#                              
#                )) 

#ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/Q2.termclusterspt1.png",
#    width = 50, height = 50,
#     limitsize = FALSE)


#dev.off()


#as.data.frame(cbind(V(Q1C1.network.updated1)$color, V(Q1C1.network.updated1)$membership)) %>%
#  mutate(V2 = as.numeric(V2)) %>%
#  distinct(., .keep_all=TRUE) %>%
#  right_join(.,Q1C1.terms.meta2, by = c("V2" = "membership")) %>%
# left_join(.,degrees, by = c("labels"= "rowname")) %>%
#mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
#  mutate(V2= as.factor(V2)) %>%
#  ggplot(., aes(label = labels, 
#                size = degree,
#                x=V2,
#                color = V2)) +
#  geom_text_wordcloud(shape = "circle", eccentricity = 1) +
#  scale_size_area(max_size = 25) +
#  theme_minimal()  +
#  theme(line = element_blank(),
#        text = element_blank(),
#        title = element_blank())+
#  theme(strip.text.x = element_text(size = 15, face = "bold")) + 
#  scale_color_manual(breaks = c("Cluster 1", "Cluster 2", "Cluster 3",
#                                "Cluster 4", "Cluster 5", "Cluster 6",
#                                "Cluster 7", "Cluster 8", "Cluster 9"),
#                     values=c("#E41A1C", 
#                              "#377EB8", 
#                              "#4DAF4A", 
#                              "#984EA3", 
#                              "#FF7F00", 
#                              "#FFFF33",
#                              "#A65628",
#                              "#F781BF",
#                              "#999999"))


# 10_topic.document.network.r --------------------------

###########################################

# just wanted to try out this idea but looks very very funky and messy...

#Q1C1.same.topics <- 
#  Q1C1.edgelist.topics %>%
#  ungroup%>%
#  filter(same.topic == TRUE)%>%
#  select(-from.topic) %>%
#  group_by(from, to, same.topic) %>%
#  summarise(n.topics = n_distinct(to.topic))

#Q1C1.NOTsame.topics <- 
#  Q1C1.edgelist.topics %>%
#  ungroup%>%
#  filter(same.topic == FALSE)%>%
#  select(-from.topic) %>%
#  group_by(from, to, same.topic) %>%
#  summarise(n.topics = n_distinct(to.topic))

#new.edgelist <-
#  rbind(Q1C1.same.topics,Q1C1.NOTsame.topics)

#network <- graph_from_data_frame(new.edgelist, directed = TRUE) 
#V(network)

#E(network)$color[E(network)$same.topic == TRUE] <- 'green'
#E(network)$color[E(network)$same.topic == FALSE] <- 'red'


#l <- layout.fruchterman.reingold(network)

#plot(network,
#     vertex.label=NA,
#     edge.width =E(network)$n.topics/2,
#     edge.color= E(network)$color,
#     vertex.size= 2,
#     layout = l)


# 11_term clusters.r Archival -----------------
# we decided not to do these
# degree.out to cluster prevalence 

set.seed(01)
glmd<-glmmTMB(degree.out~cluster.f,data=subset(Q1.clus.df,cluster.f!="2"&cluster.f!="9"&cluster.f!="10"),family="nbinom2") 
glmdb<-glmmTMB(degree.out~cluster.f,ziformula=~1,data=subset(Q1.clus.df,cluster.f!="2"&cluster.f!="9"&cluster.f!="10"),family="nbinom2") # ziformula for producing a structural zero

AIC(glmdb,glmd)
#glmdb 10 2748.543 # lower AIC
#glmd   9 2785.270

resd.glmdb<-simulateResiduals(glmdb)
plot(resd.glmdb) 
Anova(glmdb)
summary(glmdb)

predd<-ggpredict(glmdb,terms=c("cluster.f"))
plot(predd)

Q1.deg.out.plot <-
  as.data.frame(predd) %>%
  mutate( name = factor(case_when(x == "1" ~ "Environmental protection and EU programme",
                                  x == "3" ~ "Fisheries sustainable development",
                                  x == "4" ~ "Health/market standards and controls",
                                  x == "5" ~ "Economic and social development",
                                  x == "6" ~ "Information and data",
                                  x == "7" ~ "Admin. services, support, and transparency",
                                  x == "8" ~ "EU competativeness and economics",
                                  x == "11" ~ "Mutual recognition principle and admin. cooperation")),
          levels = c("Environmental protection and EU programme",
                     "Fisheries sustainable development",
                     "Health/market standards and controls",
                     "Economic and social development",
                     "Information and data",
                     "Admin. services, support, and transparency",
                     "EU competativeness and economics",
                     "Mutual recognition principle and admin. cooperation"))%>%
  ggplot(., aes(x=predicted, y=fct_inorder(name), color=x)) +
  geom_point() +
  geom_errorbar(aes(y=name, xmin=conf.low, xmax=conf.high), width=.1) +
  theme_minimal() +
  ylab("") +
  xlab("predicted degree out") +
  theme(legend.position = "none") +
  ggtitle("(B) Degree out")
#economic and social development has higher degree out

# degree.in to cluster prevalence
set.seed(01)
glmdi<-glmmTMB(degree.in~cluster.f,ziformula=~1,data=subset(Q1.clus.df,cluster.f!="2"&cluster.f!="9"&cluster.f!="10"),family="nbinom2") 
glmdib<-glmmTMB(degree.in~cluster.f,data=subset(Q1.clus.df,cluster.f!="2"&cluster.f!="9"&cluster.f!="10"),family="nbinom2") 
glmdib2<-glmmTMB(degree.in~cluster.f,offset=cluster.prop,data=subset(Q1.clus.df,cluster.f!="2"&cluster.f!="9"&cluster.f!="10"),family="nbinom2") 
glmdib<-glmmTMB(degree.in~1,data=subset(Q1.clus.df,cluster.f!="2"&cluster.f!="9"&cluster.f!="10"),family="nbinom2") 

AIC(glmdib,glmdi,glmdib2)
# glmdib   9 4922.199 # lowest
# glmdi   10 4924.199
# glmdib2  9 5064.740

resdi<-simulateResiduals(glmdib)
plot(resdi)

hist(Q1.clus.df$degree.in,50)

###we need to fix this

# degree.out to cluster prevalence  

set.seed(01)
glmd<-glmmTMB(degree.out~cluster.f,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="nbinom2") 
glmdb<-glmmTMB(degree.out~cluster.f,ziformula=~1,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="nbinom2") # ziformula for producing a structural zero
AIC(glmdb,glmd)
#df      AIC
#glmdb 13 5804.548 --> lower AIC
#glmd  12 5838.178
resd<-simulateResiduals(glmdb)
plot(resd)

predd<-ggpredict(glmdb,terms=c("cluster.f"))
plot(predd)
#cluster 5 has higher degree out

Q2.deg.out.plot <-
  as.data.frame(predd)%>%
  mutate( name = factor(case_when(x == "1" ~ "EU harmonization of environmental protections and information exchange",
                                  x == "2" ~ "EU & member state sustainable development programmes",
                                  x == "4" ~ "Power of the institutions",
                                  x == "5" ~ "EU budget",
                                  x == "6" ~ "Agriculture",
                                  x == "7" ~ "Health/market standards and controls",
                                  x == "8" ~ "EU cooperation and data", 
                                  x == "9" ~ "EU competitiveness and financing",
                                  x == "10" ~ "Single markets",
                                  x == "11" ~ "Technical standards and regulations",
                                  x == "14" ~ "Cluster 14")),
          levels = c("EU harmonization of environmental protections and information exchange",
                     "EU & member state sustainable development programmes",
                     "Power of the institutions",
                     "EU budget",
                     "Agriculture",
                     "Health/market standards and controls",
                     "EU cooperation and data",
                     "EU competitiveness and financing",
                     "Single markets",
                     "Technical standards and regulations",
                     "Cluster 14"))%>%
  ggplot(., aes(x=predicted, y=fct_inorder(name), color=x)) +
  geom_point() +
  geom_errorbar(aes(y=name, xmin=conf.low, xmax=conf.high), width=.1) +
  theme_minimal() +
  ylab("") +
  xlab("predicted degrees out") +
  theme(legend.position = "none",
        text = element_text(size = 20)) +
  ggtitle("(B) Degree out")

# degree.in to cluster prevalence 

set.seed(01)
glmdi<-glmmTMB(degree.in~cluster.f,ziformula=~1,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="nbinom2") 
glmdib<-glmmTMB(degree.in~cluster.f,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="nbinom2") 
glmdib2<-glmmTMB(degree.in~cluster.f,offset=cluster.prop,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="nbinom2") 

AIC(glmdib,glmdi,glmdib2)
#        df      AIC
#glmdib  12 8432.091
#glmdi   13 8434.091
#glmdib2 12 8537.601

glmdib<-glmmTMB(degree.in~cluster.f,data=subset(clus.df,cluster.f!="3"),family="nbinom2") #the disconnected clusters

resdi<-simulateResiduals(glmdib)
plot(resdi)

###we need to fix this

predd<-ggpredict(glmdib,terms=c("cluster.f"))
plot(predd)

#cluster 4 has higher degree in









