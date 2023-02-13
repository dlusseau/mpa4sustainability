
# Clear work space ---------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ----------------------------------------------------------

library("dplyr")
library("igraph")
library("tibble")
library("RColorBrewer")
library("ggplot2")
library("ggwordcloud")
library("patchwork")

# Define functions --------------------------------------------------------

# No defined function for this script

# Load data ---------------------------------------------------------------

# ------------ Query 1 (Q1) --------- #

# -- citations -- #
Q1.net1st<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1firstordercit.edgelist.csv")               # edge list
Q1.net1st.meta<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1firstordercit.verticesmetadata.csv")  # vertices meta data
Q1.net1st.graph<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1firstordercit.network.rds")           # the network object 

Q1.net2nd<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1secondordercit.edgelist.csv")              # edge list
Q1.net2nd.meta<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1secondordercit.verticesmetadata.csv") # vertices meta data
Q1.net2nd.graph<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1secondordercit.network.rds")          # the network object 

# -- terms co-occurance -- #

#citation order 1
Q1C1.terms<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C1term.edgelist.csv")                         # edge list
Q1C1.terms.meta<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C1term.verticesmetadata.csv")            # vertices meta data
Q1C1.terms.graph<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C1.termnetwork.rds")                     # the network object 

#citation order 2
Q1C2.terms<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C2term.edgelist.csv")                         # edge list
Q1C2.terms.meta<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C2term.verticesmetadata.csv")            # vertices meta data
Q1C2.terms.graph<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1C2.termnetwork.rds")                     # the network object 

# ----------- Query 2 (Q2) ---------- #

# -- citations -- #
Q2.net1st<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2firstordercit.edgelist.csv")               # edge list
Q2.net1st.meta<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2firstordercit.verticesmetadata.csv")  # vertices meta data
Q2.net1st.graph<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2firstordercit.network.rds")           # the network object 

Q2.net2nd<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2secondordercit.edgelist.csv")              # edge list
Q2.net2nd.meta<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2secondordercit.verticesmetadata.csv") # vertices meta data
Q2.net2nd.graph<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2secondordercit.network.rds")          # the network object 

# -- terms co-occurance -- #

#citation order 1
Q2C1.terms<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C1term.edgelist.csv")                         # edge list
Q2C1.terms.meta<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C1term.verticesmetadata.csv")            # vertices meta data
Q2C1.terms.graph<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C1.termnetwork.rds")                     # the network object 

#citation order 2
Q2C2.terms<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C2term.edgelist.csv")                         # edge list
Q2C2.terms.meta<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C2term.verticesmetadata.csv")            # vertices meta data
Q2C2.terms.graph<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/04.Q2C2.termnetwork.rds")                     # the network object 

# Network Stats ---------------------------------------------------------------

## Citations networks are directed unweighted networks
## Eurovoc term co-ocurrance networks are undirected weighted networks

# Query 1 ---------------------------------------------------------

# ------------------- First order citations -----------------------

# degree_in and degree_out
# "The degree of a vertex is its most basic structural property, the number of its adjacent edges." -- CRAN PDF
Q1.1st.degree.in<-degree(Q1.net1st.graph,mode="in")
Q1.1st.degree.out<-degree(Q1.net1st.graph,mode="out")

plot(Q1.1st.degree.in ~ Q1.1st.degree.out)

# betweenness
# "The vertex and edge betweenness are (roughly) defined by the (shortest paths) going through a vertex or an edge." -- CRAN PDF
# larger value means a greater bottleneck for the "control of information" passing between nodes.
Q1.1st.betweenness<-betweenness(Q1.net1st.graph,directed = TRUE,normalized=TRUE) # normalized so we can compare to different size networks later

plot(Q1.1st.degree.in ~ Q1.1st.betweenness)
plot(Q1.1st.degree.out ~ Q1.1st.betweenness )

# modules:
# as we have multiple components in the network we first need to identify components
# first we look for weakly connected component, so that any edge between clusters of text is considered as connecting the clusters

Q1.1st.comp.weak<-components(Q1.net1st.graph,mode="weak")

Q1.1st.df<-data.frame(name= V(Q1.net1st.graph)$name,
                        degree.in=Q1.1st.degree.in,
                        degree.out=Q1.1st.degree.out,
                        betweenness=Q1.1st.betweenness,
                        component=as.numeric(membership(Q1.1st.comp.weak))) 

rownames(Q1.1st.df) <- NULL

# save Query 1 1st order network stats:
write.csv(Q1.1st.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q1C1.networkdata.csv", row.names=FALSE)


head(Q1.1st.df)
#cluster_edge_betweenness because we deal with a directed network
# graph1st.cluster<-cluster_edge_betweenness(
# SUBSET(graph1st),
# directed = TRUE)

# membership(graph1st.cluster)
sort(table(Q1.1st.comp.weak$membership))

Q1.1st.df %>%
  filter( betweenness == max(betweenness))%>%
  select(name, betweenness)
#  32013R1380 0.009405111

Q1.1st.df %>%
  filter( degree.in == max(degree.in))%>%
  select(name, degree.in)
# 32013R1380        10

Q1.1st.df %>%
  filter( degree.out == max(degree.out))%>%
  select(name, degree.out)
# 32021R1139         33

# ------------------- Second order citations -------------------

# degree_in and degree_out
Q1.2nd.degree.in<-degree(Q1.net2nd.graph,mode="in")
Q1.2nd.degree.out<-degree(Q1.net2nd.graph,mode="out")

plot(Q1.2nd.degree.in ~ Q1.2nd.degree.out)

# betweenness
#graph1st.betweenness<-estimate_betweenness(graph1st,directed = TRUE,cutoff = -1)
Q1.2nd.betweenness<-betweenness(Q1.net2nd.graph,directed = TRUE,normalized=TRUE)

plot(Q1.2nd.degree.in ~ Q1.2nd.betweenness)
plot(Q1.2nd.degree.out ~ Q1.2nd.betweenness )

# module
## as we have multiple components in the network we first need to identify components
#first we look for weakly connected component, so that any edge between clusters of text is considered as connecting the clsuters

Q1.2nd.comp.weak<-components(Q1.net2nd.graph,mode="weak")

V(Q1.net2nd.graph)$component<-membership(Q1.2nd.comp.weak)

Q1.2nd.giant<-subgraph(Q1.net2nd.graph,vids=V(Q1.net2nd.graph)[V(Q1.net2nd.graph)$component==1])

Q1.2nd.giant.cluster<-cluster_edge_betweenness(Q1.2nd.giant, directed = TRUE)
membership(Q1.2nd.giant.cluster)
table(membership(Q1.2nd.giant.cluster))

Q1.2nd.df<-data.frame(name= V(Q1.net2nd.graph)$name,
                        degree.in=Q1.2nd.degree.in,
                        degree.out=Q1.2nd.degree.out,
                        betweenness=Q1.2nd.betweenness,
                        component=as.numeric(membership(Q1.2nd.comp.weak)))

rownames(Q1.2nd.df) <- NULL

write.csv(Q1.2nd.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q1C2.networkdata.csv", row.names=FALSE)

head(Q1.2nd.df)

plot(degree.in~degree.out,data=Q1.2nd.df)

#Q1.1st.df$degree.in.2nd<-Q1.2nd.df$degree.in[match(Q1.1st.df$name,Q1.2nd.df$name)]
#Q1.1st.df$degree.out.2nd<-Q1.2nd.df$degree.out[match(Q1.1st.df$name,Q1.2nd.df$name)]
#Q1.1st.df$betweenness.2nd<-Q1.2nd.df$betweenness[match(Q1.1st.df$name,Q1.2nd.df$name)]
#Q1.1st.df$component.2nd<-Q1.2nd.df$component[match(Q1.1st.df$name,Q1.2nd.df$name)]

#summary(Q1.1st.df)

#write.csv(Q1.1st.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q1.networkdata.csv", row.names=FALSE)

Q1.2nd.df %>%
  filter( betweenness == max(betweenness))%>%
  dplyr::select(name, betweenness)
#  32014R0508 0.005869453

Q1.2nd.df %>%
  filter( degree.in == max(degree.in))%>%
  select(name, degree.in)
# 32011R0182        34

Q1.2nd.df %>%
  filter( degree.out == max(degree.out))%>%
  select(name, degree.out)
# 32021R1139         66


# ------------------- eurovo terms -------------------

# EuroVoc Citation 1 --------------------------

# degree 
Q1C1.term.degree<-degree(Q1C1.terms.graph)
sum(Q1C1.term.degree) #5262 interactions
summary(Q1C1.term.degree)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00    6.00    9.00   12.41   14.25   68.00 
V(Q1C1.terms.graph)
#425 labels

# betweenness
Q1C1.term.betweenness<-betweenness(Q1C1.terms.graph, directed = FALSE, normalized=TRUE)

summary(Q1C1.term.betweenness)
#     Min.   1st Qu.   Median     Mean   3rd Qu.     Max. 
#  0.000000 0.000000 0.000000 0.004912 0.003789 0.135853
plot(Q1C1.term.degree ~ Q1C1.term.betweenness)

# term clusters/groups
# cluster_leading_eigen: 

#   "Community structure detecting based on the leading eigenvector of the community matrix" 
#   "This function tries to find densely connected subgraphs in a graph by calculating the leading nonnegative 
#    eigenvector of the modularity matrix of the graph." - CRAN PDF
E(Q1C1.terms.graph)$n
E(Q1C1.terms.graph)

Q1C1.termclusters <-cluster_leading_eigen(Q1C1.terms.graph,
                                        steps = -1,
                                        weights = E(Q1C1.terms.graph)$n)
table(membership(Q1C1.termclusters))
#  1  2  3  4  5  6  7  8  9 
# 83  3 72 60 35 23 58 52 38 

Q1C1.term.df<-data.frame(name= V(Q1C1.terms.graph)$name,
                       degree=Q1C1.term.degree,
                       betweenness=Q1C1.term.betweenness,
                       component=as.numeric(membership(Q1C1.termclusters))) 

write.csv(Q1C1.term.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q1C1.term.df.csv", row.names=FALSE)

Q1C1.term.df %>%
  filter( betweenness == max(betweenness))%>%
  select(name, betweenness)
#  EU programme   0.1358529

Q1C1.term.df %>%
  filter( degree == max(degree))%>%
  select(name, degree)
# environmental protection     68


plot_dendrogram(Q1C1.termclusters)

l2 <- layout.fruchterman.reingold(Q1C1.terms.graph)

plot(Q1C1.termclusters, Q1C1.terms.graph, 
     vertex.shape="circle",
     vertex.label = NA,
     edge.width=E(Q1C1.terms.graph)$n*1,
     rescale = TRUE,
     vertex.size=1,
     layout=l2
     )

plot(Q1C1.terms.graph,
     vertex.label.color=membership(Q1C1.termclusters),
     vertex.shape="none",
     edge.width=E(Q1C1.terms.graph)$n*1,
     rescale = TRUE,
     vertex.size=1,
     layout = l2)

# to make the plot slightly more legable lets remove those that have degree >= 13 (mean)
member.attributesQ1C1 <- as.data.frame(as.matrix(membership(Q1C1.termclusters))) %>%   rownames_to_column() %>% rename("membership" = "V1" )

Q1C1.terms.meta2 <-
    Q1C1.terms.meta %>%
    left_join(.,member.attributesQ1C1, by = c("labels"="rowname"))


Q1C1.network.updated <- graph_from_data_frame(d=Q1C1.terms, vertices = Q1C1.terms.meta2, directed = FALSE)


Q1C1.network.updated1 <-  delete_vertices(Q1C1.network.updated, V(Q1C1.network.updated)[degree(Q1C1.network.updated)<13])

n_distinct(V(Q1C1.network.updated1)$membership)
# 8 clusters

colors <- brewer.pal(n = 8, name = "Dark2")
colors3 <- c(colors)

V(Q1C1.network.updated1)$color <- colors3[as.numeric(as.factor(V(Q1C1.network.updated1)$membership))]

degrees <- 
    as.data.frame(cbind(degree(Q1C1.terms.graph))) %>%
    rownames_to_column() %>%
    rename("degree" = "V1")



png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/query1C1.eurovocterm.network.png",
    width = 3000, height = 3000)

set.seed(01)
plot(Q1C1.network.updated1,
     edge.width=E(Q1C1.network.updated1)$n,
     edge.color=adjustcolor("gray", alpha.f = .5),
     vertex.size=1.5,
     vertex.label.cex=(degree(Q1C1.network.updated1)/sum(degree(Q1C1.network.updated1))*200), # label size is equiv. to percent of edges associated to the word out of total edges
     vertex.label.color=V(Q1C1.network.updated1)$color, #membership(Q2.termclusters),
     vertex.shape="none",
     vertex.label.family = "sans",
     layout = layout.fruchterman.reingold,
     vertex.label.family = "sans")

dev.off()

# EuroVoc Citation 2 --------------------------


# degree 
Q1C2.term.degree<-degree(Q1C2.terms.graph)
sum(Q1C2.term.degree) #19112 interactions
summary(Q1C2.term.degree)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0     6.0     9.0    15.7    18.0   189.0  
V(Q1C2.terms.graph)
#1217 labels

# betweenness
Q1C2.term.betweenness<-betweenness(Q1C2.terms.graph, directed = FALSE, normalized=TRUE)

summary(Q1C2.term.betweenness)
#     Min.   1st Qu.   Median     Mean   3rd Qu.     Max. 
#  0.000e+00 0.000e+00 8.700e-07 1.639e-03 6.781e-04 9.104e-02 
plot(Q1C2.term.degree ~ Q1C2.term.betweenness)

# term clusters/groups
# cluster_leading_eigen: 

#   "Community structure detecting based on the leading eigenvector of the community matrix" 
#   "This function tries to find densely connected subgraphs in a graph by calculating the leading nonnegative 
#    eigenvector of the modularity matrix of the graph." - CRAN PDF
E(Q1C2.terms.graph)$n
E(Q1C2.terms.graph)


Q1C2.termclusters <-cluster_leading_eigen(Q1C2.terms.graph,
                                          steps = -1,
                                          weights = E(Q1C2.terms.graph)$n)
#Error in cluster_leading_eigen(Q1C2.terms.graph, steps = -1, weights = E(Q1C2.terms.graph)$n) : 
#At core/linalg/arpack.c:992 : ARPACK error, Maximum number of iterations reached

# maxiter is 1000 
arpack_defaults

arpack_defaults$maxiter = 10000
#lets change the options so it allows more iterations
arpack_defaults$maxiter = 10000
Q1C2.termclusters <-cluster_leading_eigen(Q1C2.terms.graph,
                                          steps = -1,
                                          weights = E(Q1C2.terms.graph)$n,
                                          options = arpack_defaults)

  
table(membership(Q1C2.termclusters))
#    1   2   3   4   5   6   7   8   9  10  11 
#  266   2 141 242  83 232 139  75  10   3  24 

Q1C2.term.df<-data.frame(name= V(Q1C2.terms.graph)$name,
                         degree=Q1C2.term.degree,
                         betweenness=Q1C2.term.betweenness,
                         component=as.numeric(membership(Q1C2.termclusters))) 

write.csv(Q1C2.term.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q1C2.term.data.csv", row.names=FALSE)

Q1C2.term.df %>%
  filter( betweenness == max(betweenness))%>%
  select(name, betweenness)
#  environmental protection  0.09104448

Q1C2.term.df %>%
  filter( degree == max(degree))%>%
  select(name, degree)
# environmental protection    189


plot_dendrogram(Q1C2.termclusters)

l2 <- layout.fruchterman.reingold(Q1C2.terms.graph)

plot(Q1C2.termclusters, Q1C2.terms.graph, 
     vertex.shape="circle",
     vertex.label = NA,
     edge.width=E(Q1C2.terms.graph)$n*1,
     rescale = TRUE,
     vertex.size=1,
     layout=l2
)

plot(Q1C2.terms.graph,
     vertex.label.color=membership(Q1C2.termclusters),
     vertex.shape="none",
     edge.width=E(Q1C2.terms.graph)$n*1,
     rescale = TRUE,
     vertex.size=1,
     layout = l2)

# to make the plot slightly more legable lets remove those that have degree >= 16 (the mean)
member.attributesQ1C2 <- as.data.frame(as.matrix(membership(Q1C2.termclusters))) %>%   rownames_to_column() %>% rename("membership" = "V1" )


Q1C2.terms.meta2 <-
  Q1C2.terms.meta %>%
  left_join(.,member.attributesQ1C2, by = c("labels"="rowname"))


Q1C2.network.updated <- graph_from_data_frame(d=Q1C2.terms, vertices = Q1C2.terms.meta2, directed = FALSE)


Q1C2.network.updated1 <-  delete_vertices(Q1C2.network.updated, V(Q1C2.network.updated)[degree(Q1C2.network.updated)<16])

V(Q1C2.network.updated)
V(Q1C2.network.updated1)

n_distinct(V(Q1C2.network.updated1)$membership)
# 8 clusters

colors <- brewer.pal(n = 8, name = "Dark2")
colors3 <- c(colors)

V(Q1C2.network.updated1)$color <- colors3[as.numeric(as.factor(V(Q1C2.network.updated1)$membership))]

degrees <- 
  as.data.frame(cbind(degree(Q1C2.terms.graph))) %>%
  rownames_to_column() %>%
  rename("degree" = "V1")

btwn<-
  Q1C2.term.df %>%
  filter(degree>=16) %>%
  select(name,betweenness) %>%
  deframe()


png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/query1C2.eurovocterm.network.png",
    width = 4000, height = 4000)

set.seed(01)
plot(Q1C2.network.updated1,
     edge.width=E(Q1C2.network.updated1)$n,
     edge.color=adjustcolor("gray", alpha.f = .5),
     vertex.size=1.5,
     vertex.label.cex= (degree(Q1C2.network.updated1)/sum(degree(Q1C2.network.updated1))*650), # label size is equiv. to percent of edges associated to the word out of total edges
     vertex.label.color=V(Q1C2.network.updated1)$color, #membership(Q2.termclusters),
     vertex.shape="none",
     vertex.label.family = "sans",
     layout = layout.fruchterman.reingold,
     vertex.label.family = "sans")

dev.off()

# EuroVoc Cluster Plots -----------------------
Clus.1.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 1") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#1B9E77")+
  ggtitle("Cluster 1: Environmental protection & EU programme (n = 241)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.1.PLOT.Q1

Clus.2.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 2") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "black")+
  ggtitle("Cluster 2 (n* = 1)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.2.PLOT.Q1

Clus.3.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 3") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#D95F02")+
  ggtitle("Cluster 3: Fisheries sustainable development (n = 280)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.3.PLOT.Q1


Clus.4.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 4") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#7570B3")+
  ggtitle("Cluster 4: Health/market standards and controls (n = 285)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))
#
#Clus.4.PLOT.Q1

Clus.5.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 5") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#E7298A")+
  ggtitle("Cluster 5: Economic and social development (n = 71)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.5.PLOT.Q1

Clus.6.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 6") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#66A61E")+
  ggtitle("Cluster 6: Information and data (n = 186)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.6.PLOT.Q1

Clus.7.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 7") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#E6AB02")+
  ggtitle("Cluster 7: Administrative services, \n support and transparency (n = 94)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.7.PLOT.Q1


Clus.8.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 8") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#A6761D")+
  ggtitle("Cluster 8: EU competativness and economics (n* = 67)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.8.PLOT.Q1

Clus.9.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 9") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "black")+
  ggtitle("Cluster 9: Limited and small companies/markets (n* = 5)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.9.PLOT.Q1


Clus.10.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 10") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "black")+
  ggtitle("Cluster 10 (n* = 3)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.10.PLOT.Q1

Clus.11.PLOT.Q1 <- 
  as.data.frame(cbind(V(Q1C2.network.updated1)$color, V(Q1C2.network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q1C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees, by = c("labels"= "rowname")) %>%
  mutate(V2 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V2= as.factor(V2)) %>%
  filter(V2 == "Cluster 11") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V2)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#666666")+
  ggtitle("Cluster 11: Mutual recognition principle and Admin. cooperation (n* = 22)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.11.PLOT.Q1


(Clus.1.PLOT.Q1 + Clus.2.PLOT.Q1 + Clus.3.PLOT.Q1) /
(Clus.4.PLOT.Q1 + Clus.5.PLOT.Q1 + Clus.6.PLOT.Q1) 


ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/Q1C2.termclusters.pt1.png",
       width = 35, height = 15)


(Clus.7.PLOT.Q1 + Clus.8.PLOT.Q1 + Clus.9.PLOT.Q1) /
(Clus.10.PLOT.Q1 + Clus.11.PLOT.Q1) 


ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/Q1C2.termclusters.pt2.png",
       width = 35, height = 15)

# Query 2 ---------------------------------------------------------

# ------------------- First order citations -------------------

# degree_in and degree_out
Q2.1st.degree.in<-degree(Q2.net1st.graph,mode="in")
Q2.1st.degree.out<-degree(Q2.net1st.graph,mode="out")

plot(Q2.1st.degree.in ~ Q2.1st.degree.out)

# betweenness
Q2.1st.betweenness<-betweenness(Q2.net1st.graph,directed = TRUE, normalized=TRUE) # normalized so we can compare to different size networks later

plot(Q2.1st.degree.in ~ Q2.1st.betweenness)
plot(Q2.1st.degree.out ~ Q2.1st.betweenness )

# module
# as we have multiple components in the network we first need to identify components
# first we look for weakly connected component, so that any edge between clusters of text is considered as connecting the clsuters

Q2.1st.comp.weak<-components(Q2.net1st.graph,mode="weak")

Q2.1st.df<-data.frame(name= V(Q2.net1st.graph)$name,
                      degree.in=Q2.1st.degree.in,
                      degree.out=Q2.1st.degree.out,
                      betweenness=Q2.1st.betweenness,
                      component=as.numeric(membership(Q2.1st.comp.weak))) 

rownames(Q2.1st.df) <- NULL

head(Q2.1st.df)

write.csv(Q2.1st.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q2C1.networkdata.csv", row.names=FALSE)

Q2.1st.df %>%
  filter( betweenness == max(betweenness))%>%
  select(name, betweenness)
# 32008L0056 0.005448802


Q2.1st.df %>%
  filter( degree.in == max(degree.in))%>%
  select(name, degree.in)
#  31992L0043        22


Q2.1st.df %>%
  filter( degree.out == max(degree.out))%>%
  select(name, degree.out)
# 32011R0142         39

# ------------------- Second order citations -------------------

# degree_in and degree_out
Q2.2nd.degree.in<-degree(Q2.net2nd.graph,mode="in")
Q2.2nd.degree.out<-degree(Q2.net2nd.graph,mode="out")

plot(Q2.2nd.degree.in ~ Q2.2nd.degree.out)

# betweenness
#graph1st.betweenness<-estimate_betweenness(graph1st,directed = TRUE,cutoff = -1)
Q2.2nd.betweenness<-betweenness(Q2.net2nd.graph,directed = TRUE,normalized=TRUE)

plot(Q2.2nd.degree.in ~ Q2.2nd.betweenness)
plot(Q2.2nd.degree.out ~ Q2.2nd.betweenness )

# module
## as we have multiple components in the network we first need to identify components
#first we look for weakly connected component, so that any edge between clusters of text is considered as connecting the clsuters

Q2.2nd.comp.weak<-components(Q2.net2nd.graph,mode="weak")

V(Q2.net2nd.graph)$component<-membership(Q2.2nd.comp.weak)

sort(table(Q2.2nd.comp.weak$membership))

Q2.2nd.giant<-subgraph(Q2.net2nd.graph,vids=V(Q2.net2nd.graph)[V(Q2.net2nd.graph)$component==1])

Q2.2nd.giant.cluster<-cluster_edge_betweenness(Q2.2nd.giant, directed = TRUE)
membership(Q2.2nd.giant.cluster)
table(membership(Q2.2nd.giant.cluster))

Q2.2nd.df<-data.frame(name= V(Q2.net2nd.graph)$name,
                      degree.in=Q2.2nd.degree.in,
                      degree.out=Q2.2nd.degree.out,
                      betweenness=Q2.2nd.betweenness,
                      component=as.numeric(membership(Q2.2nd.comp.weak)))

rownames(Q2.2nd.df) <- NULL

head(Q2.2nd.df)

write.csv(Q2.2nd.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q2C2.networkdata.csv", row.names=FALSE)

#Q2.1st.df$degree.in.2nd<-Q2.2nd.df$degree.in[match(Q2.1st.df$name,Q2.2nd.df$name)]
#Q2.1st.df$degree.out.2nd<-Q2.2nd.df$degree.out[match(Q2.1st.df$name,Q2.2nd.df$name)]
#Q2.1st.df$betweenness.2nd<-Q2.2nd.df$betweenness[match(Q2.1st.df$name,Q2.2nd.df$name)]
#Q2.1st.df$component.2nd<-Q2.2nd.df$component[match(Q2.1st.df$name,Q2.2nd.df$name)]
#summary(Q2.1st.df)

Q2.2nd.df %>%
  filter( betweenness == max(betweenness))%>%
  select(name, betweenness)
# 32000L0060 0.004303183


Q2.2nd.df %>%
  filter( degree.in == max(degree.in))%>%
  select(name, degree.in)
#  31999D0468        68


Q2.2nd.df %>%
  filter( degree.out == max(degree.out))%>%
  select(name, degree.out)
# 32021R1139         66


# ------------------- eurovo terms -------------------

# EuroVoc Citation 1 --------------------------

# degree 
Q2C1.term.degree<-degree(Q2C1.terms.graph)
sum(Q2C1.term.degree)
#9900
summary(Q2C1.term.degree)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.00    6.00    9.00   14.62   17.00  138.00 
V(Q2C1.terms.graph)
#677
# betweenness
Q2C1.term.betweenness<-betweenness(Q2C1.terms.graph, directed = FALSE, normalized=TRUE)

summary(Q2C1.term.betweenness)
# 0.000000 0.000000 0.000000 0.002813 0.001711 0.134770 

plot(Q2C1.term.degree ~ Q2C1.term.betweenness)

# term clusters/groups
Q2C1.termclusters <-cluster_leading_eigen(Q2C1.terms.graph,
                                        steps = -1,
                                        weights = E(Q2C1.terms.graph)$n)
table(membership(Q2C1.termclusters))
#  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18 
# 59   6   3  96  21 175   9  62  23   1   5   4 148  10   2   1  27  25 

Q2C1.term.df<-data.frame(name= V(Q2C1.terms.graph)$name,
                      degree=Q2C1.term.degree,
                      betweenness=Q2C1.term.betweenness,
                      component=as.numeric(membership(Q2C1.termclusters)))


write.csv(Q2C1.term.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q2C1.term.data.csv", row.names=FALSE)

Q2C1.term.df %>%
  filter( degree == max(degree))%>%
  select(name, degree)

# environmental protection    138

Q2C1.term.df %>%
  filter( betweenness == max(betweenness))%>%
  select(name, betweenness)
#  environmental protection    0.13477

plot_dendrogram(Q2C1.termclusters)

l <- layout.fruchterman.reingold(Q2C1.terms.graph)

plot(Q2C1.termclusters,Q2C1.terms.graph)
     
plot(Q2C1.terms.graph,
     vertex.label.color=membership(Q2C1.termclusters),
     vertex.shape="none",
     edge.width=E(Q2C1.terms.graph)$n*1,
     vertex.label.cex= 1 ,
     rescale = TRUE,
     vertex.size=1,
     layout = l)
# very messy....


dist <- seq(-.025,0.25, by=.0024)
dist <- rep(c(0.18, -0.18), length.out = 226)
#try to jitter the labels a little to avoid overlap 
V(Q2C1.terms.graph)$dist <- dist[as.numeric(as.factor(V(Q2C1.terms.graph)$name))]

membership(Q2C1.termclusters)

library(RColorBrewer)

colors <- brewer.pal(n = 9, name = "Set1")
colors2 <- brewer.pal(n = 9, name = "Paired")
colors3 <- c(colors,colors2)

V(Q2C1.terms.graph)$color <- colors3[as.numeric(as.factor(membership(Q2C1.termclusters)))]

l3 <- layout.fruchterman.reingold(Q2C1.terms.graph)

plot(Q2C1.terms.graph,
     edge.width=E(Q2C1.terms.graph)$n,
     edge.color=adjustcolor("gray", alpha.f = .5),
     vertex.size=2,
     vertex.label.cex= V(Q2C1.terms.graph)$total.count*.015 ,
     vertex.label.color=V(Q2C1.terms.graph)$color, 
     vertex.shape="none",
     layout = l3,
     vertex.label.family = "sans",
)



# to make the plot slightly more legable lets remove those that have edges >= 15 (the mean)
member.attributes <- 
    as.data.frame(as.matrix(membership(Q2C1.termclusters))) %>%   
    rownames_to_column() %>% 
    rename("membership" = "V1" )

Q2C1.terms.meta2 <-
    Q2C1.terms.meta %>%
    left_join(.,member.attributes, by = c("labels"="rowname")) %>%
    filter(!is.na(.$labels))


network.updated <- graph_from_data_frame(d=Q2C1.terms, vertices = Q2C1.terms.meta2, directed = FALSE)
class(network.updated)



network.updated1 <- delete_vertices(network.updated, V(network.updated)[degree(network.updated)<15])

n_distinct(V(network.updated1)$membership)
#10 in the filtered network

V(network.updated)
V(network.updated1)

library(RColorBrewer)

colors <- brewer.pal(n = 8, name = "Dark2")
colors2 <- brewer.pal(n = 3, name = "Set1")
colors2.5 <- colors2[-3] # remove the unreadable yellow
#colors2.5 <- colors2.5[-6] # remove the unreadable yellow

set.seed(022)
colors3 <- c(colors,colors2.5)
V(network.updated1)$color <- colors3[as.numeric(as.factor(V(network.updated1)$membership))]


degrees1 <- 
    as.data.frame(cbind(degree(Q2C1.terms.graph))) %>%
    rownames_to_column() %>%
    rename("degree" = "V1")

# network plot 

dist <- seq(-.025,0.25, by=.0024)
dist <- rep(c(0.18, -0.18), length.out = 226)
#try to jitter the labels a little to avoid overlap 
V(network.updated1)$dist <- dist[as.numeric(as.factor(V(network.updated1)$name))]

#dev.off()
n_distinct(V(network.updated1)$color)


png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/query2C1.eurovocterm.network.png",
    width = 2525, height = 2500)

setseed(01)
plot(network.updated1,
     edge.width=E(network.updated1)$n,
     edge.color=adjustcolor("gray", alpha.f = .5),
     vertex.size=2,
     vertex.label.cex=(degree(network.updated1)/sum(degree(network.updated1))*350), # label size is equiv. to percent of edges associated to the word out of total edges
     vertex.label.color=V(network.updated1)$color, 
     vertex.shape="none",
     vertex.label.family = "sans",
     layout = layout.fruchterman.reingold,
     vertex.label.family = "sans",
     vertex.label.dist = V(network.updated1)$dist
)

dev.off()


# EuroVoc Citation 2 --------------------------

# degree 
Q2C2.term.degree<-degree(Q2C2.terms.graph)
sum(Q2C2.term.degree)
#25670
summary(Q2C2.term.degree)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.00    6.00    9.00   17.33   19.00  230.00 
V(Q2C2.terms.graph)
#1481
# betweenness
Q2C2.term.betweenness<-betweenness(Q2C2.terms.graph, directed = FALSE, normalized=TRUE)

summary(Q2C2.term.betweenness)
# 0.000e+00 0.000e+00 2.886e-05 1.321e-03 5.340e-04 8.517e-02  

plot(Q2C2.term.degree ~ Q2C2.term.betweenness)

# term clusters/groups
Q2C2.termclusters <-cluster_leading_eigen(Q2C2.terms.graph,
                                          steps = -1,
                                          weights = E(Q2C2.terms.graph)$n)
table(membership(Q2C2.termclusters))
#    1   2   3   4   5   6   7   8   9  10  11  12  13  14 
#  216 274   2   1   2   3 283 265 176 178  45   6   3  27 

Q2C2.term.df<-data.frame(name= V(Q2C2.terms.graph)$name,
                         degree=Q2C2.term.degree,
                         betweenness=Q2C2.term.betweenness,
                         component=as.numeric(membership(Q2C2.termclusters)))


write.csv(Q2C2.term.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q2C2.term.data.csv", row.names=FALSE)


Q2C2.term.df %>%
  filter( degree == max(degree))%>%
  select(name, degree)

# environmental protection    230
# exchange of information    230

Q2C2.term.df %>%
  filter( betweenness == max(betweenness))%>%
  select(name, betweenness)
#  exchange of information  0.08516511

plot_dendrogram(Q2C2.termclusters)

l <- layout.fruchterman.reingold(Q2C2.terms.graph)

plot(Q2C2.termclusters,Q2C2.terms.graph)

plot(Q2C2.terms.graph,
     vertex.label.color=membership(Q2C2.termclusters),
     vertex.shape="none",
     edge.width=E(Q2C2.terms.graph)$n*1,
     vertex.label.cex= 1 ,
     rescale = TRUE,
     vertex.size=1,
     layout = l)
# very messy....


dist <- seq(-.025,0.25, by=.0024)
dist <- rep(c(0.18, -0.18), length.out = 226)
#try to jitter the labels a little to avoid overlap 
V(Q2C2.terms.graph)$dist <- dist[as.numeric(as.factor(V(Q2C2.terms.graph)$name))]

membership(Q2C2.termclusters)

library(RColorBrewer)


# to make the plot slightly more legable lets remove those that have edges >= 18 (the mean)
member.attributes <- 
  as.data.frame(as.matrix(membership(Q2C2.termclusters))) %>%   
  rownames_to_column() %>% 
  rename("membership" = "V1" )

Q2C2.terms.meta2 <-
  Q2C2.terms.meta %>%
  left_join(.,member.attributes, by = c("labels"="rowname")) %>%
  filter(!is.na(.$labels))


network.updated <- graph_from_data_frame(d=Q2C2.terms, vertices = Q2C2.terms.meta2, directed = FALSE)
class(network.updated)



network.updated1 <- delete_vertices(network.updated, V(network.updated)[degree(network.updated)<18])

n_distinct(V(network.updated1)$membership)
#10 in the filtered network

V(network.updated)
V(network.updated1)

library(RColorBrewer)

colors <- brewer.pal(n = 8, name = "Dark2")
colors2 <- brewer.pal(n = 3, name = "Set1")
colors2.5 <- colors2[-3] # remove the unreadable yellow
#colors2.5 <- colors2.5[-6] # remove the unreadable yellow

set.seed(022)
colors3 <- sample(c(colors,colors2.5))
V(network.updated1)$color <- colors3[as.numeric(as.factor(V(network.updated1)$membership))]


degrees1 <- 
  as.data.frame(cbind(degree(Q2C2.terms.graph))) %>%
  rownames_to_column() %>%
  rename("degree" = "V1")

# network plot 

dist <- seq(-.025,0.25, by=.0024)
dist <- rep(c(0.18, -0.18), length.out = 226)
#try to jitter the labels a little to avoid overlap 
V(network.updated1)$dist <- dist[as.numeric(as.factor(V(network.updated1)$name))]

#dev.off()
n_distinct(V(network.updated1)$color)

png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/query2C2.eurovocterm.network.png",
    width = 2525, height = 2500)

set.seed(01)
plot(network.updated1,
     edge.width=E(network.updated1)$n,
     edge.color=adjustcolor("gray", alpha.f = .5),
     vertex.size=2,
     vertex.label.cex=(degree(network.updated1)/sum(degree(network.updated1))*550), # label size is equiv. to percent of edges associated to the word out of total edges
     vertex.label.color=V(network.updated1)$color, #membership(Q2.termclusters),
     vertex.shape="none",
     vertex.label.family = "sans",
     layout = layout.fruchterman.reingold,
     vertex.label.family = "sans",
     vertex.label.dist = V(network.updated1)$dist
)

dev.off()


# EuroVoc Cluster Plots --------------


# So some of the word clouds are more legiable I will make a wordcloud for each and then patchwork them together

Clus.1.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1",  "Cluster 2", "Cluster 3",
                                     "Cluster 4",  "Cluster 5", "Cluster 6",
                                     "Cluster 7",  "Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 1") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#E6AB02")+
  ggtitle("Cluster 1: EU harmonization of Environmental protections \n and information exchange (n = 522)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.1.PLOT


Clus.2.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 2") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#E41A1C")+
  ggtitle("Cluster 2: EU and member state \n sustainable development programmes (n = 505)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.2.PLOT



Clus.3.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 3") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "Black")+
  ggtitle("Cluster 3: water resources (n = 1)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.3.PLOT

Clus.4.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 4") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#666666")+
  ggtitle("Cluster 4: Power of the institutions (n* = 11)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.4.PLOT


Clus.5.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 5") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#D95F02")+
  ggtitle("Cluster 5: EU Budget (n* = 13)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.5.PLOT


Clus.6.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 6") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#377EB8")+
  ggtitle("Cluster 6: Agriculture (n* = 11)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.6.PLOT


Clus.7.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 7") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#E7298A")+
  ggtitle("Cluster 7: Health/market standards and controls (n = 443)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.7.PLOT



Clus.8.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 8") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#66A61E")+
  ggtitle("Cluster 8: EU cooperation and data (n = 220)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.8.PLOT

Clus.9.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 9") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#7570B3")+
  ggtitle("Cluster 9: Eu competitiveness and financing (n = 152)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.9.PLOT



Clus.10.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 10") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#1B9E77")+
  ggtitle("Cluster 10: Single markets (n = 164)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.10.PLOT



Clus.11.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 11") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#A6761D")+
  ggtitle("Cluster 11: Technical standards and regulations (n* = 52)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.11.PLOT

Clus.12.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 12") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "Black")+
  ggtitle("Cluster 12: Alcohol (n* = 3)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.12.PLOT

Clus.13.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 13") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "Black")+
  ggtitle("Cluster 13: Chemistry (n* = 3)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.13.PLOT


Clus.14.PLOT <- 
  as.data.frame(cbind(V(network.updated1)$color, V(network.updated1)$membership)) %>%
  mutate(V2 = as.numeric(V2)) %>%
  distinct(., .keep_all=TRUE) %>%
  right_join(.,Q2C2.terms.meta2, by = c("V2" = "membership")) %>%
  left_join(.,degrees1, by = c("labels"= "rowname")) %>%
  mutate(V22 =paste("Cluster", V2, sep = " ")) %>%
  mutate(V22= factor(V22, levels = c("Cluster 1", "Cluster 2", "Cluster 3",
                                     "Cluster 4", "Cluster 5", "Cluster 6",
                                     "Cluster 7","Cluster 8","Cluster 9",
                                     "Cluster 10", "Cluster 11", "Cluster 12",
                                     "Cluster 13", "Cluster 14")))  %>%
  filter(V22 == "Cluster 14") %>%
  ggplot(., aes( label = labels, 
                 size = degree,
                 color = V22)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "Black")+
  ggtitle("Cluster 14 (n = 14)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

#Clus.14.PLOT

big.clsuplot1 <-
(Clus.1.PLOT + Clus.2.PLOT + Clus.3.PLOT)/
(Clus.4.PLOT + Clus.5.PLOT + Clus.6.PLOT)

ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/Q2C2.termclusters.pt1.png",
       width = 37, height = 20,
       limitsize = FALSE)

big.clsuplot2 <-
  (Clus.7.PLOT + Clus.8.PLOT + Clus.9.PLOT )/
  (Clus.10.PLOT + Clus.11.PLOT + Clus.12.PLOT)/
  (Clus.13.PLOT + Clus.14.PLOT)

ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/Results/Q2C2.termclusters.pt2.png",
       width = 40, height = 20,
       limitsize = FALSE)


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


