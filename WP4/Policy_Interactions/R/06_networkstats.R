
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("dplyr")
library("igraph")
library("tibble")

# Define functions --------------------------------------------------------

# No defined function for this script

# Load data ---------------------------------------------------------------

# ------------ Query 1 (Q1) --------- #

# -- citations -- #
Q1.net1st<-read.csv("WP4/Policy_Interactions/data/03.Q1firstordercit.edgelist.csv")               # edge list
Q1.net1st.meta<-read.csv("WP4/Policy_Interactions/data/03.Q1firstordercit.verticesmetadata.csv")  # vertices meta data
Q1.net1st.graph<-readRDS("WP4/Policy_Interactions/data/03.Q1firstordercit.network.rds")           # the network object 

Q1.net2nd<-read.csv("WP4/Policy_Interactions/data/03.Q1secondordercit.edgelist.csv")              # edge list
Q1.net2nd.meta<-read.csv("WP4/Policy_Interactions/data/03.Q1secondordercit.verticesmetadata.csv") # vertices meta data
Q1.net2nd.graph<-readRDS("WP4/Policy_Interactions/data/03.Q1secondordercit.network.rds")          # the network object 

# -- terms co-occurance -- #

Q1.terms<-read.csv("WP4/Policy_Interactions/data/03.Q1term.edgelist.csv")              # edge list
Q1.terms.meta<-read.csv("WP4/Policy_Interactions/data/03.Q1term.verticesmetadata.csv") # vertices meta data
Q1.terms.graph<-readRDS("WP4/Policy_Interactions/data/03.Q1.termnetwork.rds")          # the network object 

# ----------- Query 2 (Q2) ---------- #

# -- citations -- #
Q2.net1st<-read.csv("WP4/Policy_Interactions/data/03.Q2firstordercit.edgelist.csv")              # edge list
Q2.net1st.meta<-read.csv("WP4/Policy_Interactions/data/03.Q2firstordercit.verticesmetadata.csv") # vertices meta data
Q2.net1st.graph<-readRDS("WP4/Policy_Interactions/data/03.Q2firstordercit.network.rds")          # the network object 

Q2.net2nd<-read.csv("WP4/Policy_Interactions/data/03.Q2secondordercit.edgelist.csv")              # edge list
Q2.net2nd.meta<-read.csv("WP4/Policy_Interactions/data/03.Q2secondordercit.verticesmetadata.csv") # vertices meta data
Q2.net2nd.graph<-readRDS("WP4/Policy_Interactions/data/03.Q2secondordercit.network.rds")          # the network object 

# -- terms co-occurance -- #

Q2.terms<-read.csv("WP4/Policy_Interactions/data/03.Q2term.edgelist.csv")              # edge list
Q2.terms.meta<-read.csv("WP4/Policy_Interactions/data/03.Q2term.verticesmetadata.csv") # vertices meta data
Q2.terms.graph<-readRDS("WP4/Policy_Interactions/data/03.Q2.termnetwork.rds")          # the network object 

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
# larger value means a greater bottleneck for the control of information passing between nodes.
Q1.1st.betweenness<-betweenness(Q1.net1st.graph,directed = TRUE,normalized=TRUE) # normalized so we can compare to different size networks later

plot(Q1.1st.degree.in ~ Q1.1st.betweenness)
plot(Q1.1st.degree.out ~ Q1.1st.betweenness )

# module
# as we have multiple components in the network we first need to identify components
# first we look for weakly connected component, so that any edge between clusters of text is considered as connecting the clsuters

Q1.1st.comp.weak<-components(Q1.net1st.graph,mode="weak")

Q1.1st.df<-data.frame(name= V(Q1.net1st.graph)$name,
                        degree.in=Q1.1st.degree.in,
                        degree.out=Q1.1st.degree.out,
                        betweenness=Q1.1st.betweenness,
                        component=as.numeric(membership(Q1.1st.comp.weak))) 

rownames(Q1.1st.df) <- NULL

head(Q1.1st.df)
#cluster_edge_betweenness because we deal with a directed network
# graph1st.cluster<-cluster_edge_betweenness(
# SUBSET(graph1st),
# directed = TRUE)

# membership(graph1st.cluster)

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

head(Q1.2nd.df)

plot(degree.in~degree.out,data=Q1.2nd.df)

Q1.1st.df$degree.in.2nd<-Q1.2nd.df$degree.in[match(Q1.1st.df$name,Q1.2nd.df$name)]
Q1.1st.df$degree.out.2nd<-Q1.2nd.df$degree.out[match(Q1.1st.df$name,Q1.2nd.df$name)]
Q1.1st.df$betweenness.2nd<-Q1.2nd.df$betweenness[match(Q1.1st.df$name,Q1.2nd.df$name)]

summary(Q1.1st.df)

# ------------------- eurovo terms -------------------

# degree 
Q1.term.degree<-degree(Q1.terms.graph)

V(Q1.terms.graph)

# betweenness
Q1.term.betweenness<-betweenness(Q1.terms.graph, directed = FALSE, normalized=TRUE)

plot(Q1.term.degree ~ Q1.term.betweenness)

# term clusters/groups
# cluster_leading_eigen: 

#   "Community structure detecting based on the leading eigenvector of the community matrix" 
#   "This function tries to find densely connected subgraphs in a graph by calculating the leading nonnegative 
#    eigenvector of the modularity matrix of the graph." - CRAN PDF
E(Q1.terms.graph)$n
E(Q1.terms.graph)

Q1.termclusters <-cluster_leading_eigen(Q1.terms.graph,
                                        steps = -1,
                                        weights = E(Q1.terms.graph)$n)

# 7 clusters

plot_dendrogram(Q1.termclusters)

l2 <- layout.fruchterman.reingold(Q1.terms.graph)

plot(Q1.termclusters, Q1.terms.graph, 
    #vertex.label.color="black",
     vertex.shape="circle",
     vertex.label = NA,
     # vertex.label.cex=V(Q1.terms.graph)$total.count*.05,
     edge.width=E(Q1.terms.graph)$n*1,
     rescale = TRUE,
     ylim=c(-.8,.85),xlim=c(-.9,.9),
     vertex.size=1,
     layout=l2
     )

plot(Q1.terms.graph,
     vertex.label.color=membership(Q1.termclusters),
     vertex.shape="none",
    # vertex.label.cex=V(Q1.terms.graph)$total.count*.05,
     edge.width=E(Q1.terms.graph)$n*1,
     rescale = TRUE,
     ylim=c(-.8,.85),xlim=c(-.9,.9),
     vertex.size=1,
     layout = l2)

Q1.term.df<-data.frame(name= V(Q1.terms.graph)$name,
                       degree=Q1.term.degree,
                       betweenness=Q1.term.betweenness,
                       component=as.numeric(membership(Q1.termclusters)))

rownames(Q1.term.df) <- NULL

head(Q1.term.df)

Q2.term.df<-data.frame(name= V(Q2.terms.graph)$name,
                       degree=Q2.term.degree,
                       betweenness=Q2.term.betweenness,
                       component=as.numeric(membership(Q2.termclusters)))

rownames(Q2.term.df) <- NULL

head(Q2.term.df)

# to make the plot slightly more legable lets remove those that have edges >= 9 (the median)
member.attributesQ1 <- as.data.frame(as.matrix(membership(Q1.termclusters))) %>%   rownames_to_column() %>% rename("membership" = "V1" )

Q1.terms.meta2 <-
    Q1.terms.meta %>%
    left_join(.,member.attributesQ1, by = c("item1"="rowname"))


Q1.network.updated <- graph_from_data_frame(d=Q1.terms, vertices = Q1.terms.meta2, directed = FALSE)


Q1.network.updated1 <- delete_vertices(Q1.network.updated, V(Q1.network.updated)[degree(Q1.network.updated)<9])

n_distinct(V(Q1.network.updated1)$membership)

library(RColorBrewer)

colors <- brewer.pal(n = 7, name = "Dark2")
colors3 <- c(colors, colors2)

V(Q1.network.updated1)$color <- colors3[as.numeric(as.factor(V(Q1.network.updated1)$membership))]

l3 <- layout.fruchterman.reingold(Q1.network.updated1)

dist <- seq(-.025,0.25, by=.0024)
dist <- rep(c(0.18, -0.18), length.out = 54)
#try to jitter the labels a little to avoid overlap 
V(Q1.network.updated1)$dist <- dist[as.numeric(as.factor(V(Q1.network.updated1)$name))]


png(file = "WP4/Policy_Interactions/Results/query1.eurovocterm.network.png",
    width = 1700, height = 1700)


plot(Q1.network.updated1,
     edge.width=E(Q1.network.updated1)$n,
     edge.color=adjustcolor("gray", alpha.f = .25),
     vertex.size=2,
     vertex.label.cex=(degree(Q1.network.updated1)/sum(degree(Q1.network.updated1))*200), # label size is equiv. to percent of edges associated to the word out of total edges
     vertex.label.color=V(Q1.network.updated1)$color, #membership(Q2.termclusters),
     vertex.shape="none",
     layout = l3,
     vertex.label.family = "sans",
     ylim=c(-1.25,1.25),xlim=c(-1.25,1.25),
     layout = l2,
     vertex.label.family = "sans",
     vertex.label.dist = V(Q1.network.updated1)$dist
)

dev.off()

# Query 2 ---------------------------------------------------------

# ------------------- First order citations -------------------

# degree_in and degree_out
Q2.1st.degree.in<-degree(Q2.net1st.graph,mode="in")
Q2.1st.degree.out<-degree(Q2.net1st.graph,mode="out")

plot(Q2.1st.degree.in ~ Q2.1st.degree.out)

# betweenness
Q2.1st.betweenness<-betweenness(Q2.net1st.graph,directed = TRUE,normalized=TRUE) # normalized so we can compare to different size networks later

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

Q2.1st.df$degree.in.2nd<-Q2.2nd.df$degree.in[match(Q2.1st.df$name,Q2.2nd.df$name)]
Q2.1st.df$degree.out.2nd<-Q2.2nd.df$degree.out[match(Q2.1st.df$name,Q2.2nd.df$name)]
Q2.1st.df$betweenness.2nd<-Q2.2nd.df$betweenness[match(Q2.1st.df$name,Q2.2nd.df$name)]

summary(Q2.1st.df)
# ------------------- eurovo terms -------------------

# degree 
Q2.term.degree<-degree(Q2.terms.graph)

V(Q2.terms.graph)

# betweenness
Q2.term.betweenness<-betweenness(Q2.terms.graph, directed = FALSE, normalized=TRUE)

plot(Q2.term.degree ~ Q2.term.betweenness)

# term clusters/groups
Q2.termclusters <-cluster_leading_eigen(Q2.terms.graph,
                                        steps = -1,
                                        weights = E(Q2.terms.graph)$n,
                                        options = list(maxiter=10000))
# 21 clusters


plot_dendrogram(Q2.termclusters)

l <- layout.fruchterman.reingold(Q2.terms.graph)

plot(Q2.termclusters,Q2.terms.graph)
     
plot(Q2.terms.graph,
     vertex.label.color=membership(Q2.termclusters),
     vertex.shape="none",
     # vertex.label.cex=V(Q1.terms.graph)$total.count*.05,
     edge.width=E(Q2.terms.graph)$n*1,
     vertex.label.cex= 1 ,
     rescale = TRUE,
     #ylim=c(-.8,.85),xlim=c(-.9,.9),
     vertex.size=1,
     layout = l,
     ylim=c(-.85,.85),
     xlim=c(-.8,.8))
# very messy....


dist <- seq(-.025,0.25, by=.0024)
dist <- rep(c(0.18, -0.18), length.out = 226)
#try to jitter the labels a little to avoid overlap 
V(Q2.terms.graph)$dist <- dist[as.numeric(as.factor(V(Q2.terms.graph)$name))]

membership(Q2.termclusters)

library(RColorBrewer)

colors <- brewer.pal(n = 9, name = "Set1")
colors2 <- brewer.pal(n = 12, name = "Paired")
colors3 <- c(colors, colors2)

V(Q2.terms.graph)$color <- colors3[as.numeric(as.factor(membership(Q2.termclusters)))]

l3 <- layout.fruchterman.reingold(Q2.terms.graph)

plot(Q2.terms.graph,
     edge.width=E(Q2.terms.graph)$n,
     edge.color=adjustcolor("gray", alpha.f = .5),
     vertex.size=2,
     vertex.label.cex= V(Q2.terms.graph)$total.count*.015 ,
     vertex.label.color=V(Q2.terms.graph)$color, #membership(Q2.termclusters),
     vertex.shape="none",
     layout = l3,
     vertex.label.family = "sans",
    # rescale = TRUE,
     ylim=c(-.85,.85),
     xlim=c(-.87,.87)
     #vertex.label.dist = V(Q2.terms.graph)$dist
     # trying this layout based on pdf above...
)


Q2.term.df<-data.frame(name= V(Q2.terms.graph)$name,
                       degree=Q2.term.degree,
                       betweenness=Q2.term.betweenness,
                       component=as.numeric(membership(Q2.termclusters)))

rownames(Q2.term.df) <- NULL

head(Q2.term.df)

# to make the plot slightly more legable lets remove those that have edges >= 9 (the median)
member.attributes <- as.data.frame(as.matrix(membership(Q2.termclusters))) %>%   rownames_to_column() %>% rename("membership" = "V1" )

Q2.terms.meta2 <-
    Q2.terms.meta %>%
    left_join(.,member.attributes, by = c("item1"="rowname"))
    
    
network.updated <- graph_from_data_frame(d=Q2.terms, vertices = Q2.terms.meta2, directed = FALSE)
class(network3)


network.updated1 <- delete_vertices(network.updated, V(network.updated)[degree(network.updated)<9])

n_distinct(V(network.updated1)$membership)

library(RColorBrewer)

colors <- brewer.pal(n = 8, name = "Dark2")
colors2 <- brewer.pal(n = 8, name = "Set1")
colors3 <- c(colors, colors2)

V(network.updated1)$color <- colors3[as.numeric(as.factor(V(network.updated1)$membership))]

l3 <- layout.fruchterman.reingold(network.updated1)

dist <- seq(-.025,0.25, by=.0024)
dist <- rep(c(0.18, -0.18), length.out = 226)
#try to jitter the labels a little to avoid overlap 
V(network.updated1)$dist <- dist[as.numeric(as.factor(V(network.updated1)$name))]


png(file = "WP4/Policy_Interactions/Results/query2.eurovocterm.network.png",
    width = 1500, height = 1500)


plot(network.updated1,
     edge.width=E(network.updated1)$n,
     edge.color=adjustcolor("gray", alpha.f = .25),
     vertex.size=2,
     vertex.label.cex=(degree(network.updated1)/sum(degree(network.updated1))*200), # label size is equiv. to percent of edges associated to the word out of total edges
     vertex.label.color=V(network.updated1)$color, #membership(Q2.termclusters),
     vertex.shape="none",
     layout = l3,
     vertex.label.family = "sans",
     ylim=c(-.9,.9),xlim=c(-.9,.9),
     layout = l2,
     vertex.label.family = "sans",
     vertex.label.dist = V(network.updated1)$dist
)

dev.off()

