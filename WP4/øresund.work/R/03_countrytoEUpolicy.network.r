
# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("dplyr")
library("stringr")
library("igraph")
library("patchwork")
library("viridis")           
require("graphics")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------

DKEUlinks <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.DKEUlinks.csv")
SEEUlinks <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02SE.EU.links.csv")


################################################################################
###########################        DENMARK         #############################
################################################################################

# DK to EU document network ----------------------------------------------------

DKEUlinks1 <-
  DKEUlinks %>%
  distinct(url,EU.link.CELEX) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  mutate(url = str_replace_all(url, "https://www.retsinformation.dk","")) %>% # just to make the id url shorter
  rename("from" = "url",
         "to" = "EU.link.CELEX") %>%
  filter(!is.na(to))    # remove documents that dont link to an EU CELEX
  

n_distinct(DKEUlinks1$from)
# 206 dk documents link to an EU doc
n_distinct(DKEUlinks1$to)
# 234 EU legal acts link 


eu.vertices <- 
  DKEUlinks1%>%
 # select(-url,-search.term,-harpun,-kommercielt,-erhvervsmæssigt,-erhvervs,-rekreativt,-sæl,-fugle) %>%
  distinct(to) %>%
  rename("vertices" = "to") %>%
  mutate(source = "EU")

dk.vertices <- 
  DKEUlinks1%>%
  #select(url,search.term,harpun,kommercielt,erhvervsmæssigt,erhvervs,rekreativt,sæl,fugle) %>%
  distinct(from) %>%
 # mutate(url = str_replace_all(url, "https://www.retsinformation.dk","")) %>%
  rename("vertices" = "from")%>%
  mutate(source = "DK")

vertices <- rbind(dk.vertices,eu.vertices)

network <- graph_from_data_frame(d=DKEUlinks1, directed = FALSE, vertices = vertices)
print(network, e=TRUE, v=TRUE)

library(RColorBrewer)
col  <- brewer.pal(3, "Set2") 
col <- col[-1]
col <- c("#d1050c", "#FFCC00")
V(network)$color <- col[as.numeric(as.factor(V(network)$source))]
# DK doc are the red ones...
degree <- degree(network)

median(degree)
#2
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#1.00  1.00  2.00  3.00  7.05 82.00 

l <- layout.fruchterman.reingold(network)
sort(degree)

#png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/DK.eu.network.png",
 #   width = 1500, height = 1675)

plot(network,
     vertex.label=ifelse(degree(network) >=7.05 & V(network)$source == "EU",
                         V(network)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 3.5,
     vertex.label.color = "black",
   #  vertex.size=degree,
     #vertex.shape = ifelse(V(network)$source == "EU",
                    #       "square","circle"),
     vertex.label.family = "sans",
     layout = l)
#labels are those EU docs that are >= the 95% percentile for the degree number

legend(x=-1.15,y=-.85, legend = c("Danish Legislation","EU Legislation"), pch=21,
       
       col=col, pt.bg=col, pt.cex=2, cex=2, bty="n", ncol=1)

ifelse(degree(network) >=7.05 & V(network)$source == "EU",
       V(network)$name,NA) %>% na.omit()

legend(x=-1.2,y=-1.025, c("31992L0043: Protecting Europe’s biodiversity (Natura 2000)",
                      "32009L0147: Conservation of wild birds",
                      "32004L0035: The polluter-pays principle and environmental liability",
                      "32006L0123: The EU’s services directive",
                      "32011L0092: Assessment of the effects of projects on the environment (EIA)",
                      "31979L0409: Council Directive 79/409/EEC of 2 April 1979 on the conservation of wild birds",
                      "32000L0060: Good-quality water in Europe (EU water directive)",
                      " ",
                      "32013R1303: Provisions on the European Regional Development Fund, European Social Fund, Cohesion Fund,",
                      "European Agricultural Fund for Rural Development and European Maritime and Fisheries Fund...",
                      "31991L0676: Fighting water pollution from agricultural nitrates",
                      "32014R0508: European Maritime and Fisheries Fund (2014-2020)",
                      "32021R1060: Common rules on EU funds (2021–2027)",
                      "32021R1139: European Maritime, Fisheries and Aquaculture Fund (2021–2027)"),
                       cex=1.25,
                       ncol=2,
                       col="#777777",
                       bty="n", # no box around the legend
                      )

dev.off()

# network stats ----------------------------------

# degree 
network.degree<-degree(network)

summary(network.degree)


# betweenness
network.betweenness<-betweenness(network, directed = FALSE, normalized=TRUE)

summary(network.betweenness)

plot(network.degree ~ network.betweenness)

# term clusters/groups
# cluster_leading_eigen: 

#   "Community structure detecting based on the leading eigenvector of the community matrix" 
#   "This function tries to find densely connected subgraphs in a graph by calculating the leading nonnegative 
#    eigenvector of the modularity matrix of the graph." - CRAN PDF
E(network)

network.clusters <-cluster_leading_eigen(network,
                                         weights = NULL)

sort(table(network.clusters$membership))
# cluster 33 is the largest!

quantile(table(network.clusters$membership),probs = c(0,.25,.5,.75,.95,1))

plot_dendrogram(network.clusters)

# make a df of the network plots ---
DK.network.stats <- data.frame(name= V(network)$name,
                      degree=network.degree,
                      betweenness=network.betweenness,
                      cluster=as.numeric(membership(network.clusters)))

DK.network.stats.1 <- 
  DKEUlinks %>%
  select(EU.link.CELEX,labels) %>%
  distinct() %>%
  right_join(., DK.network.stats, by= c("EU.link.CELEX" = "name"))

cluster.labels <- 
  DK.network.stats.1 %>%
  group_by(cluster, labels) %>%
  mutate(total.labelcluster.degree = sum(degree)/2) %>%
  distinct(cluster,labels,total.labelcluster.degree) %>%
  na.omit()


l2 <- layout.fruchterman.reingold(network)

plot(network.clusters, network, 
     #vertex.label.color="black",
     vertex.shape="circle",
     vertex.label = NA,
     # vertex.label.cex=V(Q1.terms.graph)$total.count*.05,
    # edge.width=E(network.clusters)$n*1,
     rescale = TRUE,
     ylim=c(-.8,.85),xlim=c(-.9,.9),
     vertex.size=3,
     layout=l2
)


# solution to the color issue here:
# https://statisticsglobe.com/create-distinct-color-palette-in-r
palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  # Extract color info
palette3_all <- unlist(mapply(brewer.pal,                     # Create vector with all colors
                              palette3_info$maxcolors,
                              rownames(palette3_info)))
palette3_all   

set.seed(1)                                             # Set random seed
palette3 <- sample(palette3_all, 42)                    # Sample colors
palette3 

V(network)$color <- palette3[as.numeric(as.factor(membership(network.clusters)))]

plot(network,
     vertex.color=V(network)$color,
     vertex.label=ifelse(degree(network) >7.05 & V(network)$source == "EU",
                         V(network)$name,NA),
     vertex.label.cex = .75,
     vertex.shape = ifelse(V(network)$source == "EU",
                           "circle","square"),
     vertex.size= 2,
     layout = l2)

legend(
  "bottomleft",
  legend=levels(as.factor(membership(network.clusters))) ,
  col = palette3,
  pch    = 20,
  cex    = 1,
  bty    = "n",
  title  = "",
  horiz = FALSE
)

plot(network,
     vertex.label=ifelse(degree(network) >7.05 & V(network)$source == "EU",
                         V(network)$name,NA),
     vertex.label.cex = .75,
     vertex.size= 2,
     layout = l)

# Word clouds of cluster eurovoc discriptors: 

cluster.labels %>%
  filter(cluster==33 | cluster==35 |
         cluster==39 | cluster==34 |
         cluster==41 | cluster==1  ) %>%
  mutate(cluster=as.factor(cluster)) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, color = cluster)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~cluster, nrow = 3)
      
cluster.labels %>%
  filter(cluster==42 | cluster==9 |
         cluster==16 | cluster==24 |
         cluster==37 ) %>%
  mutate(cluster=as.factor(cluster)) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, color = cluster)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~cluster, nrow = 3)

cluster.labs <- c(#"33" = "Cluster 33: Environmental Protection/Biodiversity", 
                  "35"="Cluster 35: Common Fisheries Policy", 
                  "39"="Cluster 39: Work safety and health",
                  "34" = "Cluster 34: Occupational services/markets", 
                  "41"="Cluster 41: Water resources (emph. pollution)", 
                  "1"="Cluster 1: EU development funds", 
                  "42"= "Cluster 42: Sector environmental impact", 
                  "9"= "Cluster 9: Emissions/GHG",
                  "16" = "Cluster 16: Food import risks and regulations", 
                  "24"= "Cluster 24: Taxes", 
                  "37"= "Cluster 37: Food inspection (safety/quality)")

x <-as.data.frame(cbind(
  V(network)$color,
  as.numeric(as.factor(membership(network.clusters))) 
  )
  ) %>% distinct()


p1 <- 
  cluster.labels %>%
  filter(  cluster==35) %>%
  mutate(cluster=as.factor(cluster)) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, color = cluster)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values=c("#80B1D3"))+
  # facet_grid(.~cluster,  scales = "free", space = "free",
  #          labeller = labeller(cluster = cluster.labs)) +
  #facet_wrap(~cluster, nrow = 3,  scales = "free", shrink = FALSE,
    #         labeller = labeller(cluster = cluster.labs)) +
  labs(title = "Cluster 33: Environmental Protection/Biodiversity") +
  theme(plot.title = element_text(face="bold", size = 12, hjust=0.5)) 
p1
  
p2 <- 
  cluster.labels %>%
  filter(  cluster==35 |
           cluster==39 | cluster==34 |
           cluster==41 | cluster==1  |
           cluster==42 | cluster==9  |
           cluster==16 | cluster==24 |
           cluster==37 ) %>%
  mutate(cluster=as.factor(cluster)) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, color = cluster)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(breaks = c( "35", "39", "34",
                                "41", "1", "42", "9",
                                "16", "24", "37"),
                                values=c(#"#80B1D3", 
                                         "#66C2A5", 
                                         "#984EA3", 
                                         "#B3E2CD", 
                                         "#FDDAEC", 
                                         "#FDB462", 
                                         "#A65628",
                                         "#FB9A99",
                                         "#FB8072",
                                         "#c9dba4", # --> changed to be slightly darker bc the other color was very hard to read the text
                                         "#CBD5E8"))+
 # facet_grid(.~cluster,  scales = "free", space = "free",
   #          labeller = labeller(cluster = cluster.labs)) +
  facet_wrap(~cluster, nrow = 2,  scales = "free", shrink = FALSE,
             labeller = labeller(cluster = cluster.labs)) +
  theme( strip.text.x = element_text(face="bold", size = 12)) 
p2

p2/p1

ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/03.DKclusterwordclouds.png",
       width = 75,
       height = 50,
       units = c( "cm"),
       limitsize = FALSE)

# these are the word clouds for the largest (>75%) clusters in term of number of verticies (documents acssociated)
sort(table(network.clusters$membership))
# cluster 33 is the largest!

quantile(table(network.clusters$membership),probs = c(0,.25,.5,.75,.95,1))

# inspecting them individually: 
cluster.labels %>%
  filter(cluster==33) %>%
ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

cluster.labels %>%
  filter(cluster==35) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

cluster.labels %>%
  filter(cluster==39) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

cluster.labels %>%
  filter(cluster==34) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

cluster.labels %>%
  filter(cluster==41) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

cluster.labels %>%
  filter(cluster==1) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

cluster.labels %>%
  filter(cluster==37) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

cluster.labels %>%
  filter(cluster==24) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

cluster.labels %>%
  filter(cluster==16) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

cluster.labels %>%
  filter(cluster==9) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

cluster.labels %>%
  filter(cluster==42) %>%
  ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()


################################################################################
###########################        SWEDEN          #############################
################################################################################

# SE to EU document network ----------------------------------------------------

SEEUlinks1 <-
  SEEUlinks %>%
  distinct(doc.id,celex) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  rename("from" = "doc.id",
         "to" = "celex")  #  Remember documents that dont link to an EU CELEX are not in this dataframe


n_distinct(SEEUlinks1$from)
# 123 dk documents link to an EU doc
n_distinct(SEEUlinks1$to)
# 528 EU legal acts link 


eu.vertices2 <- 
  SEEUlinks1%>%
  distinct(to) %>%
  rename("vertices" = "to") %>%
  mutate(source = "EU")

SE.vertices <- 
  SEEUlinks1%>%
  distinct(from) %>%
  rename("vertices" = "from")%>%
  mutate(source = "SE")

vertices <- rbind(SE.vertices,eu.vertices2)

network <- graph_from_data_frame(d=SEEUlinks1, directed = FALSE, vertices = vertices)
print(network, e=TRUE, v=TRUE)


library(RColorBrewer)
col  <- brewer.pal(3, "Set2") 
col <- col[-1]
V(network)$color <- col[as.numeric(as.factor(V(network)$source))]
# DK doc are the red ones...
degree <- degree(network)

median(degree)
#1
sort(degree)


quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#   1    1     1    3     11    63 


l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(degree(network) >=11 & V(network)$source == "EU",
                         V(network)$name,NA),
     vertex.label.cex = .75,
     vertex.size= 3,
     layout = l)
#labels are those EU docs that are >= the 95% percentile for the degree number

# 31995L0046     32016R0679 31999L0045    


## SWEDEN AND DK WITH EU network

EUlinks <- rbind(SEEUlinks1,DKEUlinks1)

vertices <- rbind(SE.vertices,eu.vertices2,dk.vertices,eu.vertices) %>% distinct()


network <- graph_from_data_frame(d=EUlinks, directed = FALSE, vertices = vertices)

print(network, e=TRUE, v=TRUE)


library(RColorBrewer)

col <- c("#004B87", "#FFCC00", "#d1050c")
V(network)$color <- col[as.numeric(as.factor(V(network)$source))]
# DK doc are the red ones...
degree <- degree(network)

median(degree)
#2
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#     1    1    2    3   11   85 

l <- layout.fruchterman.reingold(network)
sort(degree)

plot(network,
     vertex.label=ifelse(degree(network) >=85 & V(network)$source == "EU",
                         V(network)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 2,
     vertex.label.color = "black",
     #  vertex.size=degree,
     #vertex.shape = ifelse(V(network)$source == "EU",
     #       "square","circle"),
     vertex.label.family = "sans",
     layout = l
   )

legend(x=-1.15,y=-.85, legend = c("Swedish Legislation","EU Legislation", "Danish Legislation"), pch=21,
       
       col=col, pt.bg=col, pt.cex=2, cex=2, bty="n", ncol=1)

##


dk.searchkey <-
  DKEUlinks %>%
  distinct(url, .keep_all = TRUE ) %>% # make sure no duplicate rows bc of multiple labeles/themes/citations
  mutate(url = str_replace_all(url, "https://www.retsinformation.dk","")) %>%
  select(url, search.term) %>%
  rename("doc.id" = "url")

se.searchkey <-
  SEEUlinks %>%
  distinct(doc.id, .keep_all = TRUE)%>%
  select(doc.id, search.term)

search.key <- rbind(se.searchkey, dk.searchkey)

EUlinks.terms <- 
  EUlinks %>%
  mutate(country = case_when(
    str_detect(from,"sfs-") ~ "SE",
    TRUE ~ "DK")) %>%
  left_join(., search.key, by=c("from"="doc.id"))


EUlinks.fisheries <- 
  EUlinks.terms %>% filter(search.term == "fiske" |
                             search.term ==  "fiskeri") %>%
  select(from,to)




fisheries.vertices <- 
  vertices %>%
  filter(vertices %in% EUlinks.fisheries$from |
           vertices %in% EUlinks.fisheries$to  )

network.f <- graph_from_data_frame(d=EUlinks.fisheries, directed = FALSE, vertices = fisheries.vertices)


col <- c( "#d1050c", "#FFCC00","#004B87")
V(network.f)$color <- col[as.numeric(as.factor(V(network.f)$source))]
degree <- degree(network.f)

median(degree)
#2
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#     1    1    2    3    9   47 

l.f <- layout.fruchterman.reingold(network.f)
sort(degree)

plot(network.f,
     vertex.label=ifelse(degree(network.f) >=9 & V(network.f)$source == "EU",
                         V(network.f)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 2.5,
     vertex.label.color = "black",
     #  vertex.size=degree,
     vertex.shape = ifelse(V(network.f)$source == "EU",
            "square","circle"),
     vertex.label.family = "sans",
     layout = l.f
)


legend(x=-1.15,y=-.85, legend = c("Danish Legislation", "EU Legislation","Swedish Legislation"), pch=21,
       
       col=col, pt.bg=col, pt.cex=2, cex=2, bty="n", ncol=1)

# HUNTING NETWORK
EUlinks.hunting <- 
  EUlinks.terms %>% filter(search.term == "jakt" |
                             search.term ==  "jagt") %>%
  select(from,to)




hunting.vertices <- 
  vertices %>%
  filter(vertices %in% EUlinks.hunting$from |
           vertices %in% EUlinks.hunting$to  )

network.h <- graph_from_data_frame(d=EUlinks.hunting, directed = FALSE, vertices = hunting.vertices)


col <- c( "#d1050c", "#FFCC00","#004B87")
V(network.h)$color <- col[as.numeric(as.factor(V(network.h)$source))]
degree <- degree(network.h)

median(degree)
#1
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#    1    1    1    2    4   60

l.h <- layout.fruchterman.reingold(network.h)
sort(degree)

plot(network.h,
     vertex.label=ifelse(degree(network.h) >=4 & V(network.h)$source == "EU",
                         V(network.h)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 2.5,
     vertex.label.color = "black",
     #  vertex.size=degree,
     vertex.shape = ifelse(V(network.h)$source == "EU",
                           "square","circle"),
     vertex.label.family = "sans",
     layout = l
)


legend(x=-1.15,y=-.85, legend = c("Danish Legislation", "EU Legislation","Swedish Legislation"), pch=21,
       
       col=col, pt.bg=col, pt.cex=2, cex=2, bty="n", ncol=1)

# maritime NETWORK
EUlinks.maritime <- 
  EUlinks.terms %>% filter(search.term == "sjofart" |
                             search.term ==  "sotrafik") %>%
  select(from,to)




maritime.vertices <- 
  vertices %>%
  filter(vertices %in% EUlinks.maritime$from |
           vertices %in% EUlinks.maritime$to  )

network.m <- graph_from_data_frame(d=EUlinks.maritime, directed = FALSE, vertices = maritime.vertices)


col <- c( "#d1050c", "#FFCC00","#004B87")
V(network.m)$color <- col[as.numeric(as.factor(V(network.m)$source))]
degree <- degree(network.m)

median(degree)
#1
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#    1.0  1.0  1.0  2.0  6.1 63.0

l.m <- layout.fruchterman.reingold(network.m)
sort(degree)

plot(network.m,
     vertex.label=ifelse(degree(network.m) >=6.1 & V(network.m)$source == "DK",
                         V(network.m)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 2.5,
     vertex.label.color = "black",
     #  vertex.size=degree,
     vertex.shape = ifelse(V(network.m)$source == "EU",
                           "square","circle"),
     vertex.label.family = "sans",
     layout = l.m
)


legend(x=-1.15,y=-.85, legend = c("Danish Legislation", "EU Legislation","Swedish Legislation"), pch=21,
       
       col=col, pt.bg=col, pt.cex=2, cex=2, bty="n", ncol=1)


par(mfrow=c(2,2))

plot(network.h,
     vertex.label=ifelse(degree(network.h) >=4 & V(network.h)$source == "EU",
                         V(network.h)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = .75,
     vertex.size= 3.75,
     vertex.label.color = "black",
     #  vertex.size=degree,
   #  vertex.shape = ifelse(V(network.h)$source == "EU",
    #                       "square","circle"),
     vertex.label.family = "sans",
     layout = l.h
)
title("Hunting",cex.main=1)

plot(network.m,
     vertex.label=ifelse(degree(network.m) >=6.1 & V(network.m)$source == "DK",
                         V(network.m)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = .75,
     vertex.size= 3.75,
     vertex.label.color = "black",
     #  vertex.size=degree,
  #   vertex.shape = ifelse(V(network.m)$source == "EU",
  #                         "square","circle"),
     vertex.label.family = "sans",
     layout = l.m
)
title("Maritime Traffic",cex.main=1)


plot(network.f,
     vertex.label=ifelse(degree(network.f) >=9 & V(network.f)$source == "EU",
                         V(network.f)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = .75,
     vertex.size= 3.75,
     vertex.label.color = "black",
     #  vertex.size=degree,
   #  vertex.shape = ifelse(V(network.f)$source == "EU",
    #                       "square","circle"),
     vertex.label.family = "sans",
     layout = l.f
)
title("Fisheries",cex.main=1)


legend(x=-1.15,y=-.85, legend = c("Danish Legislation", "EU Legislation","Swedish Legislation"), pch=21,
       
       col=col, pt.bg=col, pt.cex=2, cex=2, bty="n", ncol=1)

