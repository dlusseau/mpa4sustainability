
# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("dplyr")
library("stringr")
library("igraph")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------

DKEUlinks <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.DKEUlinks.csv")

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
V(network)$color <- col[as.numeric(as.factor(V(network)$source))]
# DK doc are the red ones...
degree <- degree(network)

median(degree)
#2
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#1.00  1.00  2.00  3.00  7.05 82.00 

l <- layout.fruchterman.reingold(network)

plot(network,
     vertex.label=ifelse(degree(network) >7.05 & V(network)$source == "EU",
                         V(network)$name,NA),
     vertex.label.cex = .75,
     vertex.size= 3,
     layout = l)
#labels are those EU docs that are > the 95% percentile for the degree number

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

plot_dendrogram(network.clusters)

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

install.packages("viridis")  # Install
library("viridis")           # Load
require(graphics)

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
as.numeric(V(network)$color)

plot(network,
     vertex.color=V(network)$color,
     vertex.label=ifelse(degree(network) >7.05 & V(network)$source == "EU",
                         V(network)$name,NA),
     vertex.label.cex = .75,
     vertex.shape = ifelse(V(network)$source == "EU",
                           "circle","square"),
     vertex.size= 3,
     layout = l2)

# celes to label later
# 32021R1060
#"32021R1139"   31979L0409" 
#"31992L0043"  "32004L0035"             
#"32013R1303"  "32014R0508" 
# 32006L0123
# 32009L0147
# 31991L0676
#"32000L0060"             
#"32011L0092" 

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





