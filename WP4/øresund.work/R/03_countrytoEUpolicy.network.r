
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
library("tidyr")
library("tibble")
library("ggplot2")
library("ggwordcloud")
library("patchwork")
library("kableExtra")


# Load data --------------------------------------------------------------------

DKEUlinks <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.DKEUlinks.csv")
SEEUlinks <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02SE.EU.links.csv")

# Our document-data key
document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

document.key.df <- 
  document.key.df %>%
  filter(!is.na(celex))

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
# 211 dk documents link to an EU doc
n_distinct(DKEUlinks1$to)
# 243 EU legal acts link 


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

dkeu.network <- graph_from_data_frame(d=DKEUlinks1, directed = TRUE, vertices = vertices)
print(dkeu.network, e=TRUE, v=TRUE)


col.dk <- c("#d1050c", "#FFCC00")
V(dkeu.network)$color <- col.dk[as.numeric(as.factor(V(dkeu.network)$source))]
# DK doc are the red ones...
degree <- igraph::degree(dkeu.network)

median(degree)
#1.5
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#    1.0  1.0  1.5  3.0  7.0 82.0

l.dk <- layout.fruchterman.reingold(dkeu.network)
sort(degree)

table(V(dkeu.network)$source == "EU") #243
table(V(dkeu.network)$source == "DK") #211
(V(dkeu.network)) # 454 vertices

saveRDS(dkeu.network, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/dkeu.networkigraph.rds")

png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/DK.eu.network.png",
    width = 1500, height = 1675)

plot(dkeu.network,
     vertex.label= NA, 
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 3.5,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     layout = l.dk)
#labels are those EU docs that are >= the 95% percentile for the degree number

legend(x=-1.15,y=-.85, legend = c("Danish Legislation","EU Legislation"), pch=21,
       
       col=col, pt.bg=col, pt.cex=2, cex=2, bty="n", ncol=1)
                                                              
                               

legend(x=-1.2,y=-1.025, c("31992L0043: Protecting Europe’s biodiversity (Natura 2000)",
                      "32009L0147: Conservation of wild birds",
                      "32004L0035: The polluter-pays principle and environmental liability",
                      "32006L0123: The EU’s services directive",
                      "32011L0092: Assessment of the effects of projects on the environment (EIA)",
                      "31979L0409: Council Directive 79/409/EEC of 2 April 1979 on the conservation of wild birds",
                      "32000L0060: Good-quality water in Europe (EU water directive)",
                      "32013R1303: Provisions on the European Regional Development Fund, European Social Fund, Cohesion Fund,",
                         "European Agricultural Fund for Rural Development and European Maritime and Fisheries Fund...",
                      "31991L0676: Fighting water pollution from agricultural nitrates",
                      "32014R0508: European Maritime and Fisheries Fund (2014-2020)",
                      "32021R1060: Common rules on EU funds (2021–2027)",
                      "32021R1139: European Maritime, Fisheries and Aquaculture Fund (2021–2027)",
                      "32014L0052: Assessment of the effects of projects on the environment (EIA)",
                      "32018L2001: Renewable energy",
                      "32013R1380: The EU’s common fisheries policy"),
                       cex=1.25,
                       ncol=2,
                       col="#777777",
                       bty="n", # no box around the legend
                      )

dev.off()

# modules ------------------------------------------------------

library("bipartite")

DKEU.matrix <- 
  DKEUlinks1 %>%
  mutate(links=1) %>%
  pivot_wider(names_from = from, values_from = links,values_fill = 0) %>%
  column_to_rownames(var = "to") #higher trophic level (country docs) is the columns

#DKEU.modules <- computeModules(DKEU.matrix) # ran and saved on Nov. 24th 2022
#saveRDS(DKEU.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.DKEU.modules.RDS") 
DKEU.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.DKEU.modules.RDS")
plotModuleWeb(DKEU.modules)
listModuleInformation(DKEU.modules)
printoutModuleInformation(DKEU.modules) # total 18 modules

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
# 92 dk documents link to an EU doc
n_distinct(SEEUlinks1$to)
# 301 EU legal acts link 


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

seeu.network <- graph_from_data_frame(d=SEEUlinks1, directed = TRUE, vertices = vertices)
print(seeu.network, e=TRUE, v=TRUE)


col.se <- c("#FFCC00", "#004B87")
V(seeu.network)$color <- col.se[as.numeric(as.factor(V(seeu.network)$source))]


degree <- igraph::degree(seeu.network)

median(degree)
#1
sort(degree)


quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#   1    1    1    2   10   30  


l.se <- layout.fruchterman.reingold(seeu.network)


table(V(seeu.network)$source == "EU") #301
table(V(seeu.network)$source == "SE") #92
(V(seeu.network)) # 393 vertices

saveRDS(seeu.network, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/seeu.networkigraph.rds")


plot(seeu.network,
     vertex.label=ifelse(igraph::degree(seeu.network) >=11 & V(seeu.network)$source == "EU",
                         V(seeu.network)$name,NA),
     vertex.label.cex = .75,
     vertex.size= 3,
     layout = l.se)
#labels are those EU docs that are >= the 95% percentile for the degree number

# modules ------------------------------------------------------

library("bipartite")

SEEU.matrix <- 
  SEEUlinks1 %>%
  mutate(links=1) %>%
  pivot_wider(names_from = from, values_from = links ,values_fill = 0) %>%
  column_to_rownames(var = "to") #higher trophic level (EU docs) is the columns

#SEEU.modules <- computeModules(SEEU.matrix) #computed Nov 24th, 2022 
#saveRDS(SEEU.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.SEEU.modules.RDS") 
SEEU.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.SEEU.modules.RDS")
plotModuleWeb(SEEU.modules)
printoutModuleInformation(SEEU.modules) # total 22 modules

## plot them side-by-side for the report -----------------


png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/countries.networks.png",
    width = 1750, height = 1250)

par(mfrow=c(1,2))

set.seed(5)

plot(dkeu.network,
     vertex.label= NA, #ifelse(igraph::degree(dkeu.network) >=7 & V(dkeu.network)$source == "EU",
                       #  V(dkeu.network)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 3.5,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     #layout = l.dk
     )

title("(A) Denmark to EU legisaltion",cex.main=2)
legend(x=-1.15,y=1.25, legend = c("Danish Legislation", "EU Legislation"), pch=21,
       col=col.dk, pt.bg=col.dk, pt.cex=2, cex=2, bty="n", ncol=1)

set.seed(1)
plot(seeu.network,
     vertex.label= NA, # ifelse(igraph::degree(seeu.network) >=10 & V(seeu.network)$source == "EU",
                        # V(seeu.network)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 3.5,
     vertex.label.color = "black",
     vertex.label.family = "sans",
    # layout = l.se
     )
title("(B) Sweden to EU legislation",cex.main=2)
legend(x=-1.15,y=1.25, legend = c("EU Legislation", "Swedish Legislation"), pch=21,
       col=col.se, pt.bg=col.se, pt.cex=2, cex=2, bty="n", ncol=1)

dev.off()



################################################################################
######################        SWEDEN  & DENMARK        #########################
################################################################################
EUlinks <- rbind(SEEUlinks1,DKEUlinks1)

vertices <- rbind(SE.vertices,eu.vertices2,dk.vertices,eu.vertices) %>% distinct()


network <- graph_from_data_frame(d=EUlinks, directed = TRUE, vertices = vertices)

print(network, e=TRUE, v=TRUE)


library(RColorBrewer)

col <- c("#d1050c", "#FFCC00", "#004B87")
V(network)$color <- col[as.numeric(as.factor(V(network)$source))]
# DK doc are the red ones...
degree <- igraph::degree(network)

median(degree)
#1
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
# 1    1    1    3    9   85 

l <- layout.fruchterman.reingold(network)
sort(degree)

plot(network,
     vertex.label= NA,
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 2,
     vertex.label.color = "black",
     #  vertex.size=degree,
     vertex.shape = ifelse(V(network)$source == "EU",
            "square","circle"),
     vertex.label.family = "sans",
     layout = l
   )

legend(x=-1.15,y=-.85, legend = c("Swedish Legislation","EU Legislation", "Danish Legislation"), pch=21,
       
       col=col, pt.bg=col, pt.cex=2, cex=2, bty="n", ncol=1)


############# Now make an network based on similar citations #########

# this can be turned into an matrix... linking the country legislation direclty
adj.EUlinks <- inner_join(SEEUlinks1,DKEUlinks1, by = "to") %>% 
  select(-to) %>%
  mutate(link=1) %>%
  group_by(from.x, from.y) %>%
  summarise(n=sum(link))

summary(adj.EUlinks$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.000   1.000   1.000   1.397   2.000   4.000 


adj.matrix <- 
  adj.EUlinks %>%
  pivot_wider(names_from = from.y, values_from = n, values_fill=0)

adj.dk.vertices <- 
  dk.vertices %>%
  filter(vertices %in% adj.EUlinks$from.y)

adj.se.vertices <- 
  SE.vertices %>%
  filter(vertices %in% adj.EUlinks$from.x)

adj.vertices <- rbind(adj.se.vertices,adj.dk.vertices )

adj.EUlinks <- 
  adj.EUlinks %>% rename("from" = "from.x",
                          "to" = "from.y")

network.SEDK.adj <- graph_from_data_frame(d=adj.EUlinks, directed = FALSE, vertices = adj.vertices)
l <- layout.fruchterman.reingold(network.SEDK.adj)

col <- c("#d1050c", "#004B87")
V(network.SEDK.adj)$color <- col[as.numeric(as.factor(V(network.SEDK.adj)$source))]
# DK doc are the red ones...
degree <- igraph::degree(network.SEDK.adj)

median(degree)
#3
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  1    2    3    4   10   89 

saveRDS(network.SEDK.adj, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/network.SEDK.igraph.rds")

plot(network.SEDK.adj,
     edge.width=E(network.SEDK.adj)$n,
     
     vertex.label=ifelse(igraph::degree(network.SEDK.adj) >=13 ,
                         V(network.SEDK.adj)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 3,
     vertex.label.color = "black",
     #  vertex.size=degree,
     vertex.label.family = "sans",
     layout = l
)

# network stats ---------------

library("bipartite")

adj.matrix <- 
  adj.matrix %>%
  column_to_rownames(var = "from.x") 

#set.seed(01)
#SEDK.modules <- computeModules(adj.matrix) #ran Jan 2nd, 2022
#saveRDS(SEDK.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.SEDK.modules.RDS") 

SEDK.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.SEDK.modules.RDS")
plotModuleWeb(SEDK.modules)


mod.1 <- listModuleInformation(SEDK.modules)[[2]][[1]] %>% unlist()
mod.2 <- listModuleInformation(SEDK.modules)[[2]][[2]] %>% unlist()
mod.3 <- listModuleInformation(SEDK.modules)[[2]][[3]] %>% unlist()
mod.4 <- listModuleInformation(SEDK.modules)[[2]][[4]] %>% unlist()
mod.5 <- listModuleInformation(SEDK.modules)[[2]][[5]] %>% unlist()
mod.6 <- listModuleInformation(SEDK.modules)[[2]][[6]] %>% unlist()

mod.7 <- listModuleInformation(SEDK.modules)[[2]][[7]] %>% unlist()
# now split mod 7 into two mod 6 and create new mod 8 this is bc this function groups all those not connected to the main one into one, but they are actually seperate.
mod.7 <- c( "sfs-2011-1088", "sfs-2010-598" , "/eli/lta/2021/1352", 
            "/eli/lta/2021/2167", "/eli/lta/2021/2237" ,
            "/eli/lta/2021/2520" , "/eli/lta/2021/2536",
            "/eli/lta/2021/923" , "/eli/lta/2022/642")

mod.8 <- c("sfs-1992-1300", "sfs-1992-1303", "/eli/lta/2021/1736")

modules <- list(mod.1, mod.2, mod.3, mod.4,
                 mod.5, mod.6, mod.7, mod.8)
names(modules) <- c("1","2","3","4","5","6","7", "8")

plotModuleWeb(SEDK.modules, labsize = .55)

indices <- c( "degree","PDI","nestedrank")

h.SEDK.networkstats <- 
  specieslevel(adj.matrix, index=  indices, level = "higher",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)



l.SEDK.networkstats <- 
  specieslevel(adj.matrix, index=  indices, level = "lower",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(h.SEDK.networkstats) # dk
#degree         nestedrank        PDI        
#Min.   :1.000   Min.   :0.00   Min.   :0.8780  
#1st Qu.:2.000   1st Qu.:0.25   1st Qu.:0.9512  
#Median :3.000   Median :0.50   Median :0.9634  
#Mean   :3.026   Mean   :0.50   Mean   :0.9641  
#3rd Qu.:3.000   3rd Qu.:0.75   3rd Qu.:0.9756  
#Max.   :9.000   Max.   :1.00   Max.   :1.0000 

h.SEDK.networkstats %>%
  slice_max(., order_by = nestedrank, n=3)%>%
  kable(., "latex")
#/eli/lta/2022/162 & 1 & 1.0000000 & 1\\
#/eli/lta/2022/1207 & 1 & 0.9934211 & 1\\
#/eli/lta/2022/1155 & 1 & 0.9868421 & 1\\

h.SEDK.networkstats %>%
  slice_min(., order_by = nestedrank, n=3)%>%
  kable(., "latex")
#/eli/lta/2022/100 & 9 & 0.0000000 & 0.9105691\\
#/eli/lta/2022/787 & 8 & 0.0065789 & 0.9430894\\
#/eli/lta/2022/988 & 8 & 0.0131579 & 0.9024390\\

h.SEDK.networkstats %>%
  slice_max(., order_by = nestedrank, n=1)
#                     has    degree nestedrank PDI
#/eli/lta/2022/162      1          1   1
# most specialized

h.SEDK.networkstats %>%
  slice_max(., order_by = PDI, n=1) # specalist

h.SEDK.networkstats %>%
  slice_min(., order_by = nestedrank, n=1)  # generalist
#              degree nestedrank PDI
# /eli/lta/2022/100      9          0 0.9105691

h.SEDK.networkstats %>%
  slice_min(., order_by = PDI, n=1) # # generalist
# /eli/lta/2021/2249      6 0.03947368 0.8780488
# /eli/lta/2022/964       6 0.04605263 0.8780488

summary(l.SEDK.networkstats) # se
# Min.   : 1.00   Min.   :0.00   Min.   :0.6623  
#1st Qu.: 2.00   1st Qu.:0.25   1st Qu.:0.9605  
#Median : 4.00   Median :0.50   Median :0.9825  
#Mean   :11.02   Mean   :0.50   Mean   :0.9591  
#3rd Qu.: 9.50   3rd Qu.:0.75   3rd Qu.:0.9951  
#Max.   :89.00   Max.   :1.00   Max.   :1.0000 


l.SEDK.networkstats %>%
  slice_max(., order_by = nestedrank, n=3)%>%
  kable(., "latex")
#sfs-2021-194 & 1 & 1.0000000 & 1\\
#sfs-2020-838 & 1 & 0.9756098 & 1\\
#sfs-2011-1494 & 1 & 0.9512195 & 1\\

l.SEDK.networkstats %>%
  slice_min(., order_by = nestedrank, n=3)%>%
  kable(., "latex")
#sfs-1998-808 & 89 & 0.0000000 & 0.6622807\\
#sfs-2007-845 & 85 & 0.0243902 & 0.6710526\\
#sfs-1998-1252 & 82 & 0.0487805 & 0.7335526\\


l.SEDK.networkstats %>%
  slice_max(., order_by = nestedrank, n=1)
#                     has    degree nestedrank PDI
# sfs-2021-194      1          1   1  # most specialized

l.SEDK.networkstats %>%
  slice_max(., order_by = PDI, n=1) # specalist


l.SEDK.networkstats %>%
  slice_min(., order_by = nestedrank, n=1)  # generalist
#              degree nestedrank PDI
# sfs-1998-808     89          0 0.6622807

l.SEDK.networkstats %>%
  slice_min(., order_by = PDI, n=1) # # generalist
# sfs-1998-808     89          0 0.6622807 


SEDK.networkstats <- 
  rbind(l.SEDK.networkstats,h.SEDK.networkstats) %>%
  rownames_to_column(., var = "name")

network.SEDK.adj1 <- network.SEDK.adj

png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/DKSEeu.networks.clusters.png",
    width = 1750, height = 1250)

par(mfrow=c(1,2))

set.seed(1)
plot(network.SEDK.adj,
     edge.width=E(network.SEDK.adj)$n,
     
     vertex.label= NA, #ifelse(degree(network.SEDK.adj) >=13.05 ,
     #                     V(network.SEDK.adj)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = .75,
     vertex.size= 4,
     vertex.label.color = "black",
     vertex.label.family = "sans"
)
title("(A)",cex.main=2)

set.seed(1)
plot(network.SEDK.adj1, mark.groups=modules, #network.SEDK.adj,
     mark.col = rainbow(8,alpha =.25),
     mark.border=NA,
     edge.width=E(network.SEDK.adj1)$n,
     vertex.label = NA,
     vertex.label.cex = .75,
     vertex.size= 4)
title("(B)",cex.main=2)

dev.off()


# ok lets see what discussions are similar between the countries by plotting the EuroVOc word clouds associated to these modules
SE.adj.EUlinkscelexs <- 
  inner_join(SEEUlinks1,DKEUlinks1, by = "to") %>% 
  select(-from.y) %>%
  distinct() %>%
  rename("from" = "from.x",
         "to" = "to")
colnames(SE.adj.EUlinkscelexs)

DK.adj.EUlinkscelexs <- 
  inner_join(SEEUlinks1,DKEUlinks1, by = "to") %>% 
  select(-from.x) %>% 
  distinct() %>%
  rename("from" = "from.y",
         "to" = "to") %>%
  select(from, to)
colnames(DK.adj.EUlinkscelexs)


document.label.key.df <- 
  document.key.df %>%
  select(celex, labels) %>%
  distinct() 

df.mod.1 <- listModuleInformation(SEDK.modules)[[2]][[1]] %>% unlist() %>% as.data.frame() %>% mutate(module="1")
df.mod.2 <- listModuleInformation(SEDK.modules)[[2]][[2]] %>% unlist() %>% as.data.frame() %>% mutate(module="2")
df.mod.3 <- listModuleInformation(SEDK.modules)[[2]][[3]] %>% unlist() %>% as.data.frame() %>% mutate(module="3")
df.mod.4 <- listModuleInformation(SEDK.modules)[[2]][[4]] %>% unlist() %>% as.data.frame() %>% mutate(module="4")
df.mod.5 <- listModuleInformation(SEDK.modules)[[2]][[5]] %>% unlist() %>% as.data.frame() %>% mutate(module="5")
df.mod.6 <- listModuleInformation(SEDK.modules)[[2]][[6]] %>% unlist() %>% as.data.frame() %>% mutate(module="6")
df.mod.7.edit <- listModuleInformation(SEDK.modules)[[2]][[7]] %>% unlist() %>% as.data.frame() %>% mutate(module="7")
#df.mod.8 <- listModuleInformation(SEDK.modules)[[2]][[8]] %>% unlist() %>% as.data.frame() %>% mutate(module="8")
df.mod.7 <- df.mod.7.edit[-c(1,2,5),]
df.mod.8 <- df.mod.7.edit[c(1,2,5),]%>%
  mutate(module = "8")

df.modules <- rbind(df.mod.1, df.mod.2, df.mod.3, df.mod.4,
                df.mod.5, df.mod.6, df.mod.7,df.mod.8) 

adj.network.df1 <-
  SEDK.networkstats %>%
  left_join(., SE.adj.EUlinkscelexs, by = c("name" = "from")) %>%
  left_join(., DK.adj.EUlinkscelexs, by = c("name" = "from")) %>%
  mutate(to = coalesce(to.x,to.y)) %>%
  select(-to.x, -to.y) %>%
  left_join(., document.label.key.df, by = c("to" = "celex")) %>%
  left_join(., df.modules, by = c("name" = ".")) 

adj.network.df1 %>%
  distinct(name,to,module) %>%
  group_by(module,to) %>%
  summarise(n=n_distinct(name)) %>% # how many leg. cite each celex in a module
  group_by(module) %>%
  filter(n == max(n)) # what is that top celex?
#   module to             n
#<chr>  <chr>      <int>
#  1      31992L0043    79
#2 2      32014R0651     5
#3 3      32000L0060     6
#4 4      32013R1303    13
#5 5      32017R0625     9
#6 6      32009L0013     4
#7 6      32009L0016     4
#8 7      32018L2001     9
#9 8      32009L0043     3

adj.network.df1 %>%
  distinct(name,to,module) %>%
  group_by(module,to) %>%
  summarise(n=n_distinct(name)) %>%
  group_by(module,n) %>%
  mutate(EU.Legislation = paste0(to, collapse = ", ")) %>%
  arrange(module,n) %>% 
  select(-to) %>%
  distinct() %>%
  kable(., "latex")
  

adj.network.df1 %>%
  distinct(to,module) %>%
  group_by(module) %>%
  mutate(EU.Legislation = paste0(to, collapse = ", ")) %>%
  select(-to) %>%
  distinct() %>%
  arrange(module) %>% 
  kable(., "latex")

# number of associated EU leg. to each module
adj.network.df1 %>%
  distinct(to,module) %>%
  group_by(module) %>%
  summarise(n=n_distinct(to)) 
#  module     n
#<chr>  <int>
#1 1         12
#2 2         19
#3 3         15
#4 4          6
#5 5          8
#6 6          1
#7 7         17
#8 8          1



# how many modules is a celex associated with?
x <- adj.network.df1 %>%
  distinct(to,module) %>%
  group_by(to) %>%
  summarise(n=n_distinct(module)) %>%
  arrange(desc(n))

cluster.labels <- 
  adj.network.df1 %>%
  group_by(module,labels) %>%
  summarise(n.country.doc = n_distinct(name))

rainbow(8)
#"#FF0000" "#FFBF00" "#80FF00" "#00FF40" "#00FFFF" "#0040FF" "#8000FF" "#FF00BF"
MOD.1.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "1") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#FF0000") +
  ggtitle("Module 1: Species Protection & Biodiversity") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

MOD.2.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "2") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#FFBF00")+
  ggtitle("Module 2: Government aid  ") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))


MOD.3.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "3") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#80FF00")+
  ggtitle("Module 3: Water managment and protection ") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

MOD.4.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "4") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#00FF40") +
  ggtitle("Module 4: Fisheries managment, policy & funding") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

MOD.5.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "5") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#00FFFF")+
  ggtitle("Module 5: Food quality, health, and safety ") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

MOD.6.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "6") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#0040FF")+
  ggtitle("Module 6: Vessel safety and environmental impact") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

MOD.7.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "7") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#8000FF") +
  ggtitle("Module 7: Energy consumption") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

MOD.8.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "8") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  scale_color_manual(values = "#FF00BF") +
  ggtitle("Module 8: Defense") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

(MOD.1.PLOT + MOD.2.PLOT + MOD.3.PLOT)/
(MOD.4.PLOT + MOD.5.PLOT + MOD.6.PLOT)/ 
(MOD.7.PLOT + MOD.8.PLOT)


 ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/sharedclusters.png", 
       width = 150, height = 55, units = "cm",
       limitsize = FALSE)

# total number of country (DK and SE) legislation in each module
adj.network.df1 %>%
  select(-labels,-to) %>%
  distinct() %>%
  mutate(country = case_when(
    str_detect(name,"sfs-") ~ "SE",
    TRUE ~ "DK")) %>%
  mutate(module = as.factor(module)) %>%
  group_by(module) %>%
  summarise(n.country = n()) 
#module n.country
#   1             82
#2 2              9
#3 3             11
#4 4             42
#5 5             25
#6 6             14
#7 7              9
#8 8              3

# total number of DK legislation in each module
adj.network.df1 %>%
  select(-labels,-to) %>%
  distinct() %>%
  mutate(country = case_when(
    str_detect(name,"sfs-") ~ "SE",
    TRUE ~ "DK")) %>%
  filter(country == "DK") %>%
  mutate(module = as.factor(module)) %>%
  group_by(module) %>%
  summarise(n.country = n()) 
#module n.country
#  1             79
#  2              4
#  3              9
#  4             33
#  5             13
#  6              7
#  7              7
#  8              1

# total number of SE legislation in each module
adj.network.df1 %>%
  select(-labels,-to) %>%
  distinct() %>%
  mutate(country = case_when(
    str_detect(name,"sfs-") ~ "SE",
    TRUE ~ "DK")) %>%
  filter(country == "SE") %>%
  mutate(module = as.factor(module)) %>%
  group_by(module) %>%
  summarise(n.country = n()) 
#module n.country
#  1 1              3
#  2 2              5
#  3 3              2
#  4 4              9
#  5 5             12
#  6 6              7
#  7 7              2
#  8 8              2

#################### Now subset via the query search term ######################

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

# -------- Fisheries network ------------------------

EUlinks.fisheries <- 
  EUlinks.terms %>% filter(search.term == "fiske" |
                             search.term ==  "fiskeri") %>%
  select(from,to)


fisheries.vertices <- 
  vertices %>%
  filter(vertices %in% EUlinks.fisheries$from |
           vertices %in% EUlinks.fisheries$to  )

network.f <- graph_from_data_frame(d=EUlinks.fisheries, directed = TRUE, vertices = fisheries.vertices)


col <- c( "#d1050c", "#FFCC00","#004B87")
V(network.f)$color <- col[as.numeric(as.factor(V(network.f)$source))]
degree <- igraph::degree(network.f)

median(degree)
#1
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#  1    1    1    3    9   48 

l.f <- layout.fruchterman.reingold(network.f)
sort(degree)

saveRDS(network.f, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/network.fisheries.igraph.rds")

plot(network.f,
     vertex.label=ifelse(igraph::degree(network.f) >=9 & V(network.f)$source == "EU",
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

table(V(network.f)$source == "EU") #306 
table(V(network.f)$source == "DK") #155
table(V(network.f)$source == "SE") #39
(V(network.f)) # 500 vertices

ifelse(igraph::degree(network.f) >=9 & V(network.f)$source == "EU",
       V(network.f)$name,NA)

# -------- Hunting network ------------------------------

EUlinks.hunting <- 
  EUlinks.terms %>% filter(search.term == "jakt" |
                             search.term ==  "jagt") %>%
  select(from,to)

hunting.vertices <- 
  vertices %>%
  filter(vertices %in% EUlinks.hunting$from |
           vertices %in% EUlinks.hunting$to  )

network.h <- graph_from_data_frame(d=EUlinks.hunting, directed = TRUE, vertices = hunting.vertices)


col <- c( "#d1050c", "#FFCC00","#004B87")
V(network.h)$color <- col[as.numeric(as.factor(V(network.h)$source))]
degree <- igraph::degree(network.h)

median(degree)
#1
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#     1    1    1    2    4   37 

l.h <- layout.fruchterman.reingold(network.h)
sort(degree)

saveRDS(network.h, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/network.hunting.igraph.rds")

plot(network.h,
     vertex.label=ifelse(igraph::degree(network.h) >=4 & V(network.h)$source == "EU",
                         V(network.h)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = 1,
     vertex.size= 2.5,
     vertex.label.color = "black",
     #  vertex.size=degree,
     vertex.shape = ifelse(V(network.h)$source == "EU",
                           "square","circle"),
     vertex.label.family = "sans",
     layout = l.h
)


legend(x=-1.15,y=-.85, legend = c("Danish Legislation", "EU Legislation","Swedish Legislation"), pch=21,
       
       col=col, pt.bg=col, pt.cex=2, cex=2, bty="n", ncol=1)

table(V(network.h)$source == "EU") #101
table(V(network.h)$source == "DK") #55
table(V(network.h)$source == "SE") #17
(V(network.h)) # 179 vertices


# ------- maritime traffic network ------- 

EUlinks.maritime <- 
  EUlinks.terms %>% filter(search.term == "sjofart" |
                             search.term ==  "sotrafik") %>%
  select(from,to)

maritime.vertices <- 
  vertices %>%
  filter(vertices %in% EUlinks.maritime$from |
           vertices %in% EUlinks.maritime$to  )

network.m <- graph_from_data_frame(d=EUlinks.maritime, directed = TRUE, vertices = maritime.vertices)


col <- c( "#d1050c", "#FFCC00","#004B87")
V(network.m)$color <- col[as.numeric(as.factor(V(network.m)$source))]
degree <- igraph::degree(network.m)

median(degree)
#1
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
#   1.0  1.0  1.0  2.0  5.9 29.0  

l.m <- layout.fruchterman.reingold(network.m)
sort(degree)

saveRDS(network.m, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/network.maritime.igraph.rds")


plot(network.m,
     vertex.label=ifelse(igraph::degree(network.m) >=6.05 & V(network.m)$source == "DK",
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


table(V(network.m)$source == "EU") #186   
table(V(network.m)$source == "DK") #1
table(V(network.m)$source == "SE") #36 
(V(network.m)) # 302 vertices


# -------- plot them all together -----------

png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/DKSEeu.subset.networks.png",
       width = 1750, height = 1750)

par(mfrow=c(2,2))

plot(network.h,
     vertex.label= NA, #ifelse(degree(network.h) >=4 & V(network.h)$source == "EU",
   #                      V(network.h)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = .75,
     vertex.size= 3.75,
     vertex.label.color = "black",
     arrow.size = .55,
     arrow.width = .25,
     vertex.label.family = "sans",
     layout = l.h
)
title("(A) Hunting",cex.main=2)

legend(x=.55,y=-.95, legend = c("Danish Legislation", "EU Legislation","Swedish Legislation"), pch=21,
       
       col=col, pt.bg=col, pt.cex=3, cex=2, bty="n", ncol=1)

plot(network.m,
     vertex.label= NA, #ifelse(degree(network.m) >=6.1 & V(network.m)$source == "DK",
   #                      V(network.m)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = .75,
     vertex.size= 3.75,
     vertex.label.color = "black",
     arrow.size = .55,
     arrow.width = .25,
     vertex.label.family = "sans",
     layout = l.m
)
title("(B) Maritime Traffic",cex.main=2)


plot(network.f,
     vertex.label= NA, #ifelse(degree(network.f) >=9 & V(network.f)$source == "EU",
    #                     V(network.f)$name,NA),
     vertex.frame.color = "white",
     vertex.label.cex = .75,
     vertex.size= 3.75,
     vertex.label.color = "black",
     arrow.size = .15,
     arrow.width = .15,
     vertex.label.family = "sans",
     layout = l.f
)
title("(C) Fisheries",cex.main=2)


#plot(network.SEDK.adj,
#     edge.width=E(network.SEDK.adj)$n,
     
#     vertex.label= NA, #ifelse(degree(network.SEDK.adj) >=13.05 ,
    #                     V(network.SEDK.adj)$name,NA),
#vertex.frame.color = "white",
 #    vertex.label.cex = .75,
 #    vertex.size= 3.75,
 #    vertex.label.color = "black",
 #    vertex.label.family = "sans",
#     layout = l
#)
#title("(D) All Legislation",cex.main=2)

dev.off()


