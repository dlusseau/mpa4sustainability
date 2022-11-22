
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

# Define functions -------------------------------------------------------------

# No defined function for this script

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

png(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/DK.eu.network.png",
    width = 1500, height = 1675)

plot(dkeu.network,
     vertex.label=ifelse(degree(dkeu.network) >=7 & V(dkeu.network)$source == "EU",
                         V(dkeu.network)$name,NA),
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

# network stats ------------------------------------------------------

library("bipartite")

DKEU.matrix <- 
  DKEUlinks1 %>%
  mutate(links=1) %>%
  pivot_wider(names_from = from, values_from = links,values_fill = 0) %>%
  column_to_rownames(var = "to") #higher trophic level (country docs) is the columns

#DKEU.modules <- computeModules(DKEU.matrix) # ran and saved on Nov. 22nd 2022
#saveRDS(DKEU.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.DKEU.modules.RDS") 
DKEU.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.DKEU.modules.RDS")
plotModuleWeb(DKEU.modules)
listModuleInformation(DKEU.modules)
printoutModuleInformation(DKEU.modules) # total 13 modules

indices <- c( "degree","PDI","nestedrank")

DKEU.CELEX.networkstats <- 
  specieslevel(DKEU.matrix, index=indices, level = "lower",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(DKEU.CELEX.networkstats)


DKEU.COUNTRY.networkstats <- 
  specieslevel(DKEU.matrix, index=indices, level = "higher",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(DKEU.COUNTRY.networkstats)       

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
#   1    1     1    3     11    63 


l.se <- layout.fruchterman.reingold(seeu.network)

plot(seeu.network,
     vertex.label=ifelse(igraph::degree(seeu.network) >=11 & V(seeu.network)$source == "EU",
                         V(seeu.network)$name,NA),
     vertex.label.cex = .75,
     vertex.size= 3,
     layout = l.se)
#labels are those EU docs that are >= the 95% percentile for the degree number

# network stats ------------------------------------------------------

library("bipartite")

SEEU.matrix <- 
  SEEUlinks1 %>%
  mutate(links=1) %>%
  pivot_wider(names_from = from, values_from = links ,values_fill = 0) %>%
  column_to_rownames(var = "to") #higher trophic level (EU docs) is the columns

#SEEU.modules <- computeModules(SEEU.matrix) #computed Nov 22nd, 2022 
#saveRDS(SEEU.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.SEEU.modules.RDS") 
SEEU.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.SEEU.modules.RDS")
plotModuleWeb(SEEU.modules)
printoutModuleInformation(SEEU.modules) # total 17 modules

indices <- c( "degree","PDI","nestedrank")

SEEU.CELEX.networkstats <- 
  specieslevel(SEEU.matrix, index=  indices, level = "lower",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(SEEU.CELEX.networkstats)

SEEU.CELEX.networkstats %>%
  slice_max(., order_by = nestedrank, n=1)
#            degree nestedrank PDI
#32013R0098      1  1.0000000   1 # most specialized
# Marketing and use of explosives precursors

SEEU.CELEX.networkstats %>%
  slice_max(., order_by = PDI, n=1) # specalist

SEEU.CELEX.networkstats %>%
  slice_min(., order_by = nestedrank, n=1)
#            degree  nestedrank       PDI # most generalized
#31999L0045     12 0.000000000 0.9098361
# Classification, packaging and labelling of dangerous preparations


SEEU.CELEX.networkstats %>%
  slice_min(., order_by = PDI, n=1)
# 31999L0045     12          0 0.9098361


SEEU.COUNTRY.networkstats <- 
  specieslevel(SEEU.matrix, index=  indices, level = "higher",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(SEEU.COUNTRY.networkstats)


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
     vertex.label= NA, #ifelse(igraph::degree(seeu.network) >=11 & V(seeu.network)$source == "EU",
                       #  V(seeu.network)$name,NA),
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

col <- c("#004B87", "#FFCC00", "#d1050c")
V(network)$color <- col[as.numeric(as.factor(V(network)$source))]
# DK doc are the red ones...
degree <- igraph::degree(network)

median(degree)
#2
quantile(degree,probs = c(0,.25,.5,.75,.95,1))
#  0%   25%   50%   75%   95%  100% 
# 1.0  1.0  2.0  3.0 10.8 85.0 

l <- layout.fruchterman.reingold(network)
sort(degree)

plot(network,
     vertex.label=ifelse(igraph::degree(network) >=85 & V(network)$source == "EU",
                         V(network)$name,NA),
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
#mutate(from.x = as.factor(from.x))%>%
#  mutate(from.y = as.factor(from.y))

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
# 1.00  2.00  3.00  5.25 13.05 97.00

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

#SEDK.modules <- computeModules(adj.matrix) #ran nov. 22, 2022
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
mod.8 <- listModuleInformation(SEDK.modules)[[2]][[8]] %>% unlist()

modules <- list(mod.1, mod.2, mod.3, mod.4,
                 mod.5, mod.6, mod.7, mod.8)
names(modules) <- c("1","2","3","4","5","6","7","8")

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
summary(l.SEDK.networkstats) # se

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


# ok lets see what discussions are similar between the countries: 
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
df.mod.7 <- listModuleInformation(SEDK.modules)[[2]][[7]] %>% unlist() %>% as.data.frame() %>% mutate(module="7")
df.mod.8 <- listModuleInformation(SEDK.modules)[[2]][[8]] %>% unlist() %>% as.data.frame() %>% mutate(module="8")

df.modules <- rbind(df.mod.1, df.mod.2, df.mod.3, df.mod.4,
                df.mod.5, df.mod.6, df.mod.7, df.mod.8)

adj.network.df1 <-
  SEDK.networkstats %>%
  left_join(., SE.adj.EUlinkscelexs, by = c("name" = "from")) %>%
  left_join(., DK.adj.EUlinkscelexs, by = c("name" = "from")) %>%
  mutate(to = coalesce(to.x,to.y)) %>%
  select(-to.x, -to.y) %>%
  left_join(., document.label.key.df, by = c("to" = "celex")) %>%
  left_join(., df.modules, by = c("name" = ".")) 

cluster.labels <- 
  adj.network.df1 %>%
  group_by(module,labels) %>%
  summarise(n.country.doc = n_distinct(name))

cluster.labs <- c("1" = "Cluster 1: Food health, safety, and quality", 
                  "2" = "Cluster 2: Defense and customes",
                  "3" = "Cluster 3: Environmental protection", 
                  "4" = "Cluster 4: Energy, emissions, pollution",
                  "5" = "Cluster 6: Sustainable fisheries")
  
rainbow(8,alpha =.25)
"#FF0000" "#FFBF00" "#80FF00" "#00FF40" "#00FFFF" "#0040FF" "#8000FF" "#FF00BF"

MOD.1.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "1") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_manual(values = "#FF0000") +
  ggtitle("Module 1") +
  theme(plot.title = element_text(hjust = 0.5))
 # facet_wrap(~module, ncol = 3,  scales = "free", shrink = FALSE)#,
          #   labeller = labeller(module = cluster.labs)) +
  #theme( strip.text.x = element_text(face="bold", size = 35)) 


MOD.2.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "2") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_manual(values = "#FFBF00")+
  ggtitle("Module 2") +
  theme(plot.title = element_text(hjust = 0.5))

MOD.3.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "3") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_manual(values = "#80FF00")+
  ggtitle("Module 3") +
  theme(plot.title = element_text(hjust = 0.5))

MOD.4.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "4") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_manual(values = "#00FF40") +
  ggtitle("Module 4") +
  theme(plot.title = element_text(hjust = 0.5))

MOD.5.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "5") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_manual(values = "#00FFFF")+
  ggtitle("Module 5") +
  theme(plot.title = element_text(hjust = 0.5))
# facet_wrap(~module, ncol = 3,  scales = "free", shrink = FALSE)#,
#   labeller = labeller(module = cluster.labs)) +
#theme( strip.text.x = element_text(face="bold", size = 35)) 


MOD.6.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "6") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_manual(values = "#0040FF")+
  ggtitle("Module 6") +
  theme(plot.title = element_text(hjust = 0.5))

MOD.7.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "7") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_manual(values = "#8000FF") +
  ggtitle("Module 7") +
  theme(plot.title = element_text(hjust = 0.5))

MOD.8.PLOT <- 
  cluster.labels %>%
  group_by(module) %>%
  slice_max(n.country.doc, n=20) %>%
  mutate(module=as.factor(module)) %>%
  filter(module == "8") %>%
  ggplot(., aes( label = labels, size = n.country.doc, color = module)) +
  geom_text_wordcloud_area(area_corr = TRUE, eccentricity = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_manual(values = "#FF00BF") +
  ggtitle("Module 8") +
  theme(plot.title = element_text(hjust = 0.5))

((MOD.1.PLOT + MOD.2.PLOT )+ (MOD.3.PLOT + MOD.4.PLOT ))/ 
((MOD.5.PLOT + MOD.6.PLOT) + (MOD.7.PLOT + MOD.8.PLOT))


ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/sharedclusters.png", 
       width = 75, height = 35, units = "cm",
       limitsize = FALSE)

adj.network.df1 %>%
  select(-labels,-to) %>%
  distinct() %>%
  mutate(country = case_when(
    str_detect(name,"sfs-") ~ "SE",
    TRUE ~ "DK")) %>%
  mutate(module = as.factor(module)) %>%
  group_by(module) %>%
  summarise(n.country = n()) 
  

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

# --- Fisheries network --- #

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
#    1    1    1    3    9   47  

l.f <- layout.fruchterman.reingold(network.f)
sort(degree)

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

table(V(network.f)$source == "EU") #397
table(V(network.f)$source == "DK") #155
table(V(network.f)$source == "SE") #51
(V(network.f)) # 603 vertices

ifelse(igraph::degree(network.f) >=9 & V(network.f)$source == "EU",
       V(network.f)$name,NA)

# fisheries network stats ----------------------------------



EUlinks.fisheriesmatrix <- 
  EUlinks.fisheries %>%
  mutate(links=1) %>%
  pivot_wider(names_from = from, values_from = links ,values_fill = 0) %>%
  column_to_rownames(var = "to") #higher trophic level (EU docs) is the columns

EUfisheries.modules <- computeModules(EUlinks.fisheriesmatrix) #computed Nov 22nd, 2022 
saveRDS(EUfisheries.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUfisheries.modules.RDS") 
EUfisheries.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUfisheries.modules.RDS")
plotModuleWeb(EUfisheries.modules)
printoutModuleInformation(EUfisheries.modules) # total modules

indices <- c( "degree","PDI","nestedrank")

EUfisheries.CELEX.networkstats <- 
  specieslevel(EUlinks.fisheriesmatrix, index=  indices, level = "lower",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(EUfisheries.CELEX.networkstats)
#degree         nestedrank        PDI        
#Min.   : 1.000   Min.   :0.00   Min.   :0.7756  
#1st Qu.: 1.000   1st Qu.:0.25   1st Qu.:0.9951  
#Median : 1.000   Median :0.50   Median :1.0000  
#Mean   : 2.196   Mean   :0.50   Mean   :0.9942  
#3rd Qu.: 2.000   3rd Qu.:0.75   3rd Qu.:1.0000  
#Max.   :47.000   Max.   :1.00   Max.   :1.0000  



# --- Hunting network --- #

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
#    1    1    1    2    4   60

l.h <- layout.fruchterman.reingold(network.h)
sort(degree)

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

table(V(network.h)$source == "EU") #175
table(V(network.h)$source == "DK") #55
table(V(network.h)$source == "SE") #19
(V(network.h)) # 249 vertices

# hunting network stats ----------------------------------

EUlinks.huntingmatrix <- 
  EUlinks.hunting %>%
  mutate(links=1) %>%
  pivot_wider(names_from = from, values_from = links ,values_fill = 0) %>%
  column_to_rownames(var = "to") #higher trophic level (EU docs) is the columns

EUhunting.modules <- computeModules(EUlinks.huntingmatrix) #computed Nov 22nd, 2022 
saveRDS(EUhunting.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUhunting.modules.RDS") 
EUhunting.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUhunting.modules.RDS")
plotModuleWeb(EUhunting.modules)
printoutModuleInformation(EUhunting.modules) # total modules

indices <- c( "degree","PDI","nestedrank")

EUhunting.CELEX.networkstats <- 
  specieslevel(EUlinks.huntingmatrix, index=  indices, level = "lower",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(EUhunting.CELEX.networkstats)
#Min.   : 1.000   Min.   :0.00   Min.   :0.4932  
#1st Qu.: 1.000   1st Qu.:0.25   1st Qu.:1.0000  
#Median : 1.000   Median :0.50   Median :1.0000  
#Mean   : 1.669   Mean   :0.50   Mean   :0.9908  
#3rd Qu.: 1.000   3rd Qu.:0.75   3rd Qu.:1.0000  
#Max.   :38.000   Max.   :1.00   Max.   :1.0000

# --- maritime traffic network --- #

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
#    1.0  1.0  1.0  2.0 6.05 63.0

l.m <- layout.fruchterman.reingold(network.m)
sort(degree)

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


table(V(network.m)$source == "EU") #346 
table(V(network.m)$source == "DK") #1
table(V(network.m)$source == "SE") #53
(V(network.m)) # 400 vertices

# maritime network stats ----------------------------------
EUlinks.maritimematrix <- 
  EUlinks.maritime %>%
  mutate(links=1) %>%
  pivot_wider(names_from = from, values_from = links ,values_fill = 0) %>%
  column_to_rownames(var = "to") #higher trophic level (EU docs) is the columns

EUmaritime.modules <- computeModules(EUlinks.maritimematrix) #computed Nov 22nd, 2022 
saveRDS(EUmaritime.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUmaritime.modules.RDS") 
EUmaritime.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUmaritime.modules.RDS")
plotModuleWeb(EUmaritime.modules)
printoutModuleInformation(EUmaritime.modules) # total modules

indices <- c( "degree","PDI","nestedrank")

EUmaritime.CELEX.networkstats <- 
  specieslevel(EUlinks.maritimematrix, index=  indices, level = "lower",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(EUmaritime.CELEX.networkstats)
#  Min.   :1.000   Min.   :0.00   Min.   :0.9057  
#st Qu.:1.000   1st Qu.:0.25   1st Qu.:0.9811  
#Median :1.000   Median :0.50   Median :1.0000  
#Mean   :1.454   Mean   :0.50   Mean   :0.9914  
#3rd Qu.:2.000   3rd Qu.:0.75   3rd Qu.:1.0000  
#Max.   :6.000   Max.   :1.00   Max.   :1.0000 


# Combine all network stats:
all.networkstats <- rbind(maritime.network.df,fisheries.network.df,hunting.network.df) %>%
  mutate(country = case_when( str_detect(name,"sfs\\-") == "TRUE"  ~ "se" ,
                              str_detect(name, "\\/eli\\/")== "TRUE"  ~ "dk",
                                         TRUE  ~ "EU" ))
all.networkstats %>% 
  filter(search.term == "fisheries") %>% 
  summary(.)

all.networkstats %>% 
  filter(search.term == "hunting") %>% 
  summary(.)

all.networkstats %>% 
  filter(search.term == "maritime") %>% 
  summary(.)

n_distinct(all.networkstats$name) # 1025

all.networkstats1 <- 
  document.key.df%>%
  select(celex,labels) %>%
  distinct() %>%
  right_join(., all.networkstats, by= c("celex" = "name")) %>%
  rename("name" = "celex")

n_distinct(all.networkstats1$name) # 1025 no losses or additions so passes the check 


# ---plot them all together ---#

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


# Archival code ----------------------------------------------------

# network stats (these are wrong since I did it as an undirected network but it is a directed network. BUT this code could be good to reference in the future if i need to do something similar later :) ----------------------------------

# degree 
network.degree<-degree(network)

summary(network.degree)


# betweenness
network.betweenness<-betweenness(network, directed = TRUE, normalized=TRUE)

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
     vertex.shape="circle",
     vertex.label = NA,
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
### trying ribbon plot ###

# get data in the right formate
EUlinks.terms %>%
  mutate(search.term = case_when( search.term == "jakt"~ "hunting",
                                  search.term == "jagt"~ "hunting",
                                  search.term == "fiske"~ "fisheries",
                                  search.term == "fiskeri"~ "fisheries",
                                  search.term == "sjofart"~ "maritime traffic",
                                  search.term == "sotrafik"~ "maritime traffic")) %>%
  group_by(to, country, search.term) %>%
  summarise(n=n_distinct(from)) %>%
  ggplot(.,
       aes(y = n, axis1 = search.term, axis2 = to)) +
  geom_alluvium(aes(fill = country), width = 1/12) +
  geom_stratum(width = 1/12) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  facet_wrap(~ search.term, scales = "fixed") 


