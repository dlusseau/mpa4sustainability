

# Clear work space ---------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ----------------------------------------------------------

library("dplyr")
library("tidyr")
library("knitr")
library("glmmTMB")

# Load data ---------------------------------------------------------------

# we are only doing this for the second order citations:
Q1C2.term.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q1C2.term.df.csv")
Q2C2.term.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q2C2.term.data.csv")


Q1.Order2.docs <- read.csv("WP4/Policy_Interactions/data/03.Q1secondordercit.verticesmetadata.csv")
Q2.Order2.docs <- read.csv("WP4/Policy_Interactions/data/04.Q2secondordercit.verticesmetadata.csv")

document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

#----------- Query 1 --------------------

#remove unnec. columns for this 
Q1.Order2.docs <-
  Q1.Order2.docs %>%
  select(-color,-shape,-remove)

#the key doc only keep celex and eurovoc
celex.label.key <- 
  document.key.df %>%
  distinct(celex,labels)

#now get the network docs eurovoc descriptors 
Q1.Order2.docsdescript <- 
  Q1.Order2.docs %>%
  left_join(.,celex.label.key, by = c("CELEX" = "celex")) %>%
  distinct(CELEX,labels,pulled.from) %>%
  drop_na(labels) # 3 celexes (all references) were given no EuroVoc descriptors

Q1.eurovoc.clusters <-
  Q1C2.term.df %>%
  distinct(name, component)

Q1.cluster.profiles <- right_join(Q1.eurovoc.clusters,Q1.Order2.docsdescript, by = c("name"="labels"))


Q1.cluster.profiles2 <-
  Q1.cluster.profiles %>%
  group_by(CELEX,component) %>%
  mutate(component.terms.n = n_distinct(name)) %>% # how many eurovocs a celex has within a cluster
  ungroup() %>%
  group_by(CELEX) %>%
  mutate(total.terms = n_distinct(name)) %>% # how many EuroVocs a celex has
  ungroup() %>%
  mutate(cluster.prop = component.terms.n/total.terms) %>%
  #check
 # distinct(CELEX,component,cluster.prop) %>% group_by(CELEX) %>% summarise(sum = sum(cluster.prop)) %>% filter(sum != 1) # none so the all prop == 1
  distinct(CELEX,component,cluster.prop,pulled.from)


Q1.cluster.profiles2 %>%
  group_by(component) %>%
  summarise( n = n_distinct(CELEX))
#how many celexs in each clusters?
#  component     n
#         1   241
#         2     1
#         3   280
#         4   285
#         5    71
#         6   186
#         7    94
#         8    67
#         9     5
#        10     3
#        11    22


Q1.cluster.profiles2 %>%
  group_by(component,pulled.from) %>%
  summarise( n = n_distinct(CELEX)) %>% #
  group_by(component) %>%
  mutate(sum = sum(n),
         prop = n/sum) %>% print(n = 35)
#6 clusters have both a seed and citation
# component pulled.from     n   sum    prop
#         1 both            2   241 0.00830
#         1 eurlex.web      3   241 0.0124 
#         1 reference      15   241 0.0622 
#         1 reference2    177   241 0.734  
#         1 reference3     44   241 0.183  
#         3 both            5   280 0.0179 
#         3 eurlex.web     11   280 0.0393 
#         3 reference      21   280 0.075  
#         3 reference2    174   280 0.621  
#         3 reference3     69   280 0.246  
#         4 eurlex.web      1   285 0.00351
#         4 reference      10   285 0.0351 
#         4 reference2    249   285 0.874  
#         4 reference3     25   285 0.0877 
#         5 both            1    71 0.0141 
#         5 reference2     59    71 0.831  
#         5 reference3     11    71 0.155  
#         6 both            2   186 0.0108 
#         6 reference       4   186 0.0215 
#         6 reference2    154   186 0.828  
#         6 reference3     26   186 0.140  
#         7 eurlex.web      1    94 0.0106 
#         7 reference       3    94 0.0319 
#         7 reference2     82    94 0.872  
#         7 reference3      8    94 0.0851 


# 5 clusters only composed of citations
#         2 reference2      1     1 1      
#         8 reference       2    67 0.0299 
#         8 reference2     60    67 0.896  
#         8 reference3      5    67 0.0746 
#         9 reference2      5     5 1      
#        10 reference2      3     3 1      
#        11 reference2     22    22 1  

Q1.cluster.profiles2 %>%
  group_by(CELEX) %>%
  summarise( n = n_distinct(component)) %>%
  arrange(desc(n))
# how many components a document is within

Q1.cluster.profiles2 %>%
  filter(cluster.prop >=0.5) %>%
  arrange(desc(cluster.prop)) %>% dim() # 658 have a cluster > 50%
  
xxx <-
  Q1.cluster.profiles2 %>%
  filter(cluster.prop >=0.5) %>%
  arrange(desc(cluster.prop))

less.than.fifty <-
  anti_join(Q1.cluster.profiles2,xxx, by = c("CELEX"))
# 122 docs done have a 50% majority cluster 
# 122/675 only 18% of the total network


Q1.cluster.profiles2 %>%
  filter(cluster.prop ==1) %>%
  dim() # 273

675-122-273
#280 docs are between .5-.99


### GLMM -----------------------  

# need the citation netwrok stats DF: 
#Q1C2.netstats <- read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q1C2.networkdata.csv")

#Q1C2.netstats <-
#  Q1C2.netstats %>%
#  rename("cit.component" = "component")

#Q1.clus.df <-
 # Q1.cluster.profiles2 %>%
 # rename("cluster" = "component") %>%
 # left_join(., Q1C2.netstats, by = c("CELEX" = "name"))

#write.csv(Q1.clus.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/11.Q1C2clus.df.csv", row.names=FALSE)


Q1.clus.df <- read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/11.Q1C2clus.df.csv")

# what the df looks like for ref: 
head(Q1.clus.df)


# betweenness association to cluster prevalence -------- 

Q1.clus.df$cluster.f<-factor(Q1.clus.df$cluster)

Q1.clus.df %>%
  group_by(cluster.f) %>%
  count(n_distinct(CELEX))

glm0<-glmmTMB(betweenness~cluster.f,data=Q1.clus.df,family="tweedie")
#1: In fitTMB(TMBStruc) :
#  Model convergence problem; extreme or very small eigenvalues detected. See vignette('troubleshooting')
#2: In fitTMB(TMBStruc) :
#  Model convergence problem; singular convergence (7). See vignette('troubleshooting')

glm0<-glmmTMB(betweenness~cluster.f,data=subset(Q1.clus.df,cluster.f!="2"&cluster.f!="9"&cluster.f!="10"),family="tweedie") #the disconnected clusters

# check: 
library(DHARMa)
res0<-simulateResiduals(glm0)
plot(res0)


library(ggeffects)
library(forcats)

pred0<-ggpredict(glm0,terms=c("cluster.f"))
plot(pred0)

mod.res.df <- as.data.frame(pred0)

head(mod.res.df)

mod.res.df %>%
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
  xlab("predicted betweenness") +
  theme(legend.position = "none")

# degree.out to cluster prevalence -------- 


glmd.tweedie<-glmmTMB(degree.out~cluster.f,data=subset(Q1.clus.df,cluster.f!="2"&cluster.f!="9"&cluster.f!="10"),family="tweedie") 
glmd<-glmmTMB(degree.out~cluster.f,data=subset(Q1.clus.df,cluster.f!="2"&cluster.f!="9"&cluster.f!="10"),family="nbinom2") 
glmdb<-glmmTMB(degree.out~cluster.f,ziformula=~1,data=subset(Q1.clus.df,cluster.f!="2"&cluster.f!="9"&cluster.f!="10"),family="nbinom2")

resd.tweedie<-simulateResiduals(glmd)
plot(resd.tweedie)
resd.glmdb<-simulateResiduals(glmd)
plot(resd.glmdb)
resd.glmdb<-simulateResiduals(glmdb)
plot(resd.glmdb)
#AIC(glmdb,glmd)
#df      AIC
#glmdb 13 5804.548 --> lower AIC
#glmd  12 5838.178
resd<-simulateResiduals(glmd)
plot(resd)

predd<-ggpredict(glmdb,terms=c("cluster.f"))
plot(predd)

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
  xlab("predicted betweenness") +
  theme(legend.position = "none")
#cluster 5 has higher degree out
#----------- Query 2 --------------------

#remove unnec. columns for this 
Q2.Order2.docs <-
  Q2.Order2.docs %>%
  select(-color,-shape,-remove)

#the key doc only keep celex and eurovoc
celex.label.key <- 
  document.key.df %>%
  distinct(celex,labels)

#now get the network docs eurovoc descriptors 
Q2.Order2.docsdescript <- 
  Q2.Order2.docs %>%
  left_join(.,celex.label.key, by = c("CELEX" = "celex")) %>%
  distinct(CELEX,labels,pulled.from) %>%
  drop_na(labels) # 9 celexes (all references) were given no EuroVoc descriptors

Q2.eurovoc.clusters <-
  Q2C2.term.df %>%
  distinct(name, component)

Q2.cluster.profiles <- right_join(Q2.eurovoc.clusters,Q2.Order2.docsdescript, by = c("name"="labels"))

Q2.cluster.profiles2 <-
  Q2.cluster.profiles %>%
  group_by(CELEX,component) %>%
  mutate(component.terms.n = n_distinct(name)) %>% # how many eurovocs a celex has within a cluster
  ungroup() %>%
  group_by(CELEX) %>%
  mutate(total.terms = n_distinct(name)) %>% # how many EuroVocs a celex has
  ungroup() %>%
  mutate(cluster.prop = component.terms.n/total.terms) %>%
  #check
  #distinct(CELEX,component,cluster.prop) %>% group_by(CELEX) %>% summarise(sum = sum(cluster.prop)) %>% filter(sum != 1) # none so the all prop == 1
  distinct(CELEX,component,cluster.prop,pulled.from)


Q2.cluster.profiles2 %>%
  group_by(component) %>%
  summarise( n = n_distinct(CELEX))
#how many celexs in each clusters?
#  component     n
#         1   522
#         2   505
#         3     1
#         4    11
#         5    13
#         6    11
#         7   443
#         8   220
#         9   152
#        10   164
#        11    52
#        12     3
#        13     3
#        14    14


Q2.cluster.profiles2 %>%
  group_by(component,pulled.from) %>%
  summarise( n = n_distinct(CELEX)) %>% #
  group_by(component) %>%
  mutate(sum = sum(n),
         prop = n/sum) %>% print(n = 46)
# 8 clusters have both a seed and citation
# component pulled.from     n   sum    prop
#         1 both           14   522 0.0268 
#         1 eurlex.web     35   522 0.0670 
#         1 reference      34   522 0.0651 
#         1 reference2    345   522 0.661  
#         1 reference3     94   522 0.180  
#         2 both           20   505 0.0396 
#         2 eurlex.web     45   505 0.0891 
#         2 reference      48   505 0.0950 
#         2 reference2    283   505 0.560  
#         2 reference3    109   505 0.216  
#         3 both            1     1 1      
#         7 both            2   443 0.00451
#         7 eurlex.web     11   443 0.0248 
#         7 reference      31   443 0.0700 
#         7 reference2    337   443 0.761  
#         7 reference3     62   443 0.140  
#         8 both            2   220 0.00909
#         8 eurlex.web      2   220 0.00909
#         8 reference      13   220 0.0591 
#         8 reference2    174   220 0.791  
#         8 reference3     29   220 0.132  
#         9 both            1   152 0.00658
#         9 eurlex.web      3   152 0.0197 
#         9 reference      10   152 0.0658 
#         9 reference2    123   152 0.809  
#         9 reference3     15   152 0.0987 
#        10 both            1   164 0.00610
#        10 eurlex.web      3   164 0.0183 
#        10 reference      13   164 0.0793 
#        10 reference2    131   164 0.799  
#        10 reference3     16   164 0.0976 
#        14 eurlex.web      1    14 0.0714 
#        14 reference2     13    14 0.929 

# 6 clusters with only citations:
#         4 reference       1    11 0.0909 
#         4 reference2      8    11 0.727  
#         4 reference3      2    11 0.182 
#         5 reference       1    13 0.0769 
#         5 reference2      6    13 0.462  
#         5 reference3      6    13 0.462  
#         6 reference2     11    11 1      
#        11 reference       2    52 0.0385 
#        11 reference2     45    52 0.865  
#        11 reference3      5    52 0.0962 
#        12 reference2      3     3 1      
#        13 reference2      2     3 0.667  
#        13 reference3      1     3 0.333 

Q2.cluster.profiles2 %>%
  group_by(CELEX) %>%
  summarise( n = n_distinct(component)) %>%
  arrange(desc(n))
# how many components a document is within

Q2.cluster.profiles2 %>%
  filter(cluster.prop >=0.5) %>%
  arrange(desc(cluster.prop)) %>% dim() # 1009 have a cluster > 50%

xxx <-
  Q2.cluster.profiles2 %>%
  filter(cluster.prop >=0.5) %>%
  arrange(desc(cluster.prop))

less.than.fifty <-
  anti_join(Q2.cluster.profiles2,xxx, by = c("CELEX"))
# 257 docs done have a 50% majority cluster 
# 257/1049 around 24.49% of the total network

Q2.cluster.profiles2 %>%
  filter(cluster.prop ==1) %>%
  dim() # 327 

1049-257-327
# 465 have from .5-.99

# Design title search clusters ------------------

# EU mpa directives search: 
EU.mpa.termsearch<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_EUmpa.searchterm.CELEX2.csv")

n_distinct(EU.mpa.termsearch$CELEX)
#100
# lets join the new mpa search documents to their associated document data
EU.mpa.termsearch.data <-
  EU.mpa.termsearch %>%
  filter(CELEX != "32006R1967R(01)") %>% #removing this celex bc it is a Corrigendum to a regulation that was already pulled and it is tech. not within the legal act types
  select(-work,-type) %>% # we dont need this info anymore
  distinct(CELEX,search.term)

n_distinct(EU.mpa.termsearch.data$CELEX)
#99

x <- EU.mpa.termsearch.data %>% 
  right_join(.,Q2.cluster.profiles2, by = c("CELEX")) %>%
# increased slightly dim since a doc can come up for multiple search terms
  filter(pulled.from == "both" | pulled.from == "eurlex.web")

n_distinct(x$CELEX)
#76 --> correct there are only 76 doc citing another leg 

# lets combine the titles that overlap:
unique(x$search.term)

xz <- 
  x %>%
  mutate(new.title = case_when(search.term ==  "sites of community importance*?" ~ "habitats directive*?" ,
                               search.term ==  "site of community importance*?" ~ "habitats directive*?",
                               search.term ==  "special areas of conservation*?" ~ "habitats directive*?",
                               search.term ==  "special protection area*?" ~ "birds directive*?",
                               search.term ==  "marine protected area*?" ~ "ospar*?",
                               TRUE ~ search.term ))
unique(xz$new.title)

#"barcelona convention*?"
#
#"sites of community importance*?"
#"site of community importance*?" 
#"habitats directive*?"           
#"special areas of conservation*?"
#
#"birds directive*?"              
#"special protection area*?"
#
#"ospar*?"                        
#"marine protected area*?" 
#
#"world heritage site*?"          
#
# "helcom*?"                       
#
# "specially protected area*?" 

xz %>% distinct(new.title,component,CELEX)

y <- 
  xz %>%
  group_by(CELEX,new.title) %>%
  filter(cluster.prop == max(cluster.prop)) %>%
  distinct(new.title,component,CELEX) %>%
  group_by(new.title) %>%
  mutate(n = n_distinct(CELEX)) %>%
  distinct(CELEX,new.title,n,component) %>%
  ungroup() %>%
  group_by(new.title,component) %>%
  mutate(comp.n=n_distinct(CELEX)) %>%
  mutate(prop= comp.n/n) %>%
  distinct(new.title,n,component,comp.n,n,prop)

y%>%
  filter(new.title =="barcelona convention*?") %>%
  ungroup() %>%
  group_by(new.title)%>%
  mutate(sum = sum(prop))

dup <- 
  xz %>%
  group_by(CELEX,new.title) %>%
  filter(cluster.prop == max(cluster.prop)) %>%
  distinct(new.title,component,CELEX)%>%
  filter(new.title =="barcelona convention*?") %>%
  ungroup()

which(duplicated(dup$CELEX))

y%>%
  filter(new.title =="habitats directive*?") %>%
  ungroup() %>%
  group_by(new.title)%>%
  mutate(sum = sum(prop))


dup <- 
  xz %>%
  group_by(CELEX,new.title) %>%
  filter(cluster.prop == max(cluster.prop)) %>%
  distinct(new.title,component,CELEX)%>%
  filter(new.title =="habitats directive*?") %>%
  ungroup()

which(duplicated(dup$CELEX))

y%>%
  filter(new.title =="birds directive*?")  %>%
  ungroup() %>%
  group_by(new.title)%>%
  mutate(sum = sum(prop))

y%>%
  filter(new.title =="ospar*?")  %>%
  ungroup() %>%
  group_by(new.title)%>%
  mutate(sum = sum(prop))

dup <- 
  xz %>%
  group_by(CELEX,new.title) %>%
  filter(cluster.prop == max(cluster.prop)) %>%
  distinct(new.title,component,CELEX)%>%
  filter(new.title =="ospar*?") %>%
  ungroup()

which(duplicated(dup$CELEX))


y%>%
  filter(new.title =="world heritage site*?")  %>%
  ungroup() %>%
  group_by(new.title)%>%
  mutate(sum = sum(prop))

y%>%
  filter(new.title =="helcom*?")  %>%
  ungroup() %>%
  group_by(new.title)%>%
  mutate(sum = sum(prop))

y%>%
  filter(new.title =="specially protected area*?")  %>%
  ungroup() %>%
  group_by(new.title)%>%
  mutate(sum = sum(prop))


y%>%
  select(new.title,component,prop) %>%
  arrange(desc(new.title)) %>%
  mutate(prop = round(prop, digits = 2)) %>%
  kable(.,"latex")

  
### GLMM -----------------------  

# need the citation netwrok stats DF: 
#Q2C2.netstats <- read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q2C2.networkdata.csv")

#Q2C2.netstats <-
#  Q2C2.netstats %>%
 # rename("cit.component" = "component")

#Q2.clus.df <-
#  Q2.cluster.profiles2 %>%
#  rename("cluster" = "component") %>%
#  left_join(., Q2C2.netstats, by = c("CELEX" = "name"))

# write.csv(Q2.clus.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/11.Q2C2clus.df.csv", row.names=FALSE)
Q2.clus.df <- read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/11.Q2C2clus.df.csv")

# what the df looks like for ref: 
head(Q2.clus.df)



# betweenness association to cluster prevalence -------- 

Q2.clus.df$cluster.f<-factor(Q2.clus.df$cluster)

glm0<-glmmTMB(betweenness~cluster.f,data=Q2.clus.df,family="tweedie")
#1: In fitTMB(TMBStruc) :
#  Model convergence problem; extreme or very small eigenvalues detected. See vignette('troubleshooting')
#2: In fitTMB(TMBStruc) :
#  Model convergence problem; singular convergence (7). See vignette('troubleshooting')

# ok seems like it is due to the issue that there a a few clusters with very very low sample sizes: 
Q2.clus.df %>%
  group_by(cluster.f) %>%
  count(n_distinct(CELEX))

#    cluster.f `n_distinct(CELEX)`     n
#    1                        1040   522
#    2                        1040   505
#    3                        1040     1 --Very Small
#    4                        1040    11
#    5                        1040    13 
#    6                        1040    11
#    7                        1040   443
#    8                        1040   220
#    9                        1040   152
#   10                       1040   164
#   11                       1040    52
#   12                       1040     3 -- Very Small
#   13                       1040     3 -- Very Small
#   14                       1040    14

#sub set the very small clusters out
glm0<-glmmTMB(betweenness~cluster.f,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="tweedie") #the disconnected clusters

# check: 
library(DHARMa)
res0<-simulateResiduals(glm0)
plot(res0)

#cluster 5 has higher betweenness on average, offset does very little

library(ggeffects)
library(forcats)

pred0<-ggpredict(glm0,terms=c("cluster.f"))
plot(pred0)

mod.res.df <- as.data.frame(pred0)

head(mod.res.df)

mod.res.df %>%
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
                           x == "14" ~ "14")),
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
                     "14"))%>%
  ggplot(., aes(x=predicted, y=fct_inorder(name), color=x)) +
  geom_point() +
  geom_errorbar(aes(y=name, xmin=conf.low, xmax=conf.high), width=.1) +
  theme_minimal() +
  ylab("") +
  xlab("predicted betweenness") +
  theme(legend.position = "none")

# degree.out to cluster prevalence -------- 


glmd<-glmmTMB(degree.out~cluster.f,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="nbinom2") 
glmdb<-glmmTMB(degree.out~cluster.f,ziformula=~1,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="nbinom2")
AIC(glmdb,glmd)
#df      AIC
#glmdb 13 5804.548 --> lower AIC
#glmd  12 5838.178
resd<-simulateResiduals(glmdb)
plot(resd)

predd<-ggpredict(glmdb,terms=c("cluster.f"))
plot(predd)
#cluster 5 has higher degree out

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
                                  x == "14" ~ "14")),
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
                     "14"))%>%
  ggplot(., aes(x=predicted, y=fct_inorder(name), color=x)) +
  geom_point() +
  geom_errorbar(aes(y=name, xmin=conf.low, xmax=conf.high), width=.1) +
  theme_minimal() +
  ylab("") +
  xlab("predicted degrees out") +
  theme(legend.position = "none")
# degree.in to cluster prevalence -------- 

# need to fix later

glmd<-glmmTMB(degree.out~cluster.f,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="nbinom2") 
glmdb<-glmmTMB(degree.out~cluster.f,ziformula=~1,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="nbinom2")
AIC(glmdb,glmd)
#df      AIC
#glmdb 13 5804.548 --> lower AIC
#glmd  12 5838.178
resd<-simulateResiduals(glmdb)
plot(resd)

# degree ratio to cluster prevalence -------- 

Q2.clus.df$deg.ratio<-Q2.clus.df$degree.out/Q2.clus.df$degree.in
Q2.clus.df$deg.ratio[Q2.clus.df$deg.ratio==Inf]<-0

glmdoi<-glmmTMB(deg.ratio~cluster.f,data=subset(Q2.clus.df,cluster.f!="3"&cluster.f!="12"&cluster.f!="13"),family="tweedie") #the disconnected clusters
resdoi<-simulateResiduals(glmdoi)
plot(resdoi)

preddoi<-ggpredict(glmdoi,terms=c("cluster.f"))
plot(preddoi)

as.data.frame(preddoi)%>%
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
                                  x == "14" ~ "14")),
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
                     "14"))%>%
  ggplot(., aes(x=predicted, y=fct_inorder(name), color=x)) +
  geom_point() +
  geom_errorbar(aes(y=name, xmin=conf.low, xmax=conf.high), width=.1) +
  theme_minimal() +
  ylab("") +
  xlab("predicted degrees ratio (out/in)") +
  theme(legend.position = "none")

