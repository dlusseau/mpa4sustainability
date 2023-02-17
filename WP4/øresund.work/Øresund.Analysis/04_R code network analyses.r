library(igraph)
library(bipartite)

# DK - SE --------------------------------
DKSE<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/network.SEDK.igraph.rds")

dkse.edge<-as.data.frame(get.edgelist(DKSE))
dkse.edge[,3]<-E(DKSE)$n
names(dkse.edge)[3]<-"weights"
names(dkse.edge)[1]<-"Sweden"
names(dkse.edge)[2]<-"Denmark"


#let's make it by hand
dkse.bi<-matrix(0,length(unique(dkse.edge$Sweden)),length(unique(dkse.edge$Denmark)))
rownames(dkse.bi)<-unique(dkse.edge$Sweden)
colnames(dkse.bi)<-unique(dkse.edge$Denmark)

for (i in 1:length(unique(dkse.edge$Sweden))) {
temp<-dkse.edge[dkse.edge$Sweden==unique(dkse.edge$Sweden)[i],]
dkse.bi[i,match(temp$Denmark,colnames(dkse.bi))]<-temp$weights
}

#for betweenness

dkse.edge.bet<-dkse.edge
dkse.edge.bet[,1]<-factor(dkse.edge.bet[,1])
dkse.edge.bet[,2]<-factor(dkse.edge.bet[,2])
dkse.edge.bet[,2]<-dim(dkse.bi)[1]+as.numeric(dkse.edge.bet[,2]) #needs to start edge number at 53
dkse.edge.bet[,1]<-as.numeric(dkse.edge.bet[,1])
names(dkse.edge.bet)[1:2]<-c("from","to")

#undirected, we need to mention edges twice
dkse.edge.bet.flip<-dkse.edge.bet[,c(2,1,3)] #flip from to
names(dkse.edge.bet.flip)[1:2]<-c("from","to")

dkse.edge.bet<-rbind(dkse.edge.bet,dkse.edge.bet.flip)

betweenness_w(dkse.edge.bet, directed=FALSE, alpha=1)

SE.bet<-as.data.frame(betweenness_w(dkse.edge.bet, directed=FALSE, alpha=1)[1:dim(dkse.bi)[1],]) #betweenness for Sweden
DK.bet<-as.data.frame(betweenness_w(dkse.edge.bet, directed=FALSE, alpha=1)[(dim(dkse.bi)[1]+1):(dim(dkse.bi)[1]+dim(dkse.bi)[2]),]) #betweenness for DK
SE.bet$code<-rownames(dkse.bi)
DK.bet$code<-colnames(dkse.bi)


SE.bet[sort(SE.bet$betweenness,index=TRUE,decreasing=TRUE)$ix,]
#   top SE
#node    #betweenness 
#   13 8468.7741278  sfs-1998-808
#   21 4991.0333333  sfs-2006-813
#   22 3492.2741278  sfs-2007-845

SE.bwtn.df <- 
  SE.bet[sort(SE.bet$betweenness,index=TRUE,decreasing=TRUE)$ix,]%>%
  select(-node) %>%
  as.data.frame() %>%
  mutate(betweenness = round(betweenness, digits =3))

igraph::degree(DKSE) %>%
  as.data.frame() %>%
  rename("degree" = ".") %>%
  rownames_to_column(., var = "code") %>%
  filter(str_detect(code, 'sfs')) %>%
  left_join(., SE.bwtn.df, by = c("code")) %>%
  arrange(., desc(betweenness))%>%
  kable(., "latex")

igraph::degree(DKSE) %>%
  as.data.frame() %>%
  rename("degree" = ".") %>%
  rownames_to_column(., var = "code") %>%
  filter(str_detect(code, 'sfs')) %>%
  arrange(., desc(degree))
# sfs-1998-808     89
#   sfs-2007-845     85
#  sfs-1998-1252     82
  
DK.bet[sort(DK.bet$betweenness,index=TRUE,decreasing=TRUE)$ix,]
#top DK betweenness 
#  184 5098.1666667       /eli/lta/2021/1976
#   105 2329.6298701        /eli/lta/2016/861
#  163 2072.5976572       /eli/lta/2021/2512


DK.bwtn.df <- 
  DK.bet[sort(DK.bet$betweenness,index=TRUE,decreasing=TRUE)$ix,]%>%
  select(-node) %>%
  as.data.frame() %>%
  mutate(betweenness = round(betweenness, digits =3)) 

igraph::degree(DKSE) %>%
  as.data.frame() %>%
  rename("degree" = ".") %>%
  rownames_to_column(., var = "code") %>%
  filter(str_detect(code, 'eli')) %>%
  left_join(., DK.bwtn.df, by = c("code")) %>%
  arrange(., desc(betweenness))%>%
  kable(., "latex")

igraph::degree(DKSE) %>%
  as.data.frame() %>%
  rename("degree" = ".") %>%
  rownames_to_column(., var = "code") %>%
  filter(str_detect(code, 'eli')) %>%
  arrange(., desc(degree))
#           /eli/lta/2022/100      9
#2          /eli/lta/2022/988      8
#3          /eli/lta/2022/787      8



#################################################################
## bipartite network analyses
library(igraph)
library(bipartite)

############################################
#### DK - EU first --------------
#  changing the order to make sure it is an information flow network 
DKEU<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/dkeu.networkigraph.rds")

dkeu.edge<-as.data.frame(get.edgelist(DKEU))

#information flow network
eudk.edge<-dkeu.edge[,c(2,1)]
names(eudk.edge)[1]<-"EU"
names(eudk.edge)[2]<-"Denmark"


#let's make it by hand
eudk.bi<-matrix(0,length(unique(eudk.edge$EU)),length(unique(eudk.edge$Denmark)))
rownames(eudk.bi)<-unique(eudk.edge$EU)
colnames(eudk.bi)<-unique(eudk.edge$Denmark)

for (i in 1:length(unique(eudk.edge$EU))) {
temp<-eudk.edge[eudk.edge$EU==unique(eudk.edge$EU)[i],]
eudk.bi[i,match(temp$Denmark,colnames(eudk.bi))]<-1
}


eudk.stat<-specieslevel(eudk.bi,nested.weighted=FALSE)

#243 EU texts, 211 DK texts

#EU
EUstats<-as.data.frame(eudk.stat[[2]])
EUstats$name<-rownames(eudk.bi)

#degree
#top 5
EUstats$name[sort(EUstats$degree,decreasing=TRUE,index=TRUE)$ix][1:3]
#"31992L0043" "32009L0147" "32004L0035" "32006L0123" "32011L0092" 

EUstats$degree[sort(EUstats$degree,decreasing=TRUE,index=TRUE)$ix][1:3]
#corresponding degrees: 82 70 16 15 13 

#bottom # does not make much sense as many are 1

#betweenness
EUstats$name[sort(EUstats$betweenness,decreasing=TRUE,index=TRUE)$ix][1:3]
#"32006L0123" "32013R1379" "32009L0016"
EUstats$betweenness[sort(EUstats$betweenness,decreasing=TRUE,index=TRUE)$ix][1:3]
#corresponding value 0.2013526 0.1251019 0.1138049
#######
#DK
DKstats<-as.data.frame(eudk.stat[[1]])
DKstats$name<-colnames(eudk.bi)

#degree
#top 5
DKstats$name[sort(DKstats$degree,decreasing=TRUE,index=TRUE)$ix][1:4]

# "/eli/lta/2022/221"  "/eli/lta/2022/100"  "/eli/lta/2021/2246" "/eli/lta/2022/538"  "/eli/lta/2019/1165"
DKstats$degree[sort(DKstats$degree,decreasing=TRUE,index=TRUE)$ix][1:4]

#corresponding values 21 21 19 15 11 

#betweenness
#top 3
DKstats$name[sort(DKstats$betweenness,decreasing=TRUE,index=TRUE)$ix][1:3]

#  "/eli/lta/2021/2247"       "/eli/retsinfo/2015/11234"
# [3] "/eli/lta/2019/783"
DKstats$betweenness[sort(DKstats$betweenness,decreasing=TRUE,index=TRUE)$ix][1:3]

#corresponding values 0.1986549 0.1727320 0.1169209

head(EUstats)

EUstats %>%
  select(name,degree,betweenness) %>%
  rownames_to_column(., var = "delete") %>%
  select(-delete) %>%
  arrange(desc(betweenness))%>%
  mutate(betweenness = round(betweenness, digits = 3)) %>%
  kable(., "latex")
  
  DKstats %>%
    select(name,degree,betweenness) %>%
    rownames_to_column(., var = "delete") %>%
    select(-delete) %>%
    arrange(desc(betweenness))%>%
    mutate(betweenness = round(betweenness, digits = 3)) %>%
  kable(., "latex")

############################################
#### SE - EU  --------------------------------
# chaning the order to make sure it is an information flow network

SEEU<-readRDS("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/seeu.networkigraph.rds")

SEeu.edge<-as.data.frame(get.edgelist(SEEU))

#information flow network
euSE.edge<-SEeu.edge[,c(2,1)]
names(euSE.edge)[1]<-"EU"
names(euSE.edge)[2]<-"Sweden"


#let's make it by hand
euSE.bi<-matrix(0,length(unique(euSE.edge$EU)),length(unique(euSE.edge$Sweden)))
rownames(euSE.bi)<-unique(euSE.edge$EU)
colnames(euSE.bi)<-unique(euSE.edge$Sweden)

for (i in 1:length(unique(euSE.edge$EU))) {
temp<-euSE.edge[euSE.edge$EU==unique(euSE.edge$EU)[i],]
euSE.bi[i,match(temp$Sweden,colnames(euSE.bi))]<-1
}


euSE.stat<-specieslevel(euSE.bi,nested.weighted=FALSE)

#301 EU texts, 92 SE texts

#EU
EUstats<-as.data.frame(euSE.stat[[2]])
EUstats$name<-rownames(euSE.bi)

#degree
#top 6
EUstats$name[sort(EUstats$degree,decreasing=TRUE,index=TRUE)$ix][1:7]
#"32016R0679" "31976L0769" "31991L0155" "31993L0067" "31993L0105" "32000L0021" "32019R1020"
EUstats$degree[sort(EUstats$degree,decreasing=TRUE,index=TRUE)$ix][1:7]
#corresponding degrees: 10  8  8  8  8  8  6

#bottom # does not make much sense as many are 1

#betweenness
EUstats$name[sort(EUstats$betweenness,decreasing=TRUE,index=TRUE)$ix][1:3]
#""32002R0178" "32013R1380" "32017R0745"

EUstats$betweenness[sort(EUstats$betweenness,decreasing=TRUE,index=TRUE)$ix][1:3]
#corresponding value 0.08622844 0.07841266 0.07562246
#######
#SE
SEstats<-as.data.frame(euSE.stat[[1]])
SEstats$name<-colnames(euSE.bi)

#degree
#top 5
SEstats$name[sort(SEstats$degree,decreasing=TRUE,index=TRUE)$ix][1:3]
#[1]  "sfs-1998-808"  "sfs-2011-13"   "sfs-2022-1718"

SEstats$degree[sort(SEstats$degree,decreasing=TRUE,index=TRUE)$ix][1:3]

#corresponding values  30 29 27

#betweenness
#top 5
SEstats$name[sort(SEstats$betweenness,decreasing=TRUE,index=TRUE)$ix][1:3]

# "sfs-2009-400"  "sfs-1999-1229" "sfs-1994-200"
SEstats$betweenness[sort(SEstats$betweenness,decreasing=TRUE,index=TRUE)$ix][1:3]
#corresponding values  0.13578202 0.09790169 0.07481312


# latex tables ------------
EUstats %>%
  select(name,degree,betweenness) %>%
  rownames_to_column(., var = "delete") %>%
  select(-delete) %>%
  arrange(desc(betweenness))%>%
  mutate(betweenness = round(betweenness, digits = 3)) %>%
kable(., "latex")

SEstats %>%
  select(name,degree,betweenness) %>%
  rownames_to_column(., var = "delete") %>%
  select(-delete) %>%
  arrange(desc(betweenness))%>%
  mutate(betweenness = round(betweenness, digits = 3)) %>%
kable(., "latex")


