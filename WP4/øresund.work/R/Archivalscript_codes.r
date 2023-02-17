
# Archival code

# 01_loadDKdata.R Archival -------------------

# semi working loop below -----

search.term <- deframe(retsinformation.df[,1])
search.term <- search.term[1:10]

DK.text.list <- structure(vector("list", 10), names=search.term)

# Lets try to get this data from the url...
URLs <- deframe(retsinformation.df[,30])
URLs

URLs <- URLs[1:10]

for (i in seq(URLs)) {
  
  remDr <- rsDriver(browser='chrome', 
                    port= free_port(random = TRUE),
                    check = FALSE, 
                    chromever="105.0.5195.19")
  
  browser <- remDr$client
  
  browser$open() # Open the remote browser
  
  browser$navigate(URLs[i]) # navigate to the URL 
  
  Sys.sleep(2) # Stop for 2 second because takes a couple secs for the pg. to load
  
  pagesource <- browser$getPageSource() # retrieve html page source code
  
  html <- read_html(pagesource[[1]])
  
  DK.text.list[[i]] <-
    html%>%
    html_nodes(xpath = '//*[@class="document-content "]') %>%
    html_text2()
  
  print(i) # Print what iteration we are on
  
  browser$close() # Close the browser
  rm(remDr) # Remove this obj.
  # so we dont have port use issues we need to kill the java instances found on this thread: https://github.com/ropensci/RSelenium/issues/228 
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) 
  
  Sys.sleep(1) 
  
}

# this loop works until there was like 500 and then we had another port in use issue

DK.text.list.1 <- DK.text.list


# lets make it into a df to use.
DK.text.list.1.2 <- as.data.frame(cbind(DK.text.list.1))
#DK.text.list.1.2 <- as.data.frame(unlist(DK.text.list.1))

DK.text.list.2 <- 
  DK.text.list.1.2 %>% 
  rownames_to_column(., var = "search.term") %>%
  rename("text" = "DK.text.list.1") %>%
  mutate(search.term = str_replace_all(search.term,"\\.[:graph:]+",""),
         country = "DK",
         ID = row_number()) %>%
  as_tibble() %>%
  unnest(text)

str(DK.text.list.2)

write.csv(x = DK.text.list.2,
          file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/01_DK.textDF.csv", row.names=FALSE)



# How to get the text for one URL ------

URL <- URLs2[39]
URLs2[410]


binman::list_versions("chromedriver")
# $win32
# [1] "105.0.5195.19" "105.0.5195.52" "106.0.5249.21"

remDr <- rsDriver(browser='chrome', port=9887L,check = FALSE, 
                  chromever="105.0.5195.19")

browser <- remDr$client

browser$open()

browser$navigate("https://www.retsinformation.dk/eli/retsinfo/2000/20071")

pagesource <- browser$getPageSource()

html <- read_html(pagesource[[1]],options = "HUGE")

text <-
  html%>%
  html_nodes(xpath = '//*[@class="document-content "]') %>%
  html_text2()

# "mb-0 py-0 pr-0" --> node for the EU reference

# 01_loadSEdata.r Archival ----------------------------

#old fiske link 
#"https://data.riksdagen.se/dokumentlista/?sok=%22fiske%22&doktyp=SFS&rm=&from=&tom=&ts=&bet=&tempbet=&nr=&org=&iid=&avd=&webbtv=&talare=&exakt=&planering=&facets=&sort=rel&sortorder=desc&rapport=&utformat=json&a=s#soktraff"
#"https://data.riksdagen.se/dokumentlista/?sok=%22jakt%22&doktyp=SFS&rm=&from=&tom=&ts=&bet=&tempbet=&nr=&org=&iid=&avd=&webbtv=&talare=&exakt=&planering=&facets=&sort=rel&sortorder=desc&rapport=&utformat=json&a=s#soktraff"
#"https://data.riksdagen.se/dokumentlista/?sok=%22sj%C3%B6fart%22&doktyp=SFS&rm=&from=&tom=&ts=&bet=&tempbet=&nr=&org=&iid=&avd=&webbtv=&talare=&exakt=&planering=&facets=&sort=rel&sortorder=desc&rapport=&utformat=json&a=s#soktraff"

# 1.5_EurlextitlekeyforSEdata.R -----------------------


regulation.titles <- 
  document.key.df1 %>%
  filter(resource.type == "REG")%>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)


rm(document.key.df1) # remove this bc it takes up a lot of space

reg.test<-array(0) # 


# this was our firrst try .... now archival
for (i in 4937:dim(regulation.titles)[1]) {
  httr::handle_reset("http://publications.europa.eu/")
  reg.test[i]<-elx_fetch_data(regulation.titles$work[i],type="title")
  print(i)
  flush.console()
  gc()
}

i<-4625
reg.test[i]<-elx_fetch_data(regulation.titles$work[i],type="title")
i<-4671
reg.test[i]<-elx_fetch_data(regulation.titles$work[i],type="title")
i<-4736
reg.test[i]<-elx_fetch_data(regulation.titles$work[i],type="title")
i<-4936
reg.test[i]<-elx_fetch_data(regulation.titles$work[i],type="title")

regulation.titles <- 
  regulation.titles %>%
  slice(1:5038)

regulation.titles$titles<-reg.test

write.csv(regulation.titles, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/regulation.titles.1.5038.csv", row.names=FALSE) 

# --- second try --- #

regulation.titles <- 
  document.key.df1 %>%
  filter(resource.type == "REG")%>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)

# Working on this one first
regulation.titles.5039.35041 <- 
  slice(regulation.titles,5039:35041)

rm(document.key.df1) # remove this bc it takes up a lot of space
rm(regulation.titles) # remove this bc it takes up a lot of space

reg.test<-array(0) # 

gc()
for (i in 4434:dim(regulation.titles.5039.35041)[1]) {
  httr::handle_reset("http://publications.europa.eu/")
  reg.test[i]<-elx_fetch_data(regulation.titles.5039.35041$work[i],type="title")
  print(i)
  flush.console()
  gc()
}

i<-579
reg.test[i]<-elx_fetch_data(regulation.titles.5039.35041$work[i],type="title")
i<-1457
reg.test[i]<-elx_fetch_data(regulation.titles.5039.35041$work[i],type="title")
i<-1596
reg.test[i]<-elx_fetch_data(regulation.titles.5039.35041$work[i],type="title")
i<-1756
reg.test[i]<-elx_fetch_data(regulation.titles.5039.35041$work[i],type="title")
i<-2958
reg.test[i]<-elx_fetch_data(regulation.titles.5039.35041$work[i],type="title")
i<-3832
reg.test[i]<-elx_fetch_data(regulation.titles.5039.35041$work[i],type="title")
i<-4099
reg.test[i]<-elx_fetch_data(regulation.titles.5039.35041$work[i],type="title")
i<-4219
reg.test[i]<-elx_fetch_data(regulation.titles.5039.35041$work[i],type="title")
i<-4433
reg.test[i]<-elx_fetch_data(regulation.titles.5039.35041$work[i],type="title")


regulation.titles1.5039.10168 <- 
  regulation.titles.5039.35041 %>%
  slice(1:5130)

regulation.titles1.5039.10168$titles<-reg.test

write.csv(regulation.titles1.5039.10168, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/regulation.titles.5039.10168.csv", row.names=FALSE) 


# --- second try --- #

regulation.titles <- 
  document.key.df1 %>%
  filter(resource.type == "REG")%>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)

# Working on this one first
regulation.titles.10169.15169 <- 
  slice(regulation.titles,10169:15169)

rm(document.key.df1) # remove this bc it takes up a lot of space
rm(regulation.titles) # remove this bc it takes up a lot of space

reg.test<-array(0) # 

gc()
for (i in 352:dim(regulation.titles.10169.15169)[1]) {
  httr::handle_reset("http://publications.europa.eu/")
  reg.test[i]<-elx_fetch_data(regulation.titles.10169.15169$work[i],type="title")
  print(i)
  flush.console()
}

i<-32
reg.test[i]<-elx_fetch_data(regulation.titles.10169.15169$work[i],type="title")
i<-351
reg.test[i]<-elx_fetch_data(regulation.titles.10169.15169$work[i],type="title")

























gc()
test <- 
  directive.titles %>%
  mutate(title = map_chr(work, elx_fetch_data, "title")) %>% 
  as_tibble() 

# map purr function is not working will try a loop:
celex <- as.character(directive.titles$celex)

directive.titles.list <- structure(vector("list", dim(directive.titles)[1]), names=celex)

gc()
for (i in seq(celex)) {
  
  directive.titles.list[[i]] <-
    directive.titles %>%
    elx_fetch_data(url = .$work[i], type = "title")
  
  
  print(celex[i]) # Print the URL we are on
  print(i)       # Print what iteration we are on
  gc()
  flush.console()
  
}
# keep getting: 
# Internal Server Error (HTTP 500).Error in UseMethod("status_code") : 
# no applicable method for 'status_code' applied to an object of class "NULL"



rm(document.key.df1) # remove this bc it takes up a lot of space

directive.titles[2652,]
directive.titles[2480,]


test<-array(0)
gc()

for (i in seq(dim(directive.titles)[1])) {
  
  test[i]<-elx_fetch_data(directive.titles$work[i],type="title")
  
  print(i)
  
  flush.console()
  gc()
  
}


regulation.titles <- 
  document.key.df1 %>%
  filter(resource.type == "REG") %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  select(celex,title) %>%
  mutate(celex = as.factor(celex)) 






# 02_cleantextDKdata Archival -------------------------

# Archival for making it into a stm corpus will probably need to do later 

# lets mak it into the good df format
#DKtext.df <- 
#  DKtext.df.clean1 %>%
#  as.data.frame(.) %>% 
#  select(ID,search.term,country,text) %>%
#  mutate(search.term = as.factor(search.term),
#         doc_id = as.factor(ID),
#         country = as.factor(country)) %>%
#  select(doc_id,text,search.term,country)

# lets make it into a corpus object (tm package)
mar.protected.corpus <- DataframeSource(mar.protected.text.df.clean)
mar.protected.corpus <- SimpleCorpus(mar.protected.corpus, control = list(language = "da"))

mar.protected.corpus <- corpus(mar.protected.corpus) #should convert it to quanteda package formate since it is the only type I could get a successful conversion to stm
meta(mar.protected.corpus)
docvars(mar.protected.corpus)
ndoc(mar.protected.corpus)

mar.protected.dfm <- dfm(tokens(mar.protected.corpus)) # Create a document feature matrix
Q1.textprocessed <- convert(mar.protected.dfm, to="stm") # convert dfm to stm format corpus

docs  <- Q1.textprocessed$documents
vocab <- Q1.textprocessed$vocab
meta  <- Q1.textprocessed$meta

Q1.out <- prepDocuments(docs, vocab, meta)


# lets make it into a corpus object
DK.corpus <- DataframeSource(DKtext.df.clean)
DK.corpus <- SimpleCorpus(DK.corpus, control = list(language = "da"))


# OK so now we have a cleaned corpus: 

# convert corpus to a document-term matrix
# document term matrix: lists word occurances within a document 
dtm <- DocumentTermMatrix(DK.corpus)
inspect(dtm)
#<<DocumentTermMatrix (documents: 20, terms: 3531)>>
#Non-/sparse entries: 9183/61437
#Sparsity           : 87%
#Maximal term length: 66
#Weighting          : term frequency (tf)
#Sample             :

# remove sparse terms.. those that occur in only a few documents
inspect(removeSparseTerms(dtm, 0.40))
# so for this terms that have at least a 40 sparse are removed.
# the larger the value the smaller the sparity 
# so sparity = .99, means terms within 1% of the data are kept.
# if sparity = .3, means terms within 70% of the data are kept.


# for our data the sparity is kinda high: 87% of the cells are zero!
# So we should remove terms that have low frequencies: 
# so those terms that are only in 30% of the data lets remove them: 
inspect(removeSparseTerms(dtm, 0.70))

# I assume since we are dealing with gov't documents there is a very distinct writing style,
# thus we should remove terms that appear in almost every document...
inspect(removeSparseTerms(dtm, 0.90))


# convert corpus to a term-document matrix
# document term matrix: lists word occurances within a document 
tdm <- TermDocumentMatrix(DK.corpus)
inspect(tdm)

# 02_cleantextSEdata.R ---------------------------

# archival

# ok now lets get the EU legislation key and extract titles to link to the reference codes

directive.titles <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EurLexKey.directive.titles.csv")

Q2C2.edge.text <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/05.Q2C2.edge.text.csv")

directive.titles1 <-
  directive.titles %>%
  select(-X) %>% 
  mutate(title2 = str_trunc(titles,75,side = c("right"))) %>%
  mutate(title.code =  str_extract(title2, "[:digit:]+/[:digit:]+/[:alpha:]+"))

test<- 
  EU.links4 %>%
  distinct(ref) %>%
  left_join(.,directive.titles1, by = c("ref"="title.code"))


Q2C2.edge.text1 <-
  Q2C2.edge.text %>%
  mutate(title2 = str_trunc(total.text,100,side = c("right"))) %>%
  select(CELEX,title2) %>%
  mutate(Reg.2 =  str_extract(title2,"\\([:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+(?!/)"))  #(EU or ECC) nr #/#
# mutate(Reg.3 =  str_extract(title2,"\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")) %>% #(EU or ECC) #/# (this one i noticed but not explicitly said in the link above)
# mutate(Reg. =  str_extract(title2, "Regulation\\sNo\\s[:digit:]+")) %>% # Regulation No 17
# mutate(Reg.4 =  str_extract(title2, "\\sNo\\s[:digit:]+/[:digit:]+(?!/)")) # förordning EEG nr 2658/87 

test2 <- 
  test %>%
  left_join(.,Q2C2.edge.text1, by = c("ref"="Reg.2"))

Q2C2.edge.text2 <-
  Q2C2.edge.text %>%
  mutate(title2 = str_trunc(total.text,100,side = c("right"))) %>%
  select(CELEX,title2) %>%
  # mutate(Reg.2 =  str_extract(title2,"\\([:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+(?!/)"))  #(EU or ECC) nr #/#
  mutate(Reg.3 =  str_extract(title2,"\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)"))  #(EU or ECC) #/# (this one i noticed but not explicitly said in the link above)
# mutate(Reg. =  str_extract(title2, "Regulation\\sNo\\s[:digit:]+")) %>% # Regulation No 17
# mutate(Reg.4 =  str_extract(title2, "\\sNo\\s[:digit:]+/[:digit:]+(?!/)")) # förordning EEG nr 2658/87 

test3 <- 
  test2 %>%
  left_join(.,Q2C2.edge.text2, by = c("ref"="Reg.3"))


# remove those that reference nothing
no.title <- 
  test3 %>%
  filter((is.na(title2.x) &
            is.na(title2.y) &
            is.na(title2)))









# EURLEX KEY
document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

document.key.df1 <- 
  document.key.df %>% 
  select(resource.type, work, celex) %>%
  mutate(celex = as.factor(celex)) %>%
  distinct(celex, .keep_all = TRUE) 

decision.titles <- 
  document.key.df1 %>%
  filter(resource.type == "DEC") %>%
  mutate(title = map_chr(work, elx_fetch_data, "title")) %>% 
  select(celex,title) %>%
  mutate(celex = as.factor(celex))

regulation.titles <- 
  document.key.df1 %>%
  filter(resource.type == "REG") %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  select(celex,title) %>%
  mutate(celex = as.factor(celex)) 

directive.titles <- 
  document.key.df1 %>%
  filter(resource.type == "DIR") %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) # remove na values for celex

# map purr function is not working will try a loop:
celex <- as.character(directive.titles$celex)

directive.titles.list <- structure(vector("list", dim(directive.titles)[1]), names=celex)

gc()
for (i in seq(celex)) {
  
  directive.titles.list[[i]] <-
    directive.titles %>%
    elx_fetch_data(url = .$work[i], type = "title")
  
  
  print(celex[i]) # Print the URL we are on
  print(i)       # Print what iteration we are on
  gc()
  
}
# keep getting: 
# Internal Server Error (HTTP 500).Error in UseMethod("status_code") : 
# no applicable method for 'status_code' applied to an object of class "NULL"

gc()
yy <- 
  directive.titles %>%
  slice(1:1000) %>%
  mutate(title = map_chr(work, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  select(celex,title) %>%
  mutate(celex = as.factor(celex)) 

recommendation.titles <- 
  document.key.df1 %>%
  filter(resource.type == "REC") %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  select(celex,title) %>%
  mutate(celex = as.factor(celex)) 




mutate(title2 = str_trunc(title,100,side = c("right"))) %>%
  mutate(decision = str_detect(title2, "Decision")) %>
  %>% 
  mutate(title.dec =  str_extract(title2, "Decision[:blank:]No[:blank:][:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  mutate(title.dec2 =  str_extract(title2, "Decision[:blank:]\\(.+\\)[:blank:][:digit:]+/[:digit:]+")) %>%
  mutate(title.dec3 =  str_extract(title2, "[:digit:]+/[:digit:]+/[:alpha:]+\\:[:blank:]Council[:blank:]Decision")) %>%
  mutate(title.dec4 =  str_extract(title2, "[:digit:]+/[:digit:]+/[:alpha:]+\\:[:blank:]Commission[:blank:]Decision")) %>%
  mutate(title.dec5 =  str_extract(title2, "European Union Offshore Oil and Gas")) %>%
  select(CELEX,title.dec,title.dec2,title.dec3,title.dec4,title.dec5) 

Q1.Citdecision.titles1 <-
  Q1.Citdecision.titles %>%
  pivot_longer(
    cols = title.dec:title.dec5,
    names_to = "str.type",
    values_to = "title",
    values_drop_na = TRUE
  ) %>%
  select(-str.type) %>%
  mutate(resource.type = "DEC")








# ok now we dont care how many times it ref a doc. just that it does link so group by doc.id and remove duplicated ref
EU.links4 %>%
  distinct(doc.id,ref, .keep_all= TRUE)



EU.linksTEST <- 
  SEtext.dk %>%
  get_sentences() %>%
  mutate(Reg.2 =  str_extract_all(text,"\\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)")) %>% #(EU or ECC) nr #/#
  unnest(Reg.2,keep_empty = TRUE) %>%
  mutate(Reg.3 =  str_extract_all(text,"\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")) %>% #(EU or ECC) #/# (this one i noticed but not explicitly said in the link above)
  unnest(Reg.3,keep_empty = TRUE) %>%
  mutate(Reg. =  str_extract_all(text, "förordning\\snr\\s[:digit:]+(?!/)")) %>% # Regulation No 17
  unnest(Reg.,keep_empty = TRUE) %>%
  mutate(Reg.4 =  str_extract_all(text, "förordning\\s[:alpha:]+\\snr\\s[:digit:]+/[:digit:]+(?!/)")) %>% # förordning EEG nr 2658/87 
  unnest(Reg.4,keep_empty = TRUE)



# 03_countrytoEUpolicy.network.r ------------------

#these are not in the final report. we did them for the first draft, but didnt make it in the final and wasnt updated after some changes to the data extraction

# modules 

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


# network stats (these are wrong since I did it as an undirected network but it is a directed network. BUT this code could be good to reference in the future if i need to do something similar later :) 

# degree 
#network.degree<-degree(network)

#summary(network.degree)


# betweenness
#network.betweenness<-betweenness(network, directed = TRUE, normalized=TRUE)

#summary(network.betweenness)

#plot(network.degree ~ network.betweenness)

# term clusters/groups
# cluster_leading_eigen: 

#   "Community structure detecting based on the leading eigenvector of the community matrix" 
#   "This function tries to find densely connected subgraphs in a graph by calculating the leading nonnegative 
#    eigenvector of the modularity matrix of the graph." - CRAN PDF
#E(network)
# 
# network.clusters <-cluster_leading_eigen(network,
#                                          weights = NULL)
# 
# sort(table(network.clusters$membership))
# # cluster 33 is the largest!
# 
# quantile(table(network.clusters$membership),probs = c(0,.25,.5,.75,.95,1))
# 
# plot_dendrogram(network.clusters)
# 
# # make a df of the network plots ---
# DK.network.stats <- data.frame(name= V(network)$name,
#                                degree=network.degree,
#                                betweenness=network.betweenness,
#                                cluster=as.numeric(membership(network.clusters)))
# 
# DK.network.stats.1 <- 
#   DKEUlinks %>%
#   select(EU.link.CELEX,labels) %>%
#   distinct() %>%
#   right_join(., DK.network.stats, by= c("EU.link.CELEX" = "name"))
# 
# cluster.labels <- 
#   DK.network.stats.1 %>%
#   group_by(cluster, labels) %>%
#   mutate(total.labelcluster.degree = sum(degree)/2) %>%
#   distinct(cluster,labels,total.labelcluster.degree) %>%
#   na.omit()
# 
# 
# l2 <- layout.fruchterman.reingold(network)
# 
# plot(network.clusters, network, 
#      vertex.shape="circle",
#      vertex.label = NA,
#      rescale = TRUE,
#      ylim=c(-.8,.85),xlim=c(-.9,.9),
#      vertex.size=3,
#      layout=l2
# )
# 
# 
# # solution to the color issue here:
# # https://statisticsglobe.com/create-distinct-color-palette-in-r
# palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  # Extract color info
# palette3_all <- unlist(mapply(brewer.pal,                     # Create vector with all colors
#                               palette3_info$maxcolors,
#                               rownames(palette3_info)))
# palette3_all   
# 
# set.seed(1)                                             # Set random seed
# palette3 <- sample(palette3_all, 42)                    # Sample colors
# palette3 
# 
# V(network)$color <- palette3[as.numeric(as.factor(membership(network.clusters)))]
# 
# plot(network,
#      vertex.color=V(network)$color,
#      vertex.label=ifelse(degree(network) >7.05 & V(network)$source == "EU",
#                          V(network)$name,NA),
#      vertex.label.cex = .75,
#      vertex.shape = ifelse(V(network)$source == "EU",
#                            "circle","square"),
#      vertex.size= 2,
#      layout = l2)
# 
# legend(
#   "bottomleft",
#   legend=levels(as.factor(membership(network.clusters))) ,
#   col = palette3,
#   pch    = 20,
#   cex    = 1,
#   bty    = "n",
#   title  = "",
#   horiz = FALSE
# )
# 
# plot(network,
#      vertex.label=ifelse(degree(network) >7.05 & V(network)$source == "EU",
#                          V(network)$name,NA),
#      vertex.label.cex = .75,
#      vertex.size= 2,
#      layout = l)
# 
# # Word clouds of cluster eurovoc discriptors: 
# 
# cluster.labels %>%
#   filter(cluster==33 | cluster==35 |
#            cluster==39 | cluster==34 |
#            cluster==41 | cluster==1  ) %>%
#   mutate(cluster=as.factor(cluster)) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, color = cluster)) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   theme_minimal() +
#   facet_wrap(~cluster, nrow = 3)
# 
# cluster.labels %>%
#   filter(cluster==42 | cluster==9 |
#            cluster==16 | cluster==24 |
#            cluster==37 ) %>%
#   mutate(cluster=as.factor(cluster)) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, color = cluster)) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   theme_minimal() +
#   facet_wrap(~cluster, nrow = 3)
# 
# cluster.labs <- c(#"33" = "Cluster 33: Environmental Protection/Biodiversity", 
#   "35"="Cluster 35: Common Fisheries Policy", 
#   "39"="Cluster 39: Work safety and health",
#   "34" = "Cluster 34: Occupational services/markets", 
#   "41"="Cluster 41: Water resources (emph. pollution)", 
#   "1"="Cluster 1: EU development funds", 
#   "42"= "Cluster 42: Sector environmental impact", 
#   "9"= "Cluster 9: Emissions/GHG",
#   "16" = "Cluster 16: Food import risks and regulations", 
#   "24"= "Cluster 24: Taxes", 
#   "37"= "Cluster 37: Food inspection (safety/quality)")
# 
# x <-as.data.frame(cbind(
#   V(network)$color,
#   as.numeric(as.factor(membership(network.clusters))) 
# )
# ) %>% distinct()
# 
# 
# p1 <- 
#   cluster.labels %>%
#   filter(  cluster==35) %>%
#   mutate(cluster=as.factor(cluster)) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, color = cluster)) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 15) +
#   theme_minimal() +
#   scale_color_manual(values=c("#80B1D3"))+
#   # facet_grid(.~cluster,  scales = "free", space = "free",
#   #          labeller = labeller(cluster = cluster.labs)) +
#   #facet_wrap(~cluster, nrow = 3,  scales = "free", shrink = FALSE,
#   #         labeller = labeller(cluster = cluster.labs)) +
#   labs(title = "Cluster 33: Environmental Protection/Biodiversity") +
#   theme(plot.title = element_text(face="bold", size = 12, hjust=0.5)) 
# p1
# 
# p2 <- 
#   cluster.labels %>%
#   filter(  cluster==35 |
#              cluster==39 | cluster==34 |
#              cluster==41 | cluster==1  |
#              cluster==42 | cluster==9  |
#              cluster==16 | cluster==24 |
#              cluster==37 ) %>%
#   mutate(cluster=as.factor(cluster)) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, color = cluster)) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 15) +
#   theme_minimal() +
#   scale_color_manual(breaks = c( "35", "39", "34",
#                                  "41", "1", "42", "9",
#                                  "16", "24", "37"),
#                      values=c(#"#80B1D3", 
#                        "#66C2A5", 
#                        "#984EA3", 
#                        "#B3E2CD", 
#                        "#FDDAEC", 
#                        "#FDB462", 
#                        "#A65628",
#                        "#FB9A99",
#                        "#FB8072",
#                        "#c9dba4", # --> changed to be slightly darker bc the other color was very hard to read the text
#                        "#CBD5E8"))+
#   # facet_grid(.~cluster,  scales = "free", space = "free",
#   #          labeller = labeller(cluster = cluster.labs)) +
#   facet_wrap(~cluster, nrow = 2,  scales = "free", shrink = FALSE,
#              labeller = labeller(cluster = cluster.labs)) +
#   theme( strip.text.x = element_text(face="bold", size = 12)) 
# p2
# 
# p2/p1
# 
# ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/03.DKclusterwordclouds.png",
#        width = 75,
#        height = 50,
#        units = c( "cm"),
#        limitsize = FALSE)
# 
# # these are the word clouds for the largest (>75%) clusters in term of number of verticies (documents acssociated)
# sort(table(network.clusters$membership))
# # cluster 33 is the largest!
# 
# quantile(table(network.clusters$membership),probs = c(0,.25,.5,.75,.95,1))
# 
# # inspecting them individually: 
# cluster.labels %>%
#   filter(cluster==33) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# 
# cluster.labels %>%
#   filter(cluster==35) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# 
# cluster.labels %>%
#   filter(cluster==39) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# 
# cluster.labels %>%
#   filter(cluster==34) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# 
# cluster.labels %>%
#   filter(cluster==41) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# 
# cluster.labels %>%
#   filter(cluster==1) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# 
# cluster.labels %>%
#   filter(cluster==37) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# 
# cluster.labels %>%
#   filter(cluster==24) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# 
# cluster.labels %>%
#   filter(cluster==16) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# 
# cluster.labels %>%
#   filter(cluster==9) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# 
# cluster.labels %>%
#   filter(cluster==42) %>%
#   ggplot(., aes( label = labels, size = total.labelcluster.degree, x = cluster, color = cluster )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_x_discrete(breaks = NULL) +
#   theme_minimal()
# ### trying ribbon plot ###
# 
# # get data in the right formate
# EUlinks.terms %>%
#   mutate(search.term = case_when( search.term == "jakt"~ "hunting",
#                                   search.term == "jagt"~ "hunting",
#                                   search.term == "fiske"~ "fisheries",
#                                   search.term == "fiskeri"~ "fisheries",
#                                   search.term == "sjofart"~ "maritime traffic",
#                                   search.term == "sotrafik"~ "maritime traffic")) %>%
#   group_by(to, country, search.term) %>%
#   summarise(n=n_distinct(from)) %>%
#   ggplot(.,
#        aes(y = n, axis1 = search.term, axis2 = to)) +
#   geom_alluvium(aes(fill = country), width = 1/12) +
#   geom_stratum(width = 1/12) +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)),
#             reverse = FALSE) +
#   facet_wrap(~ search.term, scales = "fixed") 
# 
# 
# # Combine all network stats:
# all.networkstats <- rbind(maritime.network.df,fisheries.network.df,hunting.network.df) %>%
#   mutate(country = case_when( str_detect(name,"sfs\\-") == "TRUE"  ~ "se" ,
#                               str_detect(name, "\\/eli\\/")== "TRUE"  ~ "dk",
#                               TRUE  ~ "EU" ))
# all.networkstats %>% 
#   filter(search.term == "fisheries") %>% 
#   summary(.)
# 
# all.networkstats %>% 
#   filter(search.term == "hunting") %>% 
#   summary(.)
# 
# all.networkstats %>% 
#   filter(search.term == "maritime") %>% 
#   summary(.)
# 
# n_distinct(all.networkstats$name) # 1025
# 
# all.networkstats1 <- 
#   document.key.df%>%
#   select(celex,labels) %>%
#   distinct() %>%
#   right_join(., all.networkstats, by= c("celex" = "name")) %>%
#   rename("name" = "celex")
# 
# n_distinct(all.networkstats1$name) # 1025 no losses or additions so passes the check 

# #these we didnt end up useing and they are not up-to-date since we didnt use them after the SE query pull updates

# fisheries network stats 

EUlinks.fisheriesmatrix <- 
  EUlinks.fisheries %>%
  mutate(links=1) %>%
  pivot_wider(names_from = from, values_from = links ,values_fill = 0) %>%
  column_to_rownames(var = "to") #higher trophic level (EU docs) is the columns

#EUfisheries.modules <- computeModules(EUlinks.fisheriesmatrix) #computed Nov 22nd, 2022 
#saveRDS(EUfisheries.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUfisheries.modules.RDS") 
EUfisheries.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUfisheries.modules.RDS")
plotModuleWeb(EUfisheries.modules, labsize = .5)
printoutModuleInformation(EUfisheries.modules) # 36 total modules

indices <- c( "degree","PDI","nestedrank")

EUfisheries.CELEX.networkstats <- 
  specieslevel(EUlinks.fisheriesmatrix, index=  indices, level = "lower",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(EUfisheries.CELEX.networkstats)
#degree         nestedrank        PDI        
#  Min.   : 1.000   Min.   :0.00   Min.   :0.7565  
#1st Qu.: 1.000   1st Qu.:0.25   1st Qu.:0.9948  
#Median : 1.000   Median :0.50   Median :1.0000  
#Mean   : 2.095   Mean   :0.50   Mean   :0.9943  
#3rd Qu.: 2.000   3rd Qu.:0.75   3rd Qu.:1.0000  
#Max.   :48.000   Max.   :1.00   Max.   :1.0000  



EUfisheries.CELEX.networkstats %>%
  slice_min(., order_by = nestedrank, n=3)%>%
  kable(., "latex")

EUfisheries.CELEX.networkstats %>%
  slice_max(., order_by = nestedrank, n=3)%>%
  kable(., "latex")



# hunting network stats 

EUlinks.huntingmatrix <- 
  EUlinks.hunting %>%
  mutate(links=1) %>%
  pivot_wider(names_from = from, values_from = links ,values_fill = 0) %>%
  column_to_rownames(var = "to") #higher trophic level (EU docs) is the columns

#EUhunting.modules <- computeModules(EUlinks.huntingmatrix) #computed Nov 22nd, 2022 
#saveRDS(EUhunting.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUhunting.modules.RDS") 
EUhunting.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUhunting.modules.RDS")
plotModuleWeb(EUhunting.modules)
printoutModuleInformation(EUhunting.modules) # 12 total modules

indices <- c( "degree","PDI","nestedrank")

EUhunting.CELEX.networkstats <- 
  specieslevel(EUlinks.huntingmatrix, index=  indices, level = "lower",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(EUhunting.CELEX.networkstats)
# Min.   : 1.00   Min.   :0.00   Min.   :0.4930  
#1st Qu.: 1.00   1st Qu.:0.25   1st Qu.:1.0000  
#Median : 1.00   Median :0.50   Median :1.0000  
#Mean   : 1.99   Mean   :0.50   Mean   :0.9861  
#3rd Qu.: 1.00   3rd Qu.:0.75   3rd Qu.:1.0000  
#Max.   :37.00   Max.   :1.00   Max.   :1.0000 


EUhunting.CELEX.networkstats %>%
  slice_min(., order_by = nestedrank, n=3)%>%
  kable(., "latex")

EUhunting.CELEX.networkstats %>%
  slice_max(., order_by = nestedrank, n=3)%>%
  kable(., "latex")


# maritime network stats ----------------------------------
EUlinks.maritimematrix <- 
  EUlinks.maritime %>%
  mutate(links=1) %>%
  pivot_wider(names_from = from, values_from = links ,values_fill = 0) %>%
  column_to_rownames(var = "to") #higher trophic level (EU docs) is the columns

#EUmaritime.modules <- computeModules(EUlinks.maritimematrix) #computed Nov 22nd, 2022 
#saveRDS(EUmaritime.modules, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUmaritime.modules.RDS") 
EUmaritime.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EUmaritime.modules.RDS")
plotModuleWeb(EUmaritime.modules)
printoutModuleInformation(EUmaritime.modules) # 24 total modules

indices <- c( "degree","PDI","nestedrank")

EUmaritime.CELEX.networkstats <- 
  specieslevel(EUlinks.maritimematrix, index=  indices, level = "lower",
               nested.weighted=FALSE,  PDI.normalise=TRUE,
               nested.method="NODF", 
               nested.normalised=TRUE)

summary(EUmaritime.CELEX.networkstats)
#  Min.   :1.000   Min.   :0.00   Min.   :0.9245  
#1st Qu.:1.000   1st Qu.:0.25   1st Qu.:1.0000  
#Median :1.000   Median :0.50   Median :1.0000  
#Mean   :1.355   Mean   :0.50   Mean   :0.9933  
#3rd Qu.:1.000   3rd Qu.:0.75   3rd Qu.:1.0000  
#Max.   :5.000   Max.   :1.00   Max.   :1.0000  

EUmaritime.CELEX.networkstats %>%
  slice_min(., order_by = nestedrank, n=3)%>%
  kable(., "latex")

EUmaritime.CELEX.networkstats %>%
  slice_max(., order_by = nestedrank, n=3)%>%
  kable(., "latex")


# 04_r code network stats ------------------------------------


identical(as.data.frame(dkse.bi), adj.matrix)
all.equal(dkse.bi, adj.matrix)

#dkse.clusters<-computeModules(dkse.bi) #are those the same?
SEDK.modules <-  readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.SEDK.modules.RDS")

plotModuleWeb(dkse.clusters)

sedkdependence<-linklevel(dkse.bi,"dependence")

sedkdependence$LL #is Sweden dependence on DK
sedkdependence$HL #is DK dependnce on SE


table(apply(sedkdependence$HL,2,which.max)) # what SE text are DK texts most dependent on
#looks like SE 17th
rownames(dkse.bi)[17]
#"sfs-1999-657" # happens to be top betweenness


table(apply(sedkdependence$LL,1,which.max)) # what DK text are SE texts most dependent on
#looks like DK 36, but close also 1, 8, and 111
colnames(dkse.bi)[c(36,1,8,111)]
# "/eli/lta/2021/2246" "/eli/lta/2019/985"  "/eli/lta/2022/964"  "/eli/lta/2022/139"



#eudk.module<-computeModules(eudk.bi)  # are those the same?
#euSE.module<-computeModules(euSE.bi)  # are those the same?

