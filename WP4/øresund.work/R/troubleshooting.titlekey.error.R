# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("tidyr")
library("tibble")
library("dplyr")
library("eurlex")
library("purrr")

# Build a legislation-title key -------------------------------------------------

# EURLEX KEY
document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

document.key.df1 <- 
  document.key.df %>% 
  select(resource.type, work, celex) %>%
  mutate(celex = as.factor(celex)) %>%
  distinct(celex, .keep_all = TRUE) 

document.key.df1 %>%
  group_by(resource.type) %>%
  summarise(n=n_distinct(celex))


rm(document.key.df) # remove this bc it takes up a lot of space

# --- Directives --- #

# run on 25-10-2022
directive.titles <- 
  document.key.df1 %>%
  filter(resource.type == "DIR") %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)

test<-array(0) #2659, 4080 problem ones, skip these and do them later (see below)
for (i in 4081:dim(directive.titles)[1]) {
  test[i]<-elx_fetch_data(directive.titles$work[i],type="title")
  print(i)
  flush.console()
}

i<-2659
test[i]<-elx_fetch_data(directive.titles$work[i],type="title")
# ran this above
i<-4080
test[i]<-elx_fetch_data(directive.titles$work[i],type="title")

directive.titles$titles<-test

write.csv(directive.titles, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EurLexKey.directive.titles.csv") 
       
# -------------
#directive.titles <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EurLexKey.directive.titles.csv")

#directive.titles <-
#  directive.titles %>%
#  select(-X) %>% 
 # mutate(title.code =  str_extract(titles, "[:digit:]+/[:digit:]+/[:alpha:]+"))


# ok this is taking FOREVER AND WORSE THAN BEFORE RUNNING THE DIRECTIVES... trying somthing new     
# --- Decisions --- #

decision.titles <- 
  document.key.df1 %>%
  filter(resource.type == "DEC") %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)

# Working on this one first
decision.titles.1.12002 <- 
  slice(decision.titles,1:12002)

rm(document.key.df1) # remove this bc it takes up a lot of space
rm(decision.titles) # remove this bc it takes up a lot of space

dec.test<-array(0) # 127 problem one make title blank url is faulty..., 386 skip these and do them later (see below)

gc()
for (i in 4555:dim(decision.titles.1.12002)[1]) {
  dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
  print(i)
  flush.console()
  Sys.sleep(1)
  gc()
}

i<-127 #--> faulty link make title blank
dec.test[127]<- " "
# ran this above
i<-278
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-528
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-610
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-653
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-693
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-761
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-854
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-1170
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-1257
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-1576
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-1845
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2048
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2058
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2137
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2161
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2205
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2211
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2222
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2269
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2272
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2293
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2331
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2334
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2338
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2432
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2438
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2520
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2522
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2562
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2581
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2590
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2625
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2760
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2763
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2904
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2921
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2933
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2948
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-2971
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3033
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3037
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3183
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3370
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3400
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3403
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3427
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3510
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3513
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3522
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3590
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3595
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3623
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3824
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3859
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3894
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3961
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-3996
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-4092
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-4295
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-4351
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-4400
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-4440
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-4482
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-4488
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-4524
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")
i<-4555
dec.test[i]<-elx_fetch_data(decision.titles.1.12002$work[i],type="title")

decision.titles.1.4725 <- 
  decision.titles.1.12002 %>%
  slice(.,1:4725)

decision.titles.1.4725$titles<-dec.test

write.csv(decision.titles.1.4725, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/TESTEurLexKey.decision.titles1.4725.csv") 

# --- second part --- #

  decision.titles <- 
  document.key.df1 %>%
  filter(resource.type == "DEC") %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)

# Working on this one first
decision.titles.4725.8725 <- 
  slice(decision.titles,4725:8725)

rm(document.key.df1) # remove this bc it takes up a lot of space
rm(decision.titles) # remove this bc it takes up a lot of space

dec.test<-array(0) # 127 problem one make title blank url is faulty..., 386 skip these and do them later (see below)

gc()
for (i in 1:dim(decision.titles.4725.8725)[1]) {
  dec.test[i]<-elx_fetch_data(decision.titles.4725.8725$work[i],type="title")
  print(i)
  flush.console()
  Sys.sleep(1)
  gc()
}


decision.titles1.4725.8725 <- 
  decision.titles.4725.8725 

decision.titles1.4725.8725$titles<-dec.test

write.csv(decision.titles1.4725.8725, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/TESTEurLexKey.decision.titles.4725.8725.csv") 


# --- third part --- #

decision.titles <- 
  document.key.df1 %>%
  filter(resource.type == "DEC") %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)

# Working on this one first
decision.titles.8726.16726 <- 
  slice(decision.titles,8726:16726)
decision.titles.8726.16726[4307,]
rm(document.key.df1) # remove this bc it takes up a lot of space
rm(decision.titles) # remove this bc it takes up a lot of space

dec.test<-array(0) # 127 problem one make title blank url is faulty..., 386 skip these and do them later (see below)

gc()
for (i in 4308:dim(decision.titles.8726.16726)[1]) {
  dec.test[i]<-elx_fetch_data(decision.titles.8726.16726$work[i],type="title")
  print(i)
  flush.console()
}

i<-2913
dec.test[i]<-elx_fetch_data(decision.titles.8726.16726$work[i],type="title")
4307#--> faulty link make title blank
dec.test[4307]<- " "

decision.titles1.8726.16726 <- 
  decision.titles.8726.16726 

decision.titles1.8726.16726$titles<-dec.test

write.csv(decision.titles1.8726.16726, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/TESTEurLexKey.decision.titles.8726.16726.csv") 


# --- fourth part --- #

decision.titles <- 
  document.key.df1 %>%
  filter(resource.type == "DEC") %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)

# Working on this one first
decision.titles.16727.48008<- 
  slice(decision.titles,16727:48008)

rm(document.key.df1) # remove this bc it takes up a lot of space
rm(decision.titles) # remove this bc it takes up a lot of space

dec.test<-array(0) # 127 problem one make title blank url is faulty..., 386 skip these and do them later (see below)

gc()
for (i in 23188:dim(decision.titles.16727.48008)[1]) {
  httr::handle_reset("http://publications.europa.eu/")
  dec.test[i]<-elx_fetch_data(decision.titles.16727.48008$work[i],type="title")
  print(i)
  flush.console()
}


decision.titles1.16727.48008 <- 
  decision.titles.16727.48008 

decision.titles1.16727.48008$titles<-dec.test


write.csv(decision.titles1.16727.48008, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/TESTEurLexKey.decision.titles.16727.48008.csv") 

# let combine the four directives part ----

dec.1 <- read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/TESTEurLexKey.decision.titles1.4725.csv")
dec.2 <- read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/TESTEurLexKey.decision.titles.4725.8725.csv")
dec.3 <- read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/TESTEurLexKey.decision.titles.8726.16726.csv")
dec.4 <- read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/TESTEurLexKey.decision.titles.16727.48008.csv")

Eurlexkey.decision.titles <- rbind(dec.1,dec.2,dec.3,dec.4) # i duplicated the title for poisition 4725 see below
Eurlexkey.decision.titles[4725,]
Eurlexkey.decision.titles[4726,]

Eurlexkey.decision.titles1 <-
  Eurlexkey.decision.titles%>%
  distinct(work, .keep_all=TRUE) %>% # ok back to the original dimentions
  left_join(., document.key.df1, by = c("work")) %>%
  select(-X)

write.csv(Eurlexkey.decision.titles1, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03EurLexKey.decision.titles.csv", row.names=FALSE) 

# --- Regulation --- #

# ok the harvard database has up to 2019 so lets extract 2020-2022
document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/0EGYWY 

library(lubridate)

regulation.titles <- 
  document.key.df %>%
  filter(resource.type == "REG")%>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  filter(year > 2019) %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(celex,work)


rm(document.key.df) # remove this bc it takes up a lot of space

reg.test<-array(0) # 

gc()
for (i in 1547:dim(regulation.titles)[1]) {
  httr::handle_reset("http://publications.europa.eu/")
  reg.test[i]<-elx_fetch_data(regulation.titles$work[i],type="title")
  print(i)
  flush.console()
}

i<-1546
reg.test[i]<-elx_fetch_data(regulation.titles$work[i],type="title")

regulation.titles1 <-
  regulation.titles 

regulation.titles1$titles<-reg.test


write.csv(regulation.titles1, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03EurlLexKey.regulation.titles.yr2020.2022.csv", row.names=FALSE) 

# --- Reccomendations --- #

reccomendations.titles <- 
  document.key.df1 %>%
  filter(resource.type == "RECO")%>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)


rm(document.key.df1) # remove this bc it takes up a lot of space

reco.test<-array(0) # 

gc()
for (i in 3116:dim(reccomendations.titles)[1]) {
  httr::handle_reset("http://publications.europa.eu/")
  reco.test[i]<-elx_fetch_data(reccomendations.titles$work[i],type="title")
  print(i)
  flush.console()
  gc()
}

i<-104
reco.test[i]<-elx_fetch_data(reccomendations.titles$work[i],type="title")
i<-535
reco.test[i]<-elx_fetch_data(reccomendations.titles$work[i],type="title")
i<-568
reco.test[i]<-""
i<-654
reco.test[i]<-""
i<-2474
reco.test[i]<-elx_fetch_data(reccomendations.titles$work[i],type="title")
i<-2787
reco.test[i]<-""
i<-3115
reco.test[i]<-""

reccomendations.titles1 <- 
  reccomendations.titles

reccomendations.titles1$titles<-reco.test

write.csv(reccomendations.titles1, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03EurlLexKey.reccomendations.titles.csv", row.names=FALSE) 


# Archived ---------------------------

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




