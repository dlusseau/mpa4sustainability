
# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("readr")
library("tidyr")
library("tibble")
library("tm")
library("corpus")
library("stringr")
library("dplyr")
library("sentimentr")
library("eurlex")
library("purrr")
library("tidyverse")
library("readxl")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------

# list of result text
sjofart.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.sjofart.list" )
jakttext.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.jakttext.list" )
fisketext.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.fisketext.list" )


sjofart.metadata <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.sjofart.df.csv")
jakt.metadata <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.jakt.df.csv")
fisk.metadata <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.fisk.df.csv")

# making into a nice df --------------------------------------------------------

sjofarttext.df <- 
  as.data.frame(unique(cbind(sjofart.list))) %>% #nned to use unique () because some of the results were duplicates
  rownames_to_column(., var = "doc.id") %>%
  rename("text" = "sjofart.list") %>%
  unnest(text, keep_empty=TRUE) %>%
  mutate(search.term = "sjofart") %>%
  mutate(country = "SE") 

jakttext.df <- 
  as.data.frame(unique(cbind(jakttext.list))) %>% 
  rownames_to_column(., var = "doc.id") %>%
  rename("text" = "jakttext.list") %>%
  unnest(text, keep_empty=TRUE) %>%
  mutate(search.term = "jakt") %>%
  mutate(country = "SE") 

fisketext.df <- 
  as.data.frame(unique(cbind(fisketext.list))) %>% 
  rownames_to_column(., var = "doc.id") %>%
  rename("text" = "fisketext.list") %>%
  unnest(text, keep_empty=TRUE) %>%
  mutate(search.term = "fiske") %>%
  mutate(country = "SE") 

SEtext.dk <- rbind(sjofarttext.df,jakttext.df,fisketext.df)


SEmeta.dk <- 
  rbind(sjofart.metadata,jakt.metadata,fisk.metadata)%>%
  mutate(id = str_replace_all(id, "-", "."),
         id = str_replace_all(id, " ", "."))  %>%
  mutate(id=as.factor(id)) %>%
  distinct(id, .keep_all=TRUE)

n_distinct(SEtext.dk$doc.id)


#looking for the other forms of hunting and fishing 
#fisheries (commercial, recreational and spear fishing) : fiske (yrkesfiske, fritidsfiske, harpunfiske). 
# You could possibly also look for “husbehovsfiske” or “fiske för husbehov” (approx. subsidiary fishing).
#hunting (along with bird and seal hunting): jakt, fågeljakt/sjöfågeljakt, säljakt
#and maritime traffic: sjöfart (general term, name of the sector), båttrafik (boating), småbåtstrafik (small private vessels), fritidsbåtstrafik (recreational vessels), fartygstrafik (large vessels), 
#Routes: farled (general term), fartygsled/sjöfartsled/transportled (routes for larger vessels), farled/båtled (route for smaller vessels)

SEtext.dk1 <-
  SEtext.dk %>%
  mutate(harpun = case_when(search.term == "fiske" ~ str_detect(text, "harpun|Harpun")), #stringr is case sensitive so make sure to have both :)
         commercial = case_when(search.term == "fiske" ~ str_detect(text, "yrkesfisk|Yrkesfisk")),
         recreational = case_when(search.term == "fiske" ~ str_detect(text, "fritidsfisk|Fritidsfisk")), 
         angling = case_when(search.term == "fiske" ~ str_detect(text, "handredskapsfisk|Handredskapsfisk")), 
         angling2    = case_when(search.term == "fiske" ~ str_detect(text, "spöfisk|Spöfisk")),
         houseneeds.fishing = case_when(search.term == "fiske" ~ str_detect(text, "husbehovsfisk|Husbehovsfisk")), 
         houseneeds.fishing2 = case_when(search.term == "fiske" ~ str_detect(text, "fiske för husbehov|Fiske för husbehov")),
         birdhunt = case_when(search.term == "jakt" ~ str_detect(text, "fågel|Fågel")),
         seal = case_when(search.term == "jakt" ~ str_detect(text, "säl|Säl")),
         boat.traffic = case_when(search.term == "sjofart" ~ str_detect(text, "båtstrafik|Båtstrafik")))

n_distinct(SEtext.dk1$doc.id)

SEtext.dk1 %>%
  mutate(doc.id = str_replace_all(doc.id, "-", "."),
         doc.id = str_replace_all(doc.id, " ", "."))%>%
  group_by(search.term) %>%
  summarise(n=n_distinct(doc.id))

# fiske         257
# jakt           93
# sjofart       199

SEtext.dk1 %>%
  mutate(doc.id = str_replace_all(doc.id, "-", "."),
         doc.id = str_replace_all(doc.id, " ", ".")) %>%
  summarise(n=n_distinct(doc.id))
#444

SEtext.dk1 %>%
  filter(search.term == "fiske" & harpun == "TRUE") # 0 mention harpun

SEtext.dk1 %>%
  filter(search.term == "fiske" & commercial == "TRUE") # 26 fisheries documents mention yrkesfisk

SEtext.dk1 %>%
  filter(search.term == "fiske" & recreational == "TRUE") # 10 fisheries documents mention fritidsfisk fisk

SEtext.dk1 %>%
  filter(search.term == "fiske" & angling == "TRUE") # 2 mention handredskapsfisk

SEtext.dk1 %>%
  filter(search.term == "fiske" & angling2 == "TRUE") # 1  documents mention spöfisk

SEtext.dk1 %>%
  filter(search.term == "fiske" & houseneeds.fishing == "TRUE") # 1  documents mention husbehovsfisk

SEtext.dk1 %>%
  filter(search.term == "fiske" & houseneeds.fishing2 == "TRUE") # 0  documents mention fiske för husbehov

SEtext.dk1 %>%
  filter(search.term == "jakt" & birdhunt == "TRUE") # 7 fågel

#SEtext.dk1 %>%
 # filter(search.term == "jakt" & waterfowl == "TRUE") # 1 sjöfågel

SEtext.dk1 %>%
  filter(search.term == "jakt" & seal == "TRUE") # 62 mention säl fisk

SEtext.dk1 %>%
  filter(search.term == "sjofart" & boat.traffic == "TRUE") # 3 mention båtstrafik


SEtext.dk1 %>%
  mutate(doc.id = str_replace_all(doc.id, "-", "."),
         doc.id = str_replace_all(doc.id, " ", ".")) %>%
  select(-text) %>%
  mutate(doc.id=as.factor(doc.id)) %>%
  left_join(.,SEmeta.dk, by = c("doc.id"="id") ) %>%
  write.csv(., file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.SEdocmetadata.clean.csv", row.names=FALSE)

# Getting Eurlex links ---------------------------------------------------------

#https://eur-lex.europa.eu/content/help/eurlex-content/numbering-of-acts.html 
# they do put habitatdirektiv and fågeldirektiv ramdirektiv för vatten
# maybe they sight others


# Example of how directives could be cited: 
# Utredaren ska även föreslå de författningsändringar som krävs för att säkerställa att kraven i artikel 6.2 och 6.3 i 
# rådets direktiv 92/43/EEG av den 21 maj 1992 om bevarande av livsmiljöer samt vilda djur och växter (art- och habitatdirektivet) tillämpas fullt ut på fiske i enlighet med Sveriges EU-rättsliga åtaganden.

# Decisions:
# kommissionens beslut 2005/909/EG.

# Reccomendations: 
# kommissionens rekommendation 2003/361/EG

# council regulations:
# rådets förordning EEG nr 2658/87 

# lets first detect if sentences mention regulation types 
EU.links <- 
  SEtext.dk %>%
  get_sentences() %>%
  mutate(dir =  str_extract(text, "direktiv|Direktiv")) %>% 
  unnest(dir,keep_empty = TRUE) %>%
  mutate(reg =  str_extract(text, "förordning|Förordning")) %>% 
  unnest(reg,keep_empty = TRUE) %>%
  mutate(dec =  str_extract(text, "beslut|Beslut|genomförandebeslut|Genomförandebeslut")) %>% 
  unnest(dec,keep_empty = TRUE) %>%
  mutate(rec =  str_extract(text, "rekommendation|Rekommendation"))%>% 
  unnest(rec,keep_empty = TRUE)

EU.links2 <-
  EU.links %>%
  pivot_longer(.,
               cols = dir:rec,
               names_to = "type",
               values_to = "ref") %>%
  distinct() %>%
  mutate(ref=as.factor(ref),
         text=as.character(text)) %>%
  filter(!is.na(ref)) # remove na values

# then lets pull all the codes in those sentences...

EU.links3 <-
  EU.links2 %>%
  mutate(code = case_when(ref == "direktiv" ~  as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")),
                          ref == "Direktiv" ~  as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")),
                          ref == "förordning" ~ as.character(str_extract_all(text, "\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|EEG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|EG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|nr\\s[:digit:]+/[:digit:]+/[:alpha:]+|förordning\\snr\\s[:digit:]+(?=\\s)")),
                          ref == "Förordning" ~ as.character(str_extract_all(text, "\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|EEG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|EG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|nr\\s[:digit:]+/[:digit:]+/[:alpha:]+|förordning\\snr\\s[:digit:]+(?=\\s)")),
                          ref == "beslut" ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")),
                          ref == "genomförandebeslut" ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")),
                          ref == "Beslut" ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")),
                          ref == "Genomförandebeslut" ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")),
                          ref == "rekommendation" ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+")),
                          ref == "Rekommendation" ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+")))) %>%
  mutate(code = str_replace_all(code,"c\\(",""),
         code = str_replace_all(code,"(?<!G|0|U)\\)",""),
         code = str_replace_all(code,'\\"',"")) %>%
  mutate(code = na_if(code,"character(0)")) %>% #change these values to NA
  filter(!is.na(code)) %>% # remove the NAs
  mutate(code = strsplit(code, ",")) %>%
  unnest(code) %>%
  mutate(code = str_trim(code, side = c("both")))%>%
  mutate(code = str_replace_all(code, "EG", "EC"), # change from SE to EN language 
         code = str_replace_all(code, "nr", "No"),
         code = str_replace_all(code, "RIF", "JHA"))




#### title Keys from EurLex ###

# EURLEX KEY
document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

document.dir.key.df1 <- 
  document.key.df %>% 
  select(resource.type, work, celex) %>%
  mutate(celex = as.factor(celex)) %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(resource.type == "DIR") %>%
  select(work,celex)



directive.titles <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EurLexKey.directive.titles.csv") 


directive.titles1 <-
  directive.titles %>%
  select(-X) %>% 
  left_join(.,document.dir.key.df1, by = c("work")) %>%
  mutate(title2 = str_trunc(titles,75,side = c("right"))) %>%
  mutate(code =  str_extract(title2, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")) %>%
  mutate(type = "dir")  %>%
  select(celex,type,code)

EU.links4 <-
  EU.links3 %>%
  left_join(., directive.titles1, by = c("type", "code")) %>%
  rename(celex.dir = celex)



decision.titles <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03EurLexKey.decision.titles.csv") 


decision.titles1 <-
  decision.titles %>%
  mutate(title2 = str_trunc(titles,75,side = c("right"))) %>%
  mutate(code =  str_extract(title2, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")) %>%
  mutate(type = "dec")  %>%
  select(celex,type,code) 


EU.links5 <-
  EU.links4 %>%
  left_join(., decision.titles1, by = c("type", "code")) %>%
  rename(celex.dec = celex)%>%
  mutate(celex.dir = as.character(celex.dir))


reccomendation.titles <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03EurlLexKey.reccomendations.titles.csv") 


document.rec.key.df1 <- 
  document.key.df %>% 
  select(resource.type, work, celex) %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(resource.type == "RECO") %>%
  select(work,celex)

reccomendation.titles1 <-
  reccomendation.titles %>%
  left_join(.,document.rec.key.df1, by = c("work")) %>%
  mutate(title2 = str_trunc(titles,75,side = c("right"))) %>%
  mutate(code =  str_extract(title2, "[:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  mutate(type = "rec")  %>%
  select(celex,type,code)

EU.links6 <-
  EU.links5 %>%
  left_join(., reccomendation.titles1, by = c("type", "code")) %>%
  rename(celex.rec = celex)

regulation.titles.yr2020.2022 <- 
  read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03EurlLexKey.regulation.titles.yr2020.2022.csv") %>%
  select(-work) 

regulation.titles <-  read_excel("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/EurLex_regulations_no_text_all.xlsx") 

regulation.titles1 <-
  regulation.titles %>%
  select(CELEX, Act_name)%>%
  distinct(CELEX, .keep_all=TRUE) %>%
  rename("celex" = "CELEX",
         "titles" = "Act_name") %>%
  rbind(.,regulation.titles.yr2020.2022) %>%
  mutate(titles = str_remove_all(titles, "Ã|Ã o ")) %>%
  mutate(title2 = str_replace(titles," °", "o")) %>%
  mutate(title3 = str_replace(title2,"N0", "No")) %>%
  mutate(title4 = str_replace(title3,"No\\.", "No")) %>%
  mutate(title5 = str_replace(title3,"no\\.", "No")) %>%
  mutate(title6 = str_replace(title5,"\\(\\sEEC\\)", "\\(EEC\\)")) %>%
  mutate(title7 = str_replace(title6,"\\(EEC ", "\\(EEC\\)")) %>%
  mutate(title8 = str_trunc(title7,75,side = c("right")),
         title9 = str_replace_all(title8,"\\(\\sEEC\\s\\)", "\\(EEC\\)")) %>%
  mutate(code =  str_extract(title9, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\,\\s[:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\,\\s[:alpha:]+,\\s[:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EEG+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EG+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|No\\s[:digit:]+/[:digit:]+/[:alpha:]+|Regulation\\sNo\\s[:digit:]+(?=\\s)")) %>%
    mutate(type = "reg")  %>%
  select(celex,type,code)




EU.links7 <-
  EU.links6 %>%
  distinct() %>% # remove those that are mentioned exactly the same multiple times in a sentence
  left_join(., regulation.titles1, by = c("type", "code")) %>%
  rename(celex.reg = celex) %>%
  mutate(ref = as.character(ref)) %>%
  mutate(celex.reg = case_when(type == "reg" & code == "(EC) No 1907/2006" ~ "32006R1907",
                               type == "reg" & code == "(EC) 1383/2003" ~ "32003R1383",
                               type == "reg" & code == "(EC) No 1966/2006" ~ "32006R1966",
                               type == "reg" & code == "(EC) 1407/2002" ~ "32002R1407",
                               type == "reg" & code == "No 4064/89/EEC" ~ "31989R4064",
                               type == "reg" & code == "No 3975/87/EEC" ~ "31987R3975",
                               type == "reg" & code == "No 2367/90/EEC" ~	"31990R2367",
                               type == "reg" & code == "(EU) No 2019/1021" ~	"32019R1021",
                               type == "reg" & code == "(EEC) 793/93" ~	"31993R0793",
                               type == "reg" & code == "(EU) No 2016/424" ~	"32016R0424",
                               type == "reg" & code == "förordning No 17" ~	"31962R0017",
                               type == "reg" & code == "(EU) 2019/1896" ~	"32019R1896",
                               type == "reg" & code == "(EU) No 868/2014" ~	"32014R0868",
                               type == "reg" & code == "(EU) No 2019/1248" ~	"32019R1248",
                               type == "reg" & code == "(EU) No 2019/1241" ~	"32019R1241",
                               type == "reg" & code == "(EU) 2019/1603" ~	"32019R1603",
                               type == "reg" & code == "(EU) 2019/2033" ~	"32019R2033",
                               type == "reg" & code == "(EU) 2021/836" ~	"32021R0836",
                               type == "reg" & code == "(EU) 2021/1139" ~	"32021R1139",
                               type == "reg" & code == "(EU) 2020/262" ~	"32021R1139",
                               type == "reg" & code == "(EEC) 2658/87" ~	"31987R2658",
                               type == "reg" & code == "(EEC) No 1182/71" ~	"31971R1182",
                               type == "reg" & code == "(EEC) No 2299/8" ~	"31989R2299", # these are actioally 80
                               type == "reg" & code == "(EEC) No 2343/9" ~	"31990R2343", # 90
                               type == "reg" & code == "(EEC) No 2344/9" ~	"31990R2344",# 90
                               type == "reg" & code == "(EEC) No 3975/8" ~	"31987R3975",# 87
                               type == "reg" & code == "(EU) 1380/2013" ~	"32013R1380",# 
                               type == "reg" & code == "No 3975/87/n" ~	"31987R3975",#  n is how they refe a section in the SE leg. 
                               type == "reg" & code == "(EC) 1907/2006" ~	"32006R1907",# 
                               type == "reg" & code == "(EEC) No 3976/8" ~	"31987R3976",# 87
                               type == "reg" & code == "(EU) 297/2008" ~	"32008R0297",# 
                               type == "reg" & code == "(EU) No 2016/425" ~	"32016R0425",# 
                               type == "reg" & code == "(EC) No 820/974" ~	"31997R0820",# typo (EC) No 820/974 it is (EC) No 820/97
                               
                               TRUE ~ celex.reg)) %>%
  mutate(celex.rec = case_when(type == "rec" & code == "2003/361/EC" ~ "32003H0361", # all recos are done!
                               TRUE ~ celex.rec)) %>%
  mutate(celex.dir = case_when(type == "reg" & code == "(EU) 2019/713" ~ "32019L0713", # this is a directive but didnt follow the directive coding and 
                               TRUE ~ celex.dir))%>%
  mutate(type = case_when(type == "reg" & code == "(EU) 2019/713" ~ "dir", # this is a directive but didnt follow the directive coding and 
                               TRUE ~ type)) %>%
  mutate(ref = case_when(doc.id == "sfs-2014-1102" & code == "(EU) 2019/713" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))         %>%
  mutate(celex.dir = case_when(type == "reg" & code == "(EU) 2018/843" ~ "32018L0843", # this is a directive but didnt follow the directive coding and 
                               TRUE ~ celex.dir))%>%
  mutate(type = case_when(type == "reg" & code == "(EU) 2018/843" ~ "dir", # this is a directive but didnt follow the directive coding and 
                          TRUE ~ type)) %>%
  mutate(ref = case_when(doc.id == "sfs-2014-1102" & code == "(EU) 2018/843" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))      %>%
  mutate(celex.dir = case_when(type == "reg" & code == "(EU) 2020/262" ~ "32020L0262", # this is a directive but didnt follow the directive coding and 
                               TRUE ~ celex.dir))%>%
  mutate(type = case_when(type == "reg" & code == "(EU) 2020/262" ~ "dir", # this is a directive but didnt follow the directive coding and 
                          TRUE ~ type)) %>%
  mutate(ref = case_when(doc.id == "sfs.1994.1776" & code == "(EU) 2020/262" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))  %>%
  mutate(ref = case_when(doc.id == "sfs-1994-1776" & code == "(EU) 2020/262" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))   %>% 
  mutate(celex.dir = case_when(type == "reg" & code == "(EU) 2015/413" ~ "32015L0413", # this is a directive but didnt follow the directive coding and 
                               TRUE ~ celex.dir))%>%
  mutate(type = case_when(type == "reg" & code == "(EU) 2015/413" ~ "dir", # this is a directive but didnt follow the directive coding and 
                          TRUE ~ type)) %>%
  mutate(ref = case_when(doc.id == "sfs-2014-1102" & code == "(EU) 2015/413" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))  %>%
  mutate(celex.dir = case_when(type == "reg" & code == "(EU) 2015/652" ~ "32015L0652", # this is a directive but didnt follow the directive coding and 
                               TRUE ~ celex.dir))%>%
  mutate(type = case_when(type == "reg" & code == "(EU) 2015/652" ~ "dir", # this is a directive but didnt follow the directive coding and 
                          TRUE ~ type)) %>%
  mutate(ref = case_when(doc.id == "sfs.2014.1434" & code == "(EU) 2015/652" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))  %>%
  mutate(ref = case_when(doc.id == "sfs-2014-1434" & code == "(EU) 2015/652" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))%>%
  mutate(celex.dir = case_when(type == "reg" & code == "(EU) 2018/851" ~ "32018L0851", # this is a directive but didnt follow the directive coding and 
                               TRUE ~ celex.dir))%>%
  mutate(type = case_when(type == "reg" & code == "(EU) 2018/851" ~ "dir", # this is a directive but didnt follow the directive coding and 
                          TRUE ~ type)) %>%
  mutate(ref = case_when(doc.id == "sfs-2020-614" & code == "(EU) 2018/851" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))%>%
  mutate(ref = case_when(doc.id == "sfs.2020.614" & code == "(EU) 2018/851" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))%>%
  mutate(celex.dir = case_when(type == "reg" & code == "(EU) 2015/849" ~ "32015L0849", # this is a directive but didnt follow the directive coding and 
                               TRUE ~ celex.dir))%>%
  mutate(type = case_when(type == "reg" & code == "(EU) 2015/849" ~ "dir", # this is a directive but didnt follow the directive coding and 
                          TRUE ~ type)) %>%
  mutate(ref = case_when(doc.id == "sfs-2014-1102" & code == "(EU) 2015/849" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))%>%
  mutate(celex.dir = case_when(type == "reg" & code == "(EU) 2017/1132" ~ "32017L1132", # this is a directive but didnt follow the directive coding and 
                               TRUE ~ celex.dir))%>%
  mutate(type = case_when(type == "reg" & code == "(EU) 2017/1132" ~ "dir", # this is a directive but didnt follow the directive coding and 
                          TRUE ~ type)) %>%
  mutate(ref = case_when(doc.id == "sfs-2009-400" & code == "(EU) 2017/1132" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))%>%
  mutate(ref = case_when(doc.id == "sfs.2009.400" & code == "(EU) 2017/1132" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))%>%
  mutate(celex.dir = case_when(type == "dec" & code == "(EU) 2018/2001" ~ "32018L2001", # this is a directive but didnt follow the directive coding and 
                               TRUE ~ celex.dir))%>%
  mutate(type = case_when(type == "dec" & code == "(EU) 2018/2001" ~ "dir", # this is a directive but didnt follow the directive coding and 
                          TRUE ~ type)) %>%
  mutate(ref = case_when(doc.id == "sfs.2011.1088" & code == "(EU) 2018/2001" ~ "direktiv", # this is a directive but didnt follow the directive coding and 
                         TRUE ~ ref))%>%
  filter(code != "No 529/2013/EU") %>% # No 529/2013/EU No 280/2004/EC	No 1313/2013/EU		 these are decisions but dec pulled it without No so remove row when type ==reg
  filter(code != "No 280/2004/EC") %>% 
  filter(code != "No 1313/2013/EU") %>% 
  filter(code != "4064/89/EEC") %>% # 4064/89/EEC	 not a decision but the regulation pull got it but with the No at the beginning
  filter(code != "2344/90/n") %>%   # 2344/90/n	and 3976/87/n	a regulation and got pulled correctly 
  filter(code != "3976/87/n") %>%   # 2344/90/n	and 3976/87/n	a regulation and got pulled correctly 
  filter(code != "förordning No 187") %>% # förordning No 187	 not EU regulation says "royal regulation No 187"
   mutate(celex.dec = case_when(type == "dec" & code == "(EU) 2018/552" ~ "32018D0552", #  
                               TRUE ~ celex.dec))%>%
  mutate(celex.dec = case_when(type == "dec" & code == "(EU) 2021/2326" ~ "32021D2326", # 
                               TRUE ~ celex.dec))%>%
  mutate(celex.dec = case_when(type == "dec" & code == "(EU) 2017/302" ~ "32017D0302", # 
                               TRUE ~ celex.dec))%>%
  mutate(celex.dec = case_when(type == "dec" & code == "(EU) 2017/1442" ~ "32017D1442", # 
                               TRUE ~ celex.dec)) %>%
  mutate(celex.dec = case_when(type == "dir" & code == "2004/27/EC" ~ "32004L0027", #  this was mising from the title key pull dont know why
                               TRUE ~ celex.dec))%>%
  mutate(celex.dec = case_when(type == "dec" & code == "2009/371/JHA" ~ "32009D0371", #  this offcical title didnt have its code so missing from the key and did not come out of the eurlex pull?
                               TRUE ~ celex.dec)) %>%
  mutate(celex.dir = case_when(doc.id == "sfs.2008.245" & sentence_id == 94 & code == "2004/42/EC" ~ "32004L0042", # # 2004/42/EC	real title is CE --> for sfs.2008.245	sentence 94 it is def the code with CE (32004L0042) but others not clear even in the sentences
                               TRUE ~ celex.dir))%>%
  mutate(celex.dir = case_when(doc.id == "sfs.2010.770" & sentence_id == 98 & code == "2004/36/EC" ~ "32004L0036", # 2004/36/EC	real title is CE --> for sfs.2010.770	sentence 98,, 394, 406 and sfs.1986.171	sentence 533 and 105 it is celex (32004L0036) that title with ce
                               TRUE ~ celex.dir))%>%
  mutate(celex.dir = case_when(doc.id == "sfs.2010.770" & sentence_id == 394 & code == "2004/36/EC" ~ "32004L0036", # 2004/36/EC	real title is CE --> for sfs.2010.770	sentence 98,, 394, 406 and sfs.1986.171	sentence 533 and 105 it is celex (32004L0036) that title with ce
                               TRUE ~ celex.dir))%>%
  mutate(celex.dir = case_when(doc.id == "sfs.2010.770" & sentence_id == 406 & code == "2004/36/EC" ~ "32004L0036", # 2004/36/EC	real title is CE --> for sfs.2010.770	sentence 98,, 394, 406 and sfs.1986.171	sentence 533 and 105 it is celex (32004L0036) that title with ce
                               TRUE ~ celex.dir))%>%
  mutate(celex.dir = case_when(doc.id == "sfs.1986.171" & sentence_id == 533 & code == "2004/36/EC" ~ "32004L0036", # 2004/36/EC	real title is CE --> for sfs.2010.770	sentence 98,, 394, 406 and sfs.1986.171	sentence 533 and 105 it is celex (32004L0036) that title with ce
                               TRUE ~ celex.dir))%>%
  mutate(celex.dir = case_when(doc.id == "sfs.1986.171" & sentence_id == 105 & code == "2004/36/EC" ~ "32004L0036", # 2004/36/EC	real title is CE --> for sfs.2010.770	sentence 98,, 394, 406 and sfs.1986.171	sentence 533 and 105 it is celex (32004L0036) that title with ce
                               TRUE ~ celex.dir))%>%
  mutate(celex.dir = case_when(doc.id == "sfs-1999-1229" & sentence_id == 2908 & code == "(EU) 2016/1164" ~ "32016L1164", 
                               TRUE ~ celex.dir))


  

check1 <-
  EU.links7 %>%
  filter(type=="reg" & is.na(celex.reg))%>%
  mutate(code = as.factor(code)) 

unique(check1$code)

EU.links8 <-
  EU.links7 %>%
  group_by(doc.id, element_id, sentence_id) %>%
  mutate( dup.first = duplicated(code, fromLast = TRUE),
          dup.last = duplicated(code, fromLast = FALSE) ) %>%
  mutate(delete = 
           case_when(  dup.first == "TRUE"& 
                       is.na(celex.dir)== "TRUE" & 
                       is.na(celex.dec)== "TRUE" & 
                       is.na(celex.rec)== "TRUE" & 
                       is.na(celex.reg)== "TRUE" | 
                        # OR
                       dup.last == "TRUE"  &
                       is.na(celex.dir)== "TRUE" & 
                       is.na(celex.dec)== "TRUE" & 
                       is.na(celex.rec)== "TRUE" & 
                       is.na(celex.reg)== "TRUE" ~ "YES",
                        # if not...
                       TRUE ~ "NO" 
                          ))   
  
EU.links9 <-
  EU.links8 %>%
  ungroup() %>%
  filter(delete != "YES")


check2 <-
  EU.links9  %>%
  filter(is.na(celex.dir) &
             is.na(celex.dec) &
             is.na(celex.rec) &
             is.na(celex.reg))

n_distinct(check2$code)

# These got deleted since idk if it is a typo or not...

# directives
#  89/106/EC  real title with EEC	--> not clear even in the sentences
# 2004/42/EC	real title is CE --> for sfs.2008.245	it is def the code with CE (32004L0042) but others not clear even in the sentences
# 2004/36/EC	real title is CE --> for sfs.2010.770	sentence 98,, 394, 406 and sfs.1986.171	sentence 533 and 105 it is celex (32004L0036) that title with ce
# 2004/35/EC	real title is CE --> not clear even in the sentences

# other notes 

#sfs-1980-789	-->(EC) No 2978/941	--> repealed leg. 
#sfs.1980.657	--> (EC) No 726/20048	 --> from the sentence also cannot tell if it is a typo




# chang from wide to long formate:
EU.links10 <- 
  EU.links9 %>%
  select(-dup.first,-dup.last,-delete) %>%
  pivot_longer(cols =  starts_with("celex"),
               names_to = "type2",
               values_to = "celex",
               values_drop_na = TRUE) %>% # here we are drioping those that are NA %>%
 filter(type2 != "celex.dec" | code != "2000/60/EC") %>% # filter this one out bc it is not referencing a dec it is a dir. 
 filter(celex != "32012R0684" | code != "(EU) No 648/2012") %>% # filter this one out bc it is not referencing a implementing reg. 
 filter(type2 != "celex.reg"  | code != "(EU) 2020/262") %>% # filter this one out bc it is not referencing a reg it is a dir. 
 filter(doc.id != "sfs-2009-400" | type2 != "celex.dec"  | code != "98/79/EC") %>% # all of these below were dir not dec
 filter(doc.id != "sfs.2009.400" | type2 != "celex.dec"  | code != "98/79/EC") %>%
 filter(doc.id != "sfs.1998.944" | type2 != "celex.dec"  | code != "98/79/EC")%>%
 filter(doc.id != "sfs.2009.641" | type2 != "celex.dec"  | code != "98/79/EC")%>%
 filter(doc.id != "sfs-2009-641" | type2 != "celex.dec"  | code != "98/79/EC") %>%
 filter(ref != "beslut" | code != "2004/27/EC") %>% # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "96/61/EC") %>% # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "96/50/EC") %>% # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "96/25/EC") %>% # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "direktiv" | code != "94/3/EC") %>% # filter this one out bc it is not referencing a dir it is a dec 
 filter(ref != "beslut" | code != "93/74/EEC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2013/40/EU") %>% # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2011/36/EU") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2009/18/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2008/105/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2006/70/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2005/60/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2004/28/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2011/62/EU") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2011/82/EU") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2001/20/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2003/87/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "genomförandebeslut" | code != "2003/96/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2007/43/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2008/119/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2008/120/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "98/58/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "96/93/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "96/23/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "93/42/EEC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2012/19/EU") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
 filter(ref != "beslut" | code != "2009/16/EC")   # filter this one out bc it is not referencing a dec it is a dir. 

#check if cele codes have mutiple resource types within a sentence...
check2 <-
  EU.links10  %>%
  group_by(doc.id,element_id,sentence_id,code) %>%
  summarise(n=n_distinct(celex)) %>%
  filter(n>1)

n_distinct(check2$code)

EU.links11 <-
  EU.links10 %>%
  mutate(doc.id = str_replace_all(doc.id, "-", "."))

# ok now we have to remove codes that are parts of othe celex titles that label which it is ammending...
#   mutate(amendtitle = str_extract_all(titles,"amending.+")) 
EU.links12 <-
  EU.links11 %>%
  mutate(amending.dir = str_extract(text, "ändring av direktiv [:digit:]+/[:digit:]+/[:alpha:]+|ändring av direktiv \\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)"),
         amending.dir = str_extract(amending.dir,"[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")) %>%
  mutate(repealing.dir = str_extract(text, "upphävande av direktiv [:digit:]+/[:digit:]+/[:alpha:]+|upphävande av direktiv \\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)"),
         repealing.dir = str_extract(repealing.dir,"[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)"))%>%
  mutate(amending.dir = str_trim(amending.dir, side = c("both")))%>%
  mutate(amending.dir = str_replace_all(amending.dir, "EG", "EC"), # change from SE to EN language 
         amending.dir = str_replace_all(amending.dir, "nr", "No"),
         amending.dir = str_replace_all(amending.dir, "RIF", "JHA"))%>%
  mutate(repealing.dir = str_replace_all(repealing.dir, "EG", "EC"), # change from SE to EN language 
         repealing.dir = str_replace_all(repealing.dir, "nr", "No"),
         repealing.dir = str_replace_all(repealing.dir, "RIF", "JHA")) %>%
  mutate(repealing.dir = str_trim(repealing.dir, side = c("both")))%>%
  mutate(delete = 
           case_when(  amending.dir == code | 
                         # OR
                         repealing.dir == code ~ "YES",
                       # if not...
                       TRUE ~ "NO" 
           ))   

#  mutate(amending.dir = str_extract(text, "ändring[\\s\\S]*")),
# amending.dir = str_extract(amending.dir,"[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")) 


EU.links13 <-
  EU.links12 %>%
  mutate(amending.reg = str_extract(text, "ändring av förordning [:digit:]+/[:digit:]+/[:alpha:]+|ändring av förordning \\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|ändring av förordning \\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|ändring av förordning \\([:alpha:]+\\,\\s[:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|ändring av förordning \\([:alpha:]+\\,\\s[:alpha:]+,\\s[:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|ändring av förordning EEG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|ändring av förordning EG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|ändring av förordning nr\\s[:digit:]+/[:digit:]+/[:alpha:]+|ändring av förordning\\snr\\s[:digit:]+(?=\\s)"),
         amending.reg = str_extract(amending.reg,"[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\,\\s[:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\,\\s[:alpha:]+,\\s[:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|EEG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|EG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|nr\\s[:digit:]+/[:digit:]+/[:alpha:]+|Regulation\\snr\\s[:digit:]+(?=\\s)")) %>%
  mutate(repealing.reg = str_extract(text, "upphävande av förordning [:digit:]+/[:digit:]+/[:alpha:]+|upphävande av förordning \\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|upphävande av förordning \\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|upphävande av förordning \\([:alpha:]+\\,\\s[:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|upphävande av förordning \\([:alpha:]+\\,\\s[:alpha:]+,\\s[:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|upphävande av förordning EEG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|upphävande av förordning EG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|upphävande av förordning nr\\s[:digit:]+/[:digit:]+/[:alpha:]+|upphävande av förordning\\snr\\s[:digit:]+(?=\\s)"),
         repealing.reg = str_extract(repealing.reg,"[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\,\\s[:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\,\\s[:alpha:]+,\\s[:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|EEG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|EG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|nr\\s[:digit:]+/[:digit:]+/[:alpha:]+|Regulation\\snr\\s[:digit:]+(?=\\s)"))%>%
  mutate(amending.reg = str_trim(amending.reg, side = c("both")))%>%
  mutate(amending.reg = str_replace_all(amending.reg, "EG", "EC"), # change from SE to EN language 
         amending.reg = str_replace_all(amending.reg, "nr", "No"),
         amending.reg = str_replace_all(amending.reg, "RIF", "JHA"))%>%
  mutate(repealing.reg = str_replace_all(repealing.reg, "EG", "EC"), # change from SE to EN language 
         repealing.reg = str_replace_all(repealing.reg, "nr", "No"),
         repealing.reg = str_replace_all(repealing.reg, "RIF", "JHA")) %>%
  mutate(repealing.reg = str_trim(repealing.reg, side = c("both")))%>%
  mutate(delete2 = 
           case_when(  repealing.reg == code | 
                         # OR
                         repealing.reg == code ~ "YES",
                       # if not...
                       TRUE ~ "NO" 
           ))   


EU.links14 <-
  EU.links13 %>%
  mutate(amending.dec = str_extract(text, "ändring av beslut [:digit:]+/[:digit:]+/[:alpha:]+|ändring av beslut \\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)"),
         amending.dec = str_extract(amending.dec,"[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")) %>%
  mutate(repealing.dec = str_extract(text, "upphävande av beslut [:digit:]+/[:digit:]+/[:alpha:]+|upphävande av beslut \\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)"),
         repealing.dec = str_extract(repealing.dec,"[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)"))%>%
  mutate(amending.dec = str_trim(amending.dec, side = c("both")))%>%
  mutate(amending.dec = str_replace_all(amending.dec, "EG", "EC"), # change from SE to EN language 
         amending.dec = str_replace_all(amending.dec, "nr", "No"),
         amending.dec = str_replace_all(amending.dec, "RIF", "JHA"))%>%
  mutate(repealing.dec = str_replace_all(repealing.dec, "EG", "EC"), # change from SE to EN language 
         repealing.dec = str_replace_all(repealing.dec, "nr", "No"),
         repealing.dec = str_replace_all(repealing.dec, "RIF", "JHA")) %>%
  mutate(repealing.dec = str_trim(repealing.dec, side = c("both")))%>%
  mutate(delete3 = 
           case_when(  amending.dec == code | 
                         # OR
                         repealing.dec == code ~ "YES",
                       # if not...
                       TRUE ~ "NO" 
           ))   
  
EU.links15 <- 
  EU.links14 %>%
  
  
EU.links16 <- 
  EU.links15 %>%
  filter(delete != "YES")%>%
  filter(delete2 != "YES")%>%
  filter(delete3 != "YES")
  
  
EU.links16 <- 
  EU.links14 %>%





## final df

SEtext.dk2 <-
  SEtext.dk1 %>%
  mutate(doc.id = str_replace_all(doc.id, "-", ".")) %>%
  select(-text, -search.term, -country)

n_distinct(EU.links15$doc.id) # 126 SE documents are linked to an EU legislation
n_distinct(EU.links15$celex) # 560 EU legislation is linked
unique(EU.links15$type)

# which keywords link to which EU documents:
SE.EU.links <- 
  EU.links15 %>%
  ungroup() %>%
  select(-text, -element_id, -sentence_id) %>%
  distinct(doc.id,celex, .keep_all = TRUE) %>%
  left_join(.,document.key.df, by = c("celex")) %>%
  left_join(.,SEtext.dk2, by = c("doc.id"))


n_distinct(SE.EU.links$doc.id) # 125 SE documents are linked to an EU legislation
n_distinct(SE.EU.links$celex) # 555 EU legislation is linked

write.csv(SE.EU.links, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02SE.EU.links.csv", row.names=FALSE)


# archival -----------------------



# ok now lets get the EU legislation key and extract titles to link to the reference codes: -------------------------------

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


