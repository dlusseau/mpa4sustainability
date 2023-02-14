
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
library("sentimentr")
library("eurlex")
library("purrr")
library("tidyverse")
library("readxl")
library("dplyr")

# Load data --------------------------------------------------------------------

# list of result text
sjofart.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.sjofart.list" )
jakttext.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.jakttext.list" )
fisketext.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.fisketext.list" )

# document metadata
sjofart.metadata <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.sjofart.df.csv")
jakt.metadata <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.jakt.df.csv")
fisk.metadata <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.fisk.df.csv")

# making into a nice df --------------------------------------------------------

sjofarttext.df <- 
  as.data.frame(unique(cbind(sjofart.list))) %>% # need to use unique () because some of the results were duplicates 
  rownames_to_column(., var = "doc.id") %>%
  unnest(sjofart.list, keep_empty=TRUE) %>%
  mutate(search.term = "sjofart") %>%
  mutate(country = "SE") %>%
  dplyr::rename("text" = "sjofart.list")


jakttext.df <- 
  as.data.frame(unique(cbind(jakttext.list))) %>% # need to use unique () because some of the results were duplicates
  rownames_to_column(., var = "doc.id") %>%
  unnest(jakttext.list, keep_empty=TRUE) %>%
  mutate(search.term = "jakt") %>%
  mutate(country = "SE") %>%
  dplyr::rename("text" = "jakttext.list")

fisketext.df <- 
  as.data.frame(unique(cbind(fisketext.list))) %>% # need to use unique () because some of the results were duplicates
  rownames_to_column(., var = "doc.id") %>%
  unnest(fisketext.list, keep_empty=TRUE) %>%
  mutate(search.term = "fiske") %>%
  mutate(country = "SE") %>%
  dplyr::rename("text" = "fisketext.list")

SEtext.dk <- rbind(sjofarttext.df,jakttext.df,fisketext.df)

# now combine the meta data: 

sjofart.metadata <-
  sjofart.metadata  %>%
  distinct(id, .keep_all=TRUE)%>%
  mutate(search.term = "sjofart")

fisk.metadata <-
  fisk.metadata  %>%
  distinct(id, .keep_all=TRUE)%>%
  mutate(search.term = "fiske")

jakt.metadata <-
  jakt.metadata  %>%
  distinct(id, .keep_all=TRUE)%>%
  mutate(search.term = "jakt")

SEmeta.dk <- 
  rbind(sjofart.metadata,jakt.metadata,fisk.metadata)

n_distinct(SEtext.dk$doc.id) # 223
n_distinct(SEmeta.dk$id)
# unique ids there are 223 documents

#looking for the other forms of related words in the texts
SEtext.dk1 <-
  SEtext.dk %>%
  mutate(harpun = case_when(search.term == "fiske" ~ str_detect(text, "harpun|Harpun")), #stringr is case sensitive so make sure to have both :)
         harpun2 = case_when(search.term == "fiske" ~ str_detect(text, "undervattensjakt|Undervattensjakt")),
         commercial = case_when(search.term == "fiske" ~ str_detect(text, "yrkesfisk|Yrkesfisk")),
         recreational = case_when(search.term == "fiske" ~ str_detect(text, "fritidsfisk|Fritidsfisk")), 
         angling = case_when(search.term == "fiske" ~ str_detect(text, "handredskapsfisk|Handredskapsfisk")), 
         angling2    = case_when(search.term == "fiske" ~ str_detect(text, "spöfisk|Spöfisk")),
         houseneeds.fishing = case_when(search.term == "fiske" ~ str_detect(text, "husbehovsfisk|Husbehovsfisk")), 
         houseneeds.fishing2 = case_when(search.term == "fiske" ~ str_detect(text, "fiske för husbehov|Fiske för husbehov")),
         birdhunt = case_when(search.term == "jakt" ~ str_detect(text, "fågel|Fågel")),
         seal = case_when(search.term == "jakt" ~ str_detect(text, "säljakt|Säljakt")),
         seal2 = case_when(search.term == "jakt" ~ str_detect(text, "säl\\s|Säl\\s")),
         seal3 = case_when(search.term == "jakt" ~ str_detect(text, "\\ssäl\\s|\\sSäl\\s")),
         seal4 = case_when(search.term == "jakt" ~ str_detect(text, "\\ssälar\\s|\\sSälar\\s")),
         boat.traffic = case_when(search.term == "sjofart" ~ str_detect(text, "båttrafik|Båttrafik")))

head(SEtext.dk1)
unique(SEtext.dk1$search.term)

SEtext.dk1 %>%
  group_by(search.term) %>%
  summarise(n=n_distinct(doc.id))

# fiske         105
# jakt           35
# sjofart       125

SEtext.dk1 %>%
  summarise(n=n_distinct(doc.id))
#223

SEtext.dk1 %>%
  filter(search.term == "fiske" & harpun == "TRUE") # 0 mention harpun

SEtext.dk1 %>%
  filter(search.term == "fiske" & harpun2 == "TRUE") # 0 mention harpun

SEtext.dk1 %>%
  filter(search.term == "fiske" & commercial == "TRUE") # 12 fisheries documents mention yrkesfisk

SEtext.dk1 %>%
  filter(search.term == "fiske" & recreational == "TRUE") # 3 fisheries documents mention fritidsfisk fisk

SEtext.dk1 %>%
  filter(search.term == "fiske" & angling == "TRUE") # 1 mention handredskapsfisk

SEtext.dk1 %>%
  filter(search.term == "fiske" & angling2 == "TRUE") # 1  documents mention spöfisk

SEtext.dk1 %>%
  filter(search.term == "fiske" & houseneeds.fishing == "TRUE") # 0  documents mention husbehovsfisk

SEtext.dk1 %>%
  filter(search.term == "fiske" & houseneeds.fishing2 == "TRUE") # 0  documents mention fiske för husbehov

SEtext.dk1 %>%
  filter(search.term == "jakt" & birdhunt == "TRUE") # 3 fågel

SEtext.dk1 %>%
  filter(search.term == "jakt" & seal == "TRUE") # 1 mention säl 

SEtext.dk1 %>%
  filter(search.term == "jakt" & seal2 == "TRUE") # 3 mention säl 

SEtext.dk1 %>%
  filter(search.term == "jakt" & seal3 == "TRUE") # 2 mention säl 

SEtext.dk1 %>%
  filter(search.term == "jakt" & seal4 == "TRUE") # 1 mention säl 


SEtext.dk1 %>%
  filter(search.term == "sjofart" & boat.traffic == "TRUE") # 0 mention båtstrafik

SEtext.dk1 %>%
  write.csv(., file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.SEdoctags.csv", row.names=FALSE)

SEtext.dk1 %>%
  select(-text) %>%
  #mutate(doc.id=as.factor(doc.id)) %>%
  left_join(.,SEmeta.dk, by = c("doc.id"="dok_id", "search.term") ) %>%
  write.csv(., file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.SEdocmetadata.clean.csv", row.names=FALSE)




# Getting Eurlex links ---------------------------------------------------------

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
  filter(!is.na(ref)) # remove na values (sentences that dont reference a legislation type)

# then lets pull all the codes in those sentences...

EU.links3 <-
  EU.links2 %>%
  mutate(code = case_when(ref == "direktiv" ~  as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")),
                          ref == "Direktiv" ~  as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")),
                          ref == "förordning" ~ as.character(str_extract_all(text, "\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|EEG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|EG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|nr\\s[:digit:]+/[:digit:]+/[:alpha:]+|förordning\\snr\\s[:digit:]+(?=\\s)")),
                          ref == "Förordning" ~ as.character(str_extract_all(text, "\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)|EEG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|EG+\\snr\\s[:digit:]+/[:digit:]+(?!/)|nr\\s[:digit:]+/[:digit:]+/[:alpha:]+|förordning\\snr\\s[:digit:]+(?=\\s)")),
                          ref == "beslut"             ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|[:digit:]+/[:digit:]+/[:digit:]+")),
                          ref == "genomförandebeslut" ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|[:digit:]+/[:digit:]+/[:digit:]+")),
                          ref == "Beslut"             ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|[:digit:]+/[:digit:]+/[:digit:]+")),
                          ref == "Genomförandebeslut" ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|[:digit:]+/[:digit:]+/[:digit:]+")),
                          ref == "rekommendation" ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+")),
                          ref == "Rekommendation" ~   as.character(str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+")))) %>%
  # if it is a Character vector string edit it to just be the codes to be cleaner
  mutate(code = str_replace_all(code,"c\\(",""), 
         code = str_replace_all(code,"(?<!G|0|U)\\)",""),
         code = str_replace_all(code,'\\"',"")) %>%
  mutate(code = na_if(code,"character(0)")) %>% # change these values (none of the codes were detected) to NA
  filter(!is.na(code)) %>% # remove the NAs
  mutate(code = strsplit(code, ",")) %>%
  unnest(code) %>%
  mutate(code = str_trim(code, side = c("both")))%>%
  mutate(code = str_replace_all(code, "EG", "EC"), # change from SE to EN language so can match celex and title key that is in english
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


# directives: title key from another r script (1.5_EurlextitlekeyforSEdata.R)
directive.titles <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EurLexKey.directive.titles.csv") 


directive.titles.cut <-
  directive.titles %>%
  select(-X) %>% 
  left_join(.,document.dir.key.df1, by = c("work")) %>%
  mutate(title2 = str_trunc(titles,75,side = c("right"))) %>%
  mutate(code =  str_extract(titles, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")) %>%
  mutate(type = "dir")  

directive.titles1 <-
  directive.titles.cut%>%
  select(celex,type,code) # those with NA just dont have a title code within the title...


directive.removalcodes <-
  directive.titles.cut %>%
  select(-title2) %>%
  mutate(title.woCode =  str_remove(.$titles, .$code)) %>%
  mutate(codes.to.remove =  str_extract_all(title.woCode, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EEC+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EC+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|No\\s[:digit:]+/[:digit:]+/[:alpha:]+|regulation\\sNo\\s[:digit:]+(?=\\s)|[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|[:digit:]+/[:digit:]+/[:digit:]+")) %>%
  unnest(codes.to.remove) %>%
  select(celex,code,codes.to.remove,type)
  
EU.links4 <-
  EU.links3 %>%
  left_join(., directive.titles1, by = c("type", "code")) %>%
  rename(celex.dir = celex)


# decisions: title key from another r script:

decision.titles <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03EurLexKey.decision.titles.csv") 

decision.titles.cut <-
  decision.titles %>%
  mutate(title2 = str_trunc(titles,75,side = c("right"))) %>%
  mutate(code =  str_extract(title2, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|[:digit:]+/[:digit:]+/[:digit:]+")) %>%
  mutate(type = "dec") 

decision.titles1 <-
  decision.titles.cut  %>%
  select(celex,type,code) 
  
decision.removalcodes <-
  decision.titles.cut %>%
  select(-title2) %>%
  mutate(title.woCode =  str_remove(.$titles, .$code)) %>%
  mutate(codes.to.remove =  str_extract_all(title.woCode, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EEC+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EC+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|No\\s[:digit:]+/[:digit:]+/[:alpha:]+|regulation\\sNo\\s[:digit:]+(?=\\s)|[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|[:digit:]+/[:digit:]+/[:digit:]+")) %>%
  unnest(codes.to.remove) %>%
  select(celex,code,codes.to.remove,type)

EU.links5 <-
  EU.links4 %>%
  left_join(., decision.titles1, by = c("type", "code")) %>%
  rename(celex.dec = celex)%>%
  mutate(celex.dir = as.character(celex.dir))

# reccomendations: : title key from another r script:

reccomendation.titles <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03EurlLexKey.reccomendations.titles.csv") 


document.rec.key.df1 <- 
  document.key.df %>% 
  select(resource.type, work, celex) %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(resource.type == "RECO") %>%
  select(work,celex)

reccomendation.titles.cut <-
  reccomendation.titles %>%
  left_join(.,document.rec.key.df1, by = c("work")) %>%
  mutate(title2 = str_trunc(titles,75,side = c("right"))) %>%
  mutate(code =  str_extract(title2, "[:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  mutate(type = "rec") 

reccomendation.titles1 <-
  reccomendation.titles.cut  %>%
  select(celex,type,code) 

reccomendation.removalcodes <-
  reccomendation.titles.cut %>%
  select(-title2) %>%
  mutate(title.woCode =  str_remove(.$titles, .$code)) %>%
  mutate(codes.to.remove =  str_extract_all(title.woCode, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EEC+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EC+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|No\\s[:digit:]+/[:digit:]+/[:alpha:]+|regulation\\sNo\\s[:digit:]+(?=\\s)|[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|[:digit:]+/[:digit:]+/[:digit:]+")) %>%
  unnest(codes.to.remove) %>%
  select(celex,code,codes.to.remove,type)


EU.links6 <-
  EU.links5 %>%
  left_join(., reccomendation.titles1, by = c("type", "code")) %>%
  rename(celex.rec = celex)

#load the data from Harvard that has all Eurlex info up until 2019
regulation.titles <-  read_excel("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/EurLex_regulations_no_text_all.xlsx") 
#load the years that are not in harvard data
regulation.titles.yr2020.2022 <- 
  read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03EurlLexKey.regulation.titles.yr2020.2022.csv") %>%
  select(-work) 

regulation.titles.cut <-
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
  mutate(title7 = str_replace(title6,"\\(EEC ", "\\(EEC\\)")) %>% # cleanest full title!
  mutate(title8 = str_trunc(title7,75,side = c("right")),
         title9 = str_replace_all(title8,"\\(\\sEEC\\s\\)", "\\(EEC\\)")) %>%
  mutate(code =  str_extract(title9, "\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EEC+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EC+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|No\\s[:digit:]+/[:digit:]+/[:alpha:]+|regulation\\sNo\\s[:digit:]+(?=\\s)")) %>%
    mutate(type = "reg")

regulation.titles1 <-
  regulation.titles.cut  %>%
  select(celex,type,code) 


regulation.removalcodes <-
  regulation.titles.cut %>%
  select(title7,code,type,celex) %>%
  mutate(titles.woCode =  str_remove(.$title7, .$code)) %>%
  mutate(codes.to.remove =  str_extract_all(titles.woCode, "[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|\\([:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EEC+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|EC+\\sNo\\s[:digit:]+/[:digit:]+(?!/)|No\\s[:digit:]+/[:digit:]+/[:alpha:]+|regulation\\sNo\\s[:digit:]+(?=\\s)|[:digit:]+/[:digit:]+/[:alpha:]+|\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)|[:digit:]+/[:digit:]+/[:digit:]+")) %>%
  unnest(codes.to.remove) %>%
  select(celex,code,codes.to.remove,type)


Removal.code.list <-
  rbind(directive.removalcodes,decision.removalcodes,regulation.removalcodes,reccomendation.removalcodes)
#the str_remove is not working for the all the dfs... no idea why... so I will also make a case when title code = remove code then remove from df...

Removal.code.list.2 <-
  Removal.code.list %>%
  mutate(delete = 
           case_when(  code == codes.to.remove ~ "YES",
                       # if not...
                       TRUE ~ "NO" 
           )) %>%
  filter(delete=="NO") %>%
  select(-code,-delete,-type) %>%
  filter(!is.na(codes.to.remove))

Removal.code.list.3 <-
  Removal.code.list.2 %>%
  distinct() %>%
  mutate(celex = as.character(celex)) # here we have a row for every "bad" code within a celex title code 

EU.links7 <-
  EU.links6 %>%
  distinct() %>% # remove those that are mentioned exactly the same multiple times in a sentence
  left_join(., regulation.titles1, by = c("type", "code")) %>%
  rename(celex.reg = celex) %>%
  mutate(ref = as.character(ref))

check1 <-
  EU.links7 %>%
  filter(type=="reg" & is.na(celex.reg))%>%
  mutate(code = as.factor(code)) 
unique(check1$code)


EU.links7 <-
  EU.links6 %>%
  distinct() %>% # remove those that are mentioned exactly the same multiple times in a sentence
  left_join(., regulation.titles1, by = c("type", "code")) %>%
  rename(celex.reg = celex) %>%
  mutate(ref = as.character(ref)) %>%
  mutate(celex.reg = case_when(type == "reg" & code == "(EC) 1907/2006" ~	"32006R1907",# 
                               type == "reg" & code == "(EC) No 1907/2006" ~ "32006R1907",
                               type == "reg" & code == "(EC) No 1966/2006" ~ "32006R1966",
                               type == "reg" & code == "(EEC) 2658/87" ~	"31987R2658",
                               type == "reg" & code == "(EEC) 793/93" ~	"31993R0793",
                               type == "reg" & code == "(EU) 1380/2013" ~	"32013R1380",# 
                               type == "reg" & code == "(EU) No 2016/424" ~	"32016R0424",
                               type == "reg" & code == "(EU) No 2019/1021" ~	"32019R1021",
                               type == "reg" & code == "(EU) 2021/1139" ~	"32021R1139",
                               type == "reg" & code == "(EU) 2019/1896" ~	"32019R1896",
                               type == "reg" & code == "(EU) 2019/1603" ~	"32019R1603",
                               type == "reg" & code == "(EU) No 868/2014" ~	"32014R0868",
                               type == "reg" & code == "(EU) 2019/2033" ~	"32019R2033",
                               type == "reg" & code == "(EU) 2021/836" ~	"32021R0836",
                               type == "reg" & code == "(EU) 297/2008" ~	"32008R0297",# 
                               type == "reg" & code == "(EU) No 2016/425" ~	"32016R0425",# 
                               type == "reg" & code == "(EU) No 2019/1241" ~	"32019R1241",
                               type == "reg" & code == "(EU) No 2019/1248" ~	"32019R1248",
                               TRUE ~ celex.reg
                               ))%>%
                                 mutate(celex.reg = case_when(doc.id == "sfs-1980-789" & sentence_id == 505 & code == "(EC) No 2978/941" ~ "31994R2978", # 2004/36/EC	real title is CE --> for sfs.2010.770	sentence 98,, 394, 406 and sfs.1986.171	sentence 533 and 105 it is celex (32004L0036) that title with ce
                                                              TRUE ~ celex.reg))%>%
                                 mutate(celex.reg = case_when(doc.id == "sfs-2022-1461" & sentence_id == 36 & code == "(EU) 1408/2013" ~ "32013R1408", # 2004/36/EC	real title is CE --> for sfs.2010.770	sentence 98,, 394, 406 and sfs.1986.171	sentence 533 and 105 it is celex (32004L0036) that title with ce
                                                              TRUE ~ celex.reg)) %>% 
  filter(doc.id != "sfs-2022-1718" | ref != "förordning" | code != "(EU) 2015/413") %>% # this is a directive but didnt follow the directive coding but got the code for the directive so remove it
  filter(doc.id != "sfs-2022-1718" | ref != "beslut" | code != "(EU) 2015/413") %>% # this is a directive but didnt follow the directive coding but got the code for the directive so remove it
  filter(doc.id != "sfs-2014-1434" | ref != "förordning" | code != "(EU) 2015/652") %>% # this is a directive but didnt follow the directive coding but got the code for the directive so remove it
  filter(doc.id != "sfs-2022-1718" | ref != "förordning" | code != "(EU) 2015/849") %>% # this is a directive but didnt follow the directive coding but got the code for the directive so remove it
  filter(doc.id != "sfs-2022-1718" | ref != "beslut" | code != "(EU) 2015/849") %>%
  filter(doc.id != "sfs-2009-400" | ref != "förordning" | code != "(EU) 2017/1132") %>% # this is a directive but didnt follow the directive coding but got the code for the directive so remove it
  filter(doc.id != "sfs-1994-1776" | ref != "förordning" | code != "(EU) 2018/552") %>%
  filter(doc.id != "sfs-1994-1776" | ref != "direktiv" | code != "(EU) 2018/552") %>%
  filter(doc.id != "sfs-2020-614" | ref != "förordning" | code != "(EU) 2018/851") %>%
  filter(doc.id != "sfs-2020-614" | ref != "beslut" | code != "(EU) 2018/851") %>%
  filter(doc.id != "sfs-1994-1776" | ref != "förordning" | code != "(EU) 2020/262") %>%
  filter(code != "No 529/2013/EU") %>% # No 529/2013/EU	 these are decisions but dec pulled it without No AND IS CORRECT so remove row when type ==reg
  filter(code != "No 1313/2013/EU") %>% # SAME REASONING AS THE ROW ABOVE
   filter(code != "förordning No 187") %>% # förordning No 187	 not EU regulation says "royal regulation No 187"
  filter(doc.id != "sfs-2011-1533" | ref != "Förordning" | code != "(EU) 2017/2397") %>% # filter this one out bc it is a directive not a regulation
  filter(doc.id != "sfs-2011-1533" | ref != "beslut" | code != "(EU) 2017/2397") %>% # filter this one out bc it is a directive not a regulation
  filter(doc.id != "sfs-2022-1718" | ref != "förordning" | code != "(EU) 2018/843") %>% # filter this one out bc it is a directive not a regulation
  filter(doc.id != "sfs-2022-1718" | ref != "beslut" | code != "(EU) 2018/843") %>% # filter this one out bc it is a directive not a regulation
  filter(doc.id != "sfs-2019-84" | ref != "förordning" | code != "(EU) 2019/420")  %>% # filter this one out bc it is a directive not a regulation
  mutate(celex.reg = case_when(doc.id == "sfs-2022-1461" & sentence_id == 13 & code == "(EU) 2021/2115" ~ "32021R2115", # 
                               TRUE ~ celex.reg))
  


check1 <-
  EU.links7 %>%
  filter(type=="reg" & is.na(celex.reg))%>%
  mutate(code = as.factor(code)) 
unique(check1$code)

# Now if there are duplicate codes but one doesnot have a celex but the other does remove the one without since this is a mistake

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

n_distinct(check2$code) #6
n_distinct(check2$doc.id)#11

EU.links9 <- 
  EU.links9  %>%
mutate(celex.dec = case_when(type == "dec" & code == "2009/371/JHA" ~ "32009D0371", #  this offcical title didnt have its code so missing from the key and did not come out of the eurlex pull
                             TRUE ~ celex.dec)) %>%
mutate(celex.dec = case_when(type == "dir" & code == "2004/27/EC" ~ "32004L0027", #  this was mising from the title key pull dont know why
                             TRUE ~ celex.dec)) %>%
filter(doc.id != "sfs-2015-315" | ref != "beslut" | code != "2004/27/EC")  %>% # this is not a decision it is a directive reference the line above
  mutate(celex.rec = case_when(type == "rec" & code == "2003/361/EC" ~ "32003H0361", # all recos are done!
                               TRUE ~ celex.rec))
check2 <-
  EU.links9  %>%
  filter(is.na(celex.dir) &
           is.na(celex.dec) &
           is.na(celex.rec) &
           is.na(celex.reg))

n_distinct(check2$code) #3
n_distinct(check2$doc.id)#8

# These got deleted since idk if it is a typo or not...
# directives
#  89/106/EC  real title with EEC	--> not clear even in the sentences
# 2004/42/EC	real title is CE --> for sfs.2008.245	it is def the code with CE (32004L0042) but others not clear even in the sentences
# 2004/35/EC	real title is CE --> not clear even in the sentences

EU.links10 <- 
  EU.links9 %>%
  select(-dup.first,-dup.last,-delete) %>%
  pivot_longer(cols =  starts_with("celex"),
               names_to = "type2",
               values_to = "celex",
               values_drop_na = TRUE)

#check if cele codes have mutiple resource types within a sentence...
check2 <-
  EU.links10  %>%
  group_by(doc.id,element_id,sentence_id,code) %>%
  summarise(n=n_distinct(celex)) %>%
  filter(n>1)

n_distinct(check2$code)

# chang from wide to long formate:
EU.links10 <- 
  EU.links9 %>%
  select(-dup.first,-dup.last,-delete) %>%
  pivot_longer(cols =  starts_with("celex"),
               names_to = "type2",
               values_to = "celex",
               values_drop_na = TRUE) %>% # here we are drioping those that are NA %>%
  filter(ref != "beslut" | code != "2009/18/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2007/43/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2008/119/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2008/120/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "direktiv" | code != "90/425/EEC"| celex != "31991L0628") %>%
  filter(ref != "beslut" | code != "96/23/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "96/93/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "98/58/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "genomförandebeslut" | code != "2003/96/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(doc.id != "sfs-2009-400" | type2 != "celex.dec"  | code != "98/79/EC") %>% # all of these below were dir not dec
  filter(doc.id != "sfs.2009.400" | type2 != "celex.dec"  | code != "98/79/EC") %>%
  filter(doc.id != "sfs.1998.944" | type2 != "celex.dec"  | code != "98/79/EC")%>%
  filter(doc.id != "sfs-1998-944" | type2 != "celex.dec"  | code != "98/79/EC")%>%
  filter(doc.id != "sfs.2009.641" | type2 != "celex.dec"  | code != "98/79/EC")%>%
  filter(doc.id != "sfs-2009-641" | type2 != "celex.dec"  | code != "98/79/EC")%>%
  filter(celex != "32012R0684" | code != "(EU) No 648/2012") %>% # filter this one out bc it is not referencing a implementing reg. 
  filter(type2 != "celex.dec" | code != "2000/60/EC") %>% # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2001/20/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2003/87/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "93/42/EEC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2004/28/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2005/60/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2006/70/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(doc.id != "sfs-2010-1770" | ref != "genomförandebeslut"  | code != "2007/2/EC")%>%
  filter(ref != "beslut" | code != "2008/105/EC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2009/16/EC") %>%   # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2011/36/EU") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2012/19/EU") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "96/50/EC") %>% # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "96/25/EC") %>% # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "direktiv" | code != "94/3/EC") %>% # filter this one out bc it is not referencing a dir it is a dec 
  filter(ref != "beslut" | code != "93/74/EEC") %>%  # filter this one out bc it is not referencing a dec it is a dir. 
  filter(ref != "beslut" | code != "2013/40/EU") # filter this one out bc it is not referencing a dec it is a dir. 
  
#check if cele codes have mutiple resource types within a sentence...
check2 <-
  EU.links10  %>%
  group_by(doc.id,element_id,sentence_id,code) %>%
  summarise(n=n_distinct(celex)) %>%
  filter(n>1)

n_distinct(check2$code)

EU.links11 <-
  EU.links10 

# ok now we have to remove codes that are parts of othe celex titles that label which it is ammending...
  
text1 <- 
  EU.links11 %>%
  mutate(doc.sentence.id = paste(doc.id,element_id, sep = "_")) # ok so if a bad code is in the same document then it is removed. We assume that if it references the amending or repealing legislation the "bad codes are not directly referenced anymore"

se.doc<-unique(text1$doc.sentence.id)

i=1
temp<-text1[text1$doc.sentence.id==se.doc[i],]

bad.celex<-temp$celex[which(temp$celex%in%Removal.code.list.3$celex)] #those are the "bad" celex codes in this text?
remove.temp<-unique(Removal.code.list.3$codes.to.remove[which(Removal.code.list.3$celex%in%bad.celex)]) #those are the associated removal codes

#text.clean<-temp[-which(temp$code%in%remove.temp),] #we find the codes that match the removal codes and remove those rows

#Anna's edits:
# ok if none of the bad codes were found it returns empty so made an if else statment to put in the loop

if(dim(temp[-which(temp$code%in%remove.temp),])[1]==0){
  text.clean <-temp
}else{
  text.clean<-temp[-which(temp$code%in%remove.temp),] #we find the codes that match the removal codes and remove those rows
}

#data frame initialised

for (i in 2:length(se.doc)) {
  
  temp<-text1[text1$doc.sentence.id==se.doc[i],]
  
  bad.celex<-temp$celex[which(temp$celex%in%Removal.code.list.3$celex)] #those are the "bad" celex codes in this text?
  remove.temp<-unique(Removal.code.list.3$codes.to.remove[which(Removal.code.list.3$celex%in%bad.celex)]) #those are the associated removal codes
  
  if(dim(temp[-which(temp$code%in%remove.temp),])[1]==0){
    text.clean <-rbind(text.clean,temp)
  }else{
    
    text.clean<-rbind(text.clean,temp[-which(temp$code%in%remove.temp),]) #we find the codes that match the removal codes and remove those rows
  }
  
}
  


## final df

cleantext.df2 <-
  text.clean %>%
  select(-text, -search.term, -country)

n_distinct(cleantext.df2$doc.id) # 92 SE documents are linked to an EU legislation
n_distinct(cleantext.df2$celex) # 301 EU legislation is linked
unique(cleantext.df2$type)

# which keywords link to which EU documents:
SE.EU.links <- 
  cleantext.df2 %>%
  ungroup() %>%
  select(-element_id, -sentence_id, -type2, -doc.sentence.id) %>%
  distinct(doc.id,celex, .keep_all = TRUE) %>%
  left_join(.,document.key.df, by = c("celex")) %>%
  left_join(.,SEtext.dk1, by = c("doc.id")) %>%
  select(-text)


n_distinct(SE.EU.links$doc.id) # 92 SE documents are linked to an EU legislation
n_distinct(SE.EU.links$celex) # 301 EU legislation is linked

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


