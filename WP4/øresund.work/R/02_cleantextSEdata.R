
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

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------

# list of result text
sjofart.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.sjofart.list" )
jakttext.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.jakttext.list" )
fisketext.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.fisketext.list" )

# making into a nice df --------------------------------------------------------

sjofarttext.df <- 
  as.data.frame(cbind(sjofart.list)) %>% 
  rownames_to_column(., var = "doc.id") %>%
  rename("text" = "sjofart.list") %>%
  unnest(text, keep_empty=TRUE) %>%
  mutate(search.term = "sjofart") %>%
  mutate(country = "SE") 

jakttext.df <- 
  as.data.frame(cbind(jakttext.list)) %>% 
  rownames_to_column(., var = "doc.id") %>%
  rename("text" = "jakttext.list") %>%
  unnest(text, keep_empty=TRUE) %>%
  mutate(search.term = "jakttrafik") %>%
  mutate(country = "SE") 

fisketext.df <- 
  as.data.frame(cbind(fisketext.list)) %>% 
  rownames_to_column(., var = "doc.id") %>%
  rename("text" = "fisketext.list") %>%
  unnest(text, keep_empty=TRUE) %>%
  mutate(search.term = "fiske") %>%
  mutate(country = "SE") 

SEtext.dk <- rbind(sjofarttext.df,jakttext.df,fisketext.df)

#https://eur-lex.europa.eu/content/help/eurlex-content/numbering-of-acts.html 
# they do put habitatdirektiv and fågeldirektiv ramdirektiv för vatten
# maybe they sight others
EU.links <- 
  SEtext.dk %>%
  get_sentences() %>%
  mutate(Dir.Reg.Rec.Dec =  str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+")) %>% #/#/# or will also pick up those under the code nr #/#/# just not the nr
  unnest(Dir.Reg.Rec.Dec,keep_empty = TRUE) %>%
  mutate(Reg.2 =  str_extract_all(text,"\\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)")) %>% #(EU or ECC) nr #/#
  unnest(Reg.2,keep_empty = TRUE) %>%
  mutate(Reg.3 =  str_extract_all(text,"\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")) %>% #(EU or ECC) #/# (this one i noticed but not explicitly said in the link above)
  unnest(Reg.3,keep_empty = TRUE) %>%
  mutate(Reg. =  str_extract_all(text, "förordning\\snr\\s[:digit:]+")) %>% # Regulation No 17
  unnest(Reg.,keep_empty = TRUE) %>%
  mutate(Reg.4 =  str_extract_all(text, "förordning\\s[:alpha:]+\\snr\\s[:digit:]+/[:digit:]+(?!/)")) %>% # förordning EEG nr 2658/87 
  unnest(Reg.4,keep_empty = TRUE)
  
  
# remove those that reference nothing
EU.links2 <- 
  EU.links %>%
  filter(!(is.na(Dir.Reg.Rec.Dec) &
           is.na(Reg.2) &
             is.na(Reg.3) &
             is.na(Reg.4)&
             is.na(Reg.)))

#Reg4 has no hits. 
unique(EU.links2$Dir.Reg.Rec.Dec)
unique(EU.links2$Reg.2)
unique(EU.links2$Reg.3)
unique(EU.links2$Reg.4)
unique(EU.links2$Reg.)

# lets remove it 
EU.links2 <-
  EU.links2 %>%
  select(-Reg.4)

# OK since it is written in swedish EG --> EC and EEG --> EEC and change nr to No. 
EU.links3 <- 
  EU.links2 %>%
  mutate(Dir.Reg.Rec.Dec = str_replace_all(Dir.Reg.Rec.Dec, "EG", "EC"),
         Reg.2 = str_replace_all(Reg.2, "EG", "EC"),
         Reg.3 = str_replace_all(Reg.3, "EG", "EC"),
         Reg.2 = str_replace_all(Reg.2, "nr", "No"))

#convert wide to long and remove duplicates with distinct function
EU.links4 <-
  EU.links3 %>%
  pivot_longer(.,
  cols = Dir.Reg.Rec.Dec:Reg.,
  names_to = "type",
  values_to = "ref") %>%
  distinct() %>%
  filter(!is.na(ref)) # remove na values


# ok now lets get the EU legislation key and extract titles to link to the reference codes: -------------------------------

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


# Example of how directives could be cited: 
# Utredaren ska även föreslå de författningsändringar som krävs för att säkerställa att kraven i artikel 6.2 och 6.3 i 
# rådets direktiv 92/43/EEG av den 21 maj 1992 om bevarande av livsmiljöer samt vilda djur och växter (art- och habitatdirektivet) tillämpas fullt ut på fiske i enlighet med Sveriges EU-rättsliga åtaganden.

# Decisions:
# kommissionens beslut 2005/909/EG.

# Reccomendations: 
# kommissionens rekommendation 2003/361/EG

# council regulations:
# rådets förordning EEG nr 2658/87 

