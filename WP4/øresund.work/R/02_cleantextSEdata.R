
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

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------

# list of result text
sjotrafik.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.sjotrafik.list" )
jakttext.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.jakttext.list" )
fisketext.list <- readRDS(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.fisketext.list" )

# making into a nice df --------------------------------------------------------

sjotrafiktext.df <- 
  as.data.frame(cbind(sjotrafik.list)) %>% 
  rownames_to_column(., var = "doc.id") %>%
  rename("text" = "sjotrafik.list") %>%
  unnest(text, keep_empty=TRUE) %>%
  mutate(search.term = "sjotrafik") %>%
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

SEtext.dk <- rbind(sjotrafiktext.df,jakttext.df,fisketext.df)

#https://eur-lex.europa.eu/content/help/eurlex-content/numbering-of-acts.html 
# they do put habitatdirektiv and fågeldirektiv ramdirektiv för vatten
# maybe they sight others
test <- 
  SEtext.dk %>%
  get_sentences() %>%
  mutate(Dir.Reg.Rec.Dec =  str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+")) %>% #/#/#
  unnest(Dir.Reg.Rec.Dec,keep_empty = TRUE) %>%
  mutate(Dec =  str_extract_all(text, "nr\\s[:digit:]+/[:digit:]+/[:alpha:]+")) %>% # nr #/#/#
  unnest(Dec,keep_empty = TRUE) %>%
  mutate(Reg.2 =  str_extract_all(text,"\\([:alpha:]+\\)\\snr\\s[:digit:]+/[:digit:]+(?!/)")) %>% #(EU or ECC) nr #/#
  unnest(Reg.2,keep_empty = TRUE) %>%
  mutate(Reg.3 =  str_extract_all(text,"\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+(?!/)")) %>% #(EU or ECC) #/# (this one i noticed but not explicitly said in the link above)
  unnest(Reg.3,keep_empty = TRUE) %>%
  mutate(Reg. =  str_extract_all(text, "förordning\\snr\\s[:digit:]+")) %>% # Regulation No 17
  unnest(Reg.,keep_empty = TRUE) %>%
  mutate(Reg.4 =  str_extract_all(text, "förordning\\s[:alpha:]+\\snr\\s[:digit:]+/[:digit:]+(?!/)")) %>% # förordning EEG nr 2658/87 
  unnest(Reg.4,keep_empty = TRUE)
  
  
# remove those that reference nothing
test2 <- 
  test %>%
  filter(!(is.na(Dir.Reg.Rec.Dec) &
           is.na(Dec) & 
           is.na(Reg.2) &
             is.na(Reg.3) &
             is.na(Reg.4)&
             is.na(Reg.)))

#Reg4 has no hits. 
unique(test2$Reg.4)

# lets remove it 
test2 <-
  test2 %>%
  select(-Reg.4)

# OK since it is written in swedish EG --> EC and EEG --> EEC and change nr to No. 
test3 <- 
  test2 %>%
  mutate(Dir.Reg.Rec.Dec = str_replace_all(Dir.Reg.Rec.Dec, "EG", "EC"),
         Dec = str_replace_all(Dec, "EG", "EC"),
         Reg.2 = str_replace_all(Reg.2, "EG", "EC"),
         Reg.3 = str_replace_all(Reg.3, "EG", "EC"),
         Dec = str_replace_all(Dec, "nr", "No"),
         Reg.2 = str_replace_all(Reg.2, "nr", "No"))

# all decisions are additioonally pulled in the Dir.Reg.Rec.Dec pull string so will also remove this column for simplicity sake. 
test4 <-
  test3 %>%
  select(-Dec)

#convert wide to long and remove duplicates with distinct function
test5 <-
  test4 %>%
  pivot_longer(.,
  cols = Dir.Reg.Rec.Dec:Reg.,
  names_to = "type",
  values_to = "cite") %>%
  distinct()

# distinct network edge list
edge.list <-
  test5 %>%
  distinct(doc.id, cite) %>%
  filter(!is.na(cite))


# Example of how directives could be cited: 
# Utredaren ska även föreslå de författningsändringar som krävs för att säkerställa att kraven i artikel 6.2 och 6.3 i 
# rådets direktiv 92/43/EEG av den 21 maj 1992 om bevarande av livsmiljöer samt vilda djur och växter (art- och habitatdirektivet) tillämpas fullt ut på fiske i enlighet med Sveriges EU-rättsliga åtaganden.

# Decisions:
# kommissionens beslut 2005/909/EG.

# Reccomendations: 
# kommissionens rekommendation 2003/361/EG

# council regulations:
# rådets förordning EEG nr 2658/87 

