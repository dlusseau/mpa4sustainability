
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("sentimentr")
library("tidyverse")
library("readr")
library("eurlex")
library("tibble")
library("dplyr")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------

#First query info:
Q1.edgelist <- read.csv(file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1firstordercit.edgelist.csv")
n_distinct(Q1.edgelist$from)

Q1.verticesdata <- read.csv(file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/Policy_Interactions/data/03.Q1firstordercit.verticesmetadata.csv")

mar.protected.text <- read_csv(file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/Policy_Interactions/data/01_CELEXmpa.text.data.csv",
                               locale = locale(encoding = "ISO-8859-1"),
                               show_col_types = FALSE)

# EU mpa designation term associated text data: 
MPA.DESG.text <- read.csv(file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/Policy_Interactions/data/01_mpaterms.text.data.dup.csv")

# EURLEX KEY
document.key.df <- read.csv(file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

document.key.df1 <- 
  document.key.df %>% 
  select(resource.type, celex) %>%
  mutate(celex = as.factor(celex)) %>%
  distinct(celex, .keep_all = TRUE)

#--------------------------------------------------------------------------------------
#----------------- This is the analysis on the first search query  --------------------
#------------------------------- "marine protected " ----------------------------------
#--------------------------------------------------------------------------------------

# we need to know the tiles of the citations:

Q1.network.citationtitles <- 
  Q1.verticesdata %>%
  filter(CELEX %in% Q1.edgelist$from) %>%
  select(CELEX) %>%
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX)) %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  select(CELEX,title) %>%
  mutate(CELEX = as.factor(CELEX))

Q1.network.citationtitles1 <- left_join(Q1.network.citationtitles, document.key.df1, by = c("CELEX"="celex"))
unique(Q1.network.citationtitles1$resource.type)

Q1.Citdecision.titles <- 
  Q1.network.citationtitles %>%
  mutate(title2 = str_trunc(title,100,side = c("right"))) %>%
  mutate(decision = str_detect(title2, "Decision")) %>%
  filter(decision == "TRUE") %>%
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

Q1.Citdirective.titles <- 
  Q1.network.citationtitles %>%
  mutate(title2 = str_trunc(title,40,side = c("right"))) %>%
  mutate(directive = str_detect(title2, "Directive")) %>%
  filter(directive == "TRUE") %>%
  mutate(title.Dir =  str_extract(title2, "Directive[:blank:][:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  mutate(title.Dir2 = str_extract(title2, "Directive[:blank:]\\(.+\\)[:blank:][:digit:]+/[:digit:]+")) %>%
  select(CELEX,title.Dir,title.Dir2) 

Q1.Citdirective.titles1 <-
  Q1.Citdirective.titles%>%
  pivot_longer(
    cols = title.Dir:title.Dir2,
    names_to = "str.type",
    values_to = "title",
    values_drop_na = TRUE
  ) %>%
  select(-str.type) %>%
  mutate(resource.type = "DIR")


Q1.Citreg.titles <- 
  Q1.network.citationtitles %>%
  mutate(title2 = str_trunc(title,50,side = c("right"))) %>%
  mutate(reg = str_detect(title2, "Regulation")) %>%
  filter(reg == "TRUE") %>%
  mutate(title.Reg =  str_extract(title2, "Regulation[:blank:]\\(.+\\)[:blank:]No[:blank:][:digit:]+/[:digit:]+")) %>%
  mutate(title.Reg2 =  str_extract(title2, "Regulation[:blank:]\\(.+\\)[:blank:][:digit:]+/[:digit:]+"))%>%
  select(CELEX,title.Reg,title.Reg2) 

Q1.Citreg.titles1 <-
  Q1.Citreg.titles %>%
  pivot_longer(
    cols = title.Reg:title.Reg2,
    names_to = "str.type",
    values_to = "title",
    values_drop_na = TRUE
  ) %>%
  select(-str.type) %>%
  mutate(resource.type = "REG")

citation.titles <- rbind(Q1.Citdecision.titles1,
                         Q1.Citdirective.titles1,
                         Q1.Citreg.titles1) %>%
  rename("citation.celex" = "CELEX")

# recommendations and opinions I will deal with later...

Q1.network.rec.titles <- 
  Q1.network.citationtitles %>%
  mutate(title2 = str_trunc(title,100,side = c("right"))) %>%
  mutate(rec = str_detect(title2, "Recommendation"))%>%
  mutate(rec2 = str_detect(title2, "recommendation")) %>%
  filter(rec == "TRUE" |
           rec2 == "TRUE") %>%
 # mutate(title.Rec =  str_extract(title2, "Regulation[:blank:]\\(.+\\)[:blank:]No[:blank:][:digit:]+/[:digit:]+")) %>%
#  mutate(title.Rec2 =  str_extract(title2, "Regulation[:blank:]\\(.+\\)[:blank:][:digit:]+/[:digit:]+"))

Q1.network.opin.titles <- 
  Q1.network.citationtitles %>%
  mutate(title2 = str_trunc(title,50,side = c("right"))) %>%
  mutate(opin = str_detect(title2, "Opinion"))%>%
  filter(opin == "TRUE" )

# we want to find the location sentence/paragraph of each citation for the network data

# ok so we only neet the text that is within the network: 
# we have the text from the 24 result documents but we need the citations text aswell 
Q1.network.text <- 
  Q1.verticesdata %>%
  filter(pulled.from == "eurlex.web" | 
         pulled.from == "both") %>%
  select(CELEX) %>%
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX)) %>%
  mutate(text = map_chr(url, elx_fetch_data, "text")) %>% 
  select(CELEX,text)

# Directives
Q1.network.Dir.sentences <- 
  Q1.network.text %>%
  get_sentences() %>%
  mutate(Ref.Dir =  str_extract_all(text, "Directive[:blank:][:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  unnest(Ref.Dir,keep_empty = TRUE) %>%
  mutate(Ref.Dir2 = str_extract_all(text, "Directive[:blank:]\\(.+\\)[:blank:][:digit:]+/[:digit:]+")) %>%
  unnest(Ref.Dir2,keep_empty = TRUE) %>%
  pivot_longer(
    cols = Ref.Dir:Ref.Dir2,
    names_to = "str.type",
    values_to = "title",
    values_drop_na = TRUE) %>%
  select(-str.type) %>%
  mutate(title = str_replace_all(title, "Directive","")) %>%
  mutate(title = str_replace_all(title, "\\(EU\\)",""))%>%
  mutate(title = str_trim(title, side = c("both")))
# ok lets match the citation title to the citation
directives.key <- 
  citation.titles %>%
  filter(resource.type == "DIR") %>%
  mutate(title = str_replace_all(title, "Directive","")) %>%
  mutate(title = str_replace_all(title, "\\(EU\\)","")) %>%
  mutate(title = str_trim(title, side = c("both"))) %>%
  right_join(.,Q1.network.Dir.sentences, by = c("title" = "title"))


# Regulation
Q1.network.Reg.sentences <- 
  Q1.network.text %>%
  get_sentences() %>%
  mutate(Ref.Reg =  str_extract_all(text,"Regulation\\s\\([:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+")) %>%
  unnest(Ref.Reg,keep_empty = TRUE) %>%
  mutate(Ref.Reg2 =  str_extract_all(text, "Regulation\\s\\([:alpha:]+\\)\\s[:digit:]+/[:digit:]+")) %>%
  unnest(Ref.Reg2, keep_empty = TRUE) %>%
  mutate(Ref.Reg3 =  str_extract_all(text, "Regulation\\s\\([:alpha:]+\\,\\s[:alpha:]+\\)\\s[:digit:]+/[:digit:]+")) %>%
  unnest(Ref.Reg3, keep_empty = TRUE) %>%
  mutate(Ref.Reg4 =  str_extract_all(text, "Regulation\\s\\([:alpha:]+\\,\\s[:alpha:]+\\)\\sNo\\s[:digit:]+/[:digit:]+")) %>%
  unnest(Ref.Reg4, keep_empty = TRUE) %>%
  pivot_longer(
    cols = Ref.Reg:Ref.Reg4,
    names_to = "str.type",
    values_to = "title",
    values_drop_na = TRUE) %>%
  select(-str.type) %>%
  mutate(title = str_extract_all(title, "[:digit:]+/[:digit:]+")) %>%
  mutate(title = str_trim(title, side = c("both")))
# ok lets match the citation title to the citation
regulation.key <- 
  citation.titles %>%
  filter(resource.type == "REG") %>%
  mutate(title = str_extract_all(title, "[:digit:]+/[:digit:]+")) %>%
  mutate(title = str_trim(title, side = c("both"))) %>%
  right_join(.,Q1.network.Reg.sentences, by = c("title" = "title"))

Q1.network.Dec.sentences <- 
  Q1.network.text %>%
  get_sentences() %>%
  mutate(Ref.Dec =  str_extract_all(text, "Decision[:blank:][:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  unnest(Ref.Dec,keep_empty = TRUE) %>%
  mutate(Ref.Dec2 =  str_extract_all(text, "Decision[:blank:]No[:blank:][:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  unnest(Ref.Dec2,keep_empty = TRUE) %>%
  mutate(Ref.Dec3 =  str_extract_all(text, "European Union Offshore Oil and Gas")) %>%
  unnest(Ref.Dec3,keep_empty = TRUE) %>%
  mutate(Ref.Dec4 =  str_extract_all(text, "Decision[:blank:]\\(.+\\)[:blank:][:digit:]+/[:digit:]+")) %>%
  unnest(Ref.Dec4,keep_empty = TRUE) %>%
  mutate(Ref.Dec5 =  str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+\\:[:blank:]Council[:blank:]Decision")) %>%
  unnest(Ref.Dec5,keep_empty = TRUE) %>%
  mutate(Ref.Dec6 =  str_extract_all(text, "[:digit:]+/[:digit:]+/[:alpha:]+\\:[:blank:]Commission[:blank:]Decision")) %>%
  unnest(Ref.Dec6,keep_empty = TRUE) %>%
  pivot_longer(
    cols = Ref.Dec:Ref.Dec6,
    names_to = "str.type",
    values_to = "title",
    values_drop_na = TRUE) %>%
  select(-str.type)%>%
  mutate(title = str_replace_all(title, "Decision","")) %>%
  mutate(title = str_replace_all(title, "\\(EU\\)","")) %>%
  mutate(title = str_replace_all(title, "No","")) %>%
  mutate(title = str_replace_all(title, "Council","")) %>%
  mutate(title = str_replace_all(title, "\\:","")) %>%
  mutate(title = str_replace_all(title, "Commission","")) %>%
  mutate(title = str_trim(title, side = c("both")))
# ok lets match the citation title to the citation
decisions.key <- 
  citation.titles %>%
  filter(resource.type == "DEC") %>%
  mutate(title = str_replace_all(title, "Decision","")) %>%
  mutate(title = str_replace_all(title, "\\(EU\\)","")) %>%
  mutate(title = str_replace_all(title, "No","")) %>%
  mutate(title = str_replace_all(title, "Council","")) %>%
  mutate(title = str_replace_all(title, "\\:","")) %>%
  mutate(title = str_replace_all(title, "Commission","")) %>%
  mutate(title = str_trim(title, side = c("both"))) %>%
  right_join(.,Q1.network.Dec.sentences, by = c("title" = "title"))

  

  
# not consistent at all for these

Q1.network.Rec.sentences <- 
  Q1.network.text %>%
  get_sentences() %>%
  mutate(Rec.Dec =  str_extract_all(text, "Recommendation")) %>%
  unnest(Rec.Dec)


Q1.network.Opin.sentences <- 
  Q1.network.text %>%
  get_sentences() %>%
  mutate(opin =  str_extract_all(text, "Opinion on")) %>%
  unnest(opin)


#---------------------------------------------------------------------------
#------------- This is the analysis on the second search query  ------------
#----------------------- 18 terms all diff types of ------------------------
#-------------------------- MPA Designation names --------------------------
#---------------------------------------------------------------------------



# Save files ---------------------------------------------------------------------


# archival -----

Q1.sent.bytheme <- 
  Q1.sent.text %>%
  distinct(doc_id,MT, .keep_all = TRUE) %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(MT))

plot(sent.bytheme)

Q1.sent.bydocument <- 
  Q1.sent.text %>%
  distinct(doc_id,MT, .keep_all = TRUE) %>%
  group_by(MT) %>%
  summarise(n = n_distinct(doc_id)) 

# double check and compare these two outputs... 
# the functions might not take ito account duplicates due to multiple theme associations


Q1.sent.byresourcetype <- 
  Q1.sent.text %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(resource.type))



Q2.sent.bydocument <- 
  Q2.sent.text %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(doc_id))

Q2.sent2.resource.type <- 
  Q2.sent.text %>%
  mutate(sentences = get_sentences(text)) %$%
  sentiment_by(sentences, list(resource.type))


