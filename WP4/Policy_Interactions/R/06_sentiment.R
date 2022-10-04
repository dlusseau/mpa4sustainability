
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("sentimentr")
library("tidyverse")
library("magrittr")
library("eurlex")
library("dplyr")
library("tibble")

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

#--------------------------------------------------------------------------------------
#----------------- This is the analysis on the first search query  --------------------
#------------------------------- "marine protected " ----------------------------------
#--------------------------------------------------------------------------------------

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

Q1.network.Reg.sentences <- 
  Q1.network.text %>%
  get_sentences() %>%
  mutate(Ref.Reg =  str_extract_all(text, "Regulation[:blank:]\\([:alpha:]+\\)[:blank:]No[:blank:][:digit:]+/[:digit:]+")) %>%
  unnest(Ref.Reg) # this removes the sentences without references
# keep_empty = TRUE

n_distinct(Q1.network.Reg.sentences$Ref.Reg) # 133

Q1.network.Dir.sentences <- 
  Q1.network.text %>%
  get_sentences() %>%
  mutate(Ref.Dir =  str_extract_all(text, "Directive[:blank:][:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  unnest(Ref.Dir)

n_distinct(Q1.network.Dir.sentences$Ref.Dir) # 60

Q1.network.Dec.sentences <- 
  Q1.network.text %>%
  get_sentences() %>%
  mutate(Ref.Dec =  str_extract_all(text, "Decision[:blank:][:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  unnest(Ref.Dec)

n_distinct(Q1.network.Dec.sentences$Ref.Dec) # 31


Q1.network.Rec.sentences <- 
  Q1.network.text %>%
  get_sentences() %>%
  mutate(Rec.Dec =  str_extract_all(text, "Recommendation[:blank:][:digit:]+")) %>%
  unnest(Rec.Dec)

# not consistent at all for these
# Q1.network.Opin.sentences <- 
#   Q1.network.text %>%
#   get_sentences() %>%
#   mutate(Opin.Dec =  str_extract_all(text, "Recommendation[:blank:][:digit:]+")) %>%
#   unnest(Opin.Dec)

Q1.network.titles <- 
  Q1.verticesdata %>%
  select(CELEX) %>%
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX)) %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  select(CELEX,title) %>%
  mutate(title.Dec =  str_extract(title, "Decision[:blank:]No[:blank:][:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  mutate(title.Dir =  str_extract(title, "Directive[:blank:][:digit:]+/[:digit:]+/[:alpha:]+")) %>%
  mutate(title.Reg =  str_extract(title, "Regulation[:blank:]\\([:alpha:]+\\)[:blank:]No[:blank:][:digit:]+/[:digit:]+")) %>%
  mutate(title.Dec =  str_extract(title, "Recommendation[:blank:][:digit:]+")) 
  

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


