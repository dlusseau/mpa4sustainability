
# Clear work space ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------

library("eurlex")
library("dplyr")
library("tibble")
library("stringr")
library("purrr")

# Define functions --------------------------------------------------------

# pull the function David created: 
source(file = "WP4/Policy_Interactions/R/freetext_eurlex")

# Load data ---------------------------------------------------------------

# term eur-lex search ---------------

# query term: marine protected area* (no parenthesis!)

# Resource Types we want with associated list of act codes (FM_CODE) 
# what we are interested in is legislation (5 types of them):
# https://european-union.europa.eu/institutions-law-budget/law/types-legislation_en

  # Directives: DIR
  # Regulation: REG
  # Decisions: DEC
  # Recommendations: RECO
  # Opinions: OPIN

resource.types <- c("DIR","REG", "DEC",
                    "RECO","OPIN")

mpaCELEX.list<-list()

for (i in 1:length(resource.types)) {  
  mpaCELEX.list[[i]]<- 
    freetext_eurlex("marine protected area*",
                    act=resource.types[[i]],
                    lang="en",
                    exactly=FALSE)

}

# How many result pages show up from the search: 
#[1] "there are 13 pages of results"
#[1] "there are 47 pages of results"
#[1] "there are 26 pages of results"
#[1] "there are 3 pages of results"
#[1] "there are 28 pages of results"

# Lets give each list the name based on the resource type: 
mpaCELEX.list <- structure(mpaCELEX.list, names=resource.types)

# Make it into a nice data frame
mpaCELEX.df <- 
  mpaCELEX.list %>%
  unlist(.) %>%
  as.data.frame() %>%
  rownames_to_column(.) %>%
  rename(.,  CELEX = .) %>%
  rename(.,  resource.type = rowname) %>%
  mutate(resource.type = str_extract(resource.type,"[:alpha:]+"))

# We have 1,084 EU legislation documents relating to marine protected area*
# this number changes every time I run the loop... the pages of results dont change, but the document numbers do...
# one time it was 1114 documents, another time it was 1104 documents


# duplicate CELEX? shouldn't be since I am guessing a document can 
# only be categorized into one resource types BUT... double check to be sure
mpaCELEX.df[duplicated(mpaCELEX.df$CELEX)]
# no duplicates :) 

mpaCELEX.df <-
  mpaCELEX.df %>%
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX))

# eurlex package search ---------------

# we have to make a key to link key terms

SPARQL.resource.type <- c("directive","regulation", 
                          "decision", "recommendation")

SPARQL.CELEX.list<-list()

for (i in 1:length(SPARQL.resource.type)) {  
  SPARQL.CELEX.list[[i]]<- 
    elx_make_query(resource_type = SPARQL.resource.type[[i]],
                   include_eurovoc = TRUE,
                   include_date = TRUE, 
                   include_force = TRUE) %>% 
    elx_run_query() %>% 
    rename(date = `callret-3`) #rename column to be more understandable
  
}

# Lets give each list the name based on the resource type: 
SPARQL.CELEX.list <- structure(SPARQL.CELEX.list, names=SPARQL.resource.type)

# Make it into a nice data frame
SPARQL.CELEX.df <- 
  SPARQL.CELEX.list %>%
  bind_rows()
  
#opinion has to be done manually: 
opinion.key <- elx_make_query(resource_type = "manual", 
                              manual_type = "OPIN",
                              include_eurovoc = TRUE,
                              include_date = TRUE, 
                              include_force = TRUE) %>% 
  elx_run_query() %>% 
  rename(date = `callret-3`) #rename column to be more understandable

SPARQL.CELEX.df <- rbind(SPARQL.CELEX.df,opinion.key)

# Lets bind the CELEX df to the key df:

mpa.policy.df <- 
  mpaCELEX.df %>%
  left_join(.,SPARQL.CELEX.df, by = c("CELEX" = "celex"))
#the df has become larger bc there can be multiple keywords for each document

#convert eurovoc codes to actual words
eurovoc_lookup.key <- elx_label_eurovoc(uri_eurovoc = mpa.policy.df$eurovoc)

# now lets join them back to the data set 
mpa.policy.df <-
  mpa.policy.df %>% 
  left_join(.,eurovoc_lookup.key, by = "eurovoc")
# remember each row is not necessarily a unique document! due to multiple key terms 

# # extract text data: ---------------------------------
# error when trying to do all... takes too long
CELEX_text.data <- 
  mpaCELEX.df %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  mutate(text = map_chr(url, elx_fetch_data, "text")) %>% 
  as_tibble() 


