
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("eurlex")
library("dplyr")
library("tibble")
library("stringr")
library("purrr")
library("countrycode")
library("tidyr")

# Define functions --------------------------------------------------------

# pull the function David created: 
source(file = "WP4/Policy_Interactions/R/freetext_eurlex")

# Load data ---------------------------------------------------------------

# EUR-Lex Data --------------------------------------

# -------- term eur-lex website search ---------------

# query term: marine protected area* (no parenthesis!)

# Resource Types we want with associated list of act codes (FM_CODE) 
# what we are interested in is legislation (5 types of them):
# https://european-union.europa.eu/institutions-law-budget/law/types-legislation_en

#result for the webpage:
  # Directives: DIR -128
  # Regulation: REG - 466
  # Decisions: DEC - 259
  # Recommendations: RECO - 24 
  # Opinions: OPIN -279

# function results:
#mpaCELEX.list
#$ DIR : chr [1:128] "32008L0056" "32014L0089" "32019L1937" "32006L0007" ...
#$ REG : chr [1:432] "32019R1241" "32009R1107" "32021R1139" "32014R1143" ...
#$ DEC : chr [1:245] "32013D1386" "32022D0591" "32008D0768" "32017D1324" ...
#$ RECO: chr [1:22] "32007H0526" "32013H0179" "52020IP0152" "32019H0423(01)" ...
#$ OPIN: chr [1:277] "52008AE0990(01)" "52016AR2898" "52010AR0339" "52012AR2203" ...

resource.types <- c("DIR","REG", "DEC",
                    "RECO","OPIN")

mpaCELEX.list<-list()

for (i in 1:length(resource.types)) {  
  mpaCELEX.list[[i]]<- 
    freetext_eurlex("marine protected area*",
                    act=resource.types[[i]],
                    lang="en",
                    exactly=TRUE)

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

# -------- eurlex package search ---------------

# we have to make a key to link key terms

SPARQL.resource.type <- c("directive","regulation", 
                          "decision", "recommendation")

SPARQL.CELEX.list<-list()

for (i in 1:length(SPARQL.resource.type)) {  
  SPARQL.CELEX.list[[i]]<- 
    elx_make_query(resource_type = SPARQL.resource.type[[i]],
                   include_eurovoc = TRUE,
                   include_date = TRUE, 
                   include_force = TRUE,
                   include_citations = TRUE) %>% 
    elx_run_query() %>% 
    rename(date = `callret-3`) #rename column to be more understandable
  
}

# Lets give each list the name based on the resource type: 
SPARQL.CELEX.list <- structure(SPARQL.CELEX.list, names=SPARQL.resource.type)

# Make it into a nice data frame
SPARQL.CELEX.df <- 
  SPARQL.CELEX.list %>%
  bind_rows(.id = "resource.type") %>%
  mutate(resource.type = case_when(
    resource.type == "directive" ~ "DIR",
    resource.type == "regulation" ~ "REG",
    resource.type == "decision" ~ "DEC",
    resource.type == "recommendation" ~ "RECO"))


#opinion has to be done manually: 
opinion.key <- elx_make_query(resource_type = "manual", 
                              manual_type = "OPIN",
                              include_eurovoc = TRUE,
                              include_date = TRUE, 
                              include_force = TRUE,
                              include_citations = TRUE) %>% 
  elx_run_query() %>% 
  rename(date = `callret-3`) %>% #rename column to be more understandable
  mutate(resource.type = "OPIN")

SPARQL.CELEX.df <- rbind(SPARQL.CELEX.df,opinion.key)

# Save file 
write.csv(x = SPARQL.CELEX.df,
          file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv", row.names=FALSE)

#SPARQL.CELEX.df <-  read.csv(file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")
  
#convert eurovoc codes to actual words
eurovoc_lookup.key <- elx_label_eurovoc(uri_eurovoc = SPARQL.CELEX.df$eurovoc)

eurovoc.themes <- read.csv("WP4/Policy_Interactions/data/raw_data/eurovoc_export_en.csv")


# now lets join them back to the data set 
SPARQL.CELEX.df <-
  SPARQL.CELEX.df %>% 
  left_join(.,eurovoc_lookup.key, by = "eurovoc")
# remember each row is not necessarily a unique document! due to multiple key and citations...
# so rows can have duplicate info


# Lets bind the CELEX df to the key df:
mpa.policy.df <- 
  mpaCELEX.df %>%
  left_join(.,SPARQL.CELEX.df, by = c("CELEX" = "celex","resource.type"))
#the df has become larger bc there can be multiple keywords for each document

# checking no missing or duplicates...
n_distinct(unique(mpa.policy.df$CELEX))
# 1104 matches the original :) 
n_distinct(unique(mpa.policy.df$labels))


# Save file 
write.csv(x = mpa.policy.df,
          file = "WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv", row.names=FALSE)

# -------- extract text data: -------------

# error when trying to do all... takes too long
#CELEX_text.data <- 
#  mpaCELEX.df[1:5,]%>%
#  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
#  as_tibble() %>%
#  mutate(text = map_chr(url, elx_fetch_data, "text")) %>% 
#  as_tibble() 


#text.df2 <- 
#  CELEX_text.data %>%
#  mutate(references = str_extract_all(text, "\\d+\\/\\d+\\/\\b[:alpha:]+")) 

#text.df2[1,]$references
#text.df3 <- unnest(text.df2, references)

# World Database of Protected Areas Data ------------

load("WP4/Policy_Interactions/data/raw_data/all.characteristics.mpas.Rdata")

# O.k. so we first are only interested in countries within the EU so lets subset all other countries out of the data set 

# ‘countrycode’: This package might help with converting the codes to country names: 

# this data set from countrycode package has country names and ISO3 codes
codelist <- codelist
str(codelist)

country.key <- 
  codelist %>%
  subset(.,select = c(iso3c,country.name.en))

# EU member countries (27 but 28 since we are including the UK for this)
EU.members <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                "Czechia","Denmark","Estonia","Finland","France",
                "Germany","Greece","Hungary","Ireland","Italy",
                "Latvia","Lithuania","Luxembourg","Malta","Netherlands",
                "Poland","Portugal","Romania","Slovakia","Slovenia",
                "Spain","Sweden","United Kingdom") 

EU.members.key <- data.frame(country.name.en = EU.members, EU = "Yes")

country.key <- 
  country.key %>% 
  left_join(.,EU.members.key, by="country.name.en") %>%
  mutate(EU = replace_na(EU,"No"))

# lets join with mpa.char
mpa.char1 <-
  mpa.char %>%
  left_join(.,country.key,by= c("PARENT_ISO"="iso3c"))

# We are only interested in EU:
EU.mpa.char <- 
  mpa.char1 %>%
  filter(.,EU=="Yes")
# total 8,448 MPAs associated to EU parent ISOs 

unique(EU.mpa.char$DESIG_TYPE)
# [1] "National" "International" "Regional"     

# We are only sticking to international and regional designation areas:
EU.mpa.char.edit <-
  EU.mpa.char %>%
  filter(DESIG_TYPE == "International" | 
         DESIG_TYPE == "Regional" ) %>%
  mutate(DESIG_ENG = tolower(DESIG_ENG)) # change all to lowercase bc there are some inconsitencies
# total 3,604 MPAs associated to EU parent ISOs in international or regional designations 

#check:
unique(EU.mpa.char.edit$DESIG_TYPE)
# [1] "International" "Regional"     

unique(EU.mpa.char.edit$DESIG_ENG)
# We have 10 different unique designations: 
#[1] "ramsar site, wetland of international importance"                            
#[2] "world heritage site (natural or mixed)"                                      
#[3] "unesco-mab biosphere reserve"                                                
#[4] "specially protected areas of mediterranean importance (barcelona convention)"
#[5] "sites of community importance (habitats directive)"                          
#[6] "special areas of conservation (habitats directive)"                          
#[7] "special protection area (birds directive)"                                   
#[8] "baltic sea protected area (helcom)"                                          
#[9] "marine protected area (ospar)"                                               
#10] "specially protected area (cartagena convention)" 

# Mentioned documents: 
# habitats directive (31992L0043): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:31992L0043 (92/43/EEC)
# birds directive (32009L0147): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:32009L0147
# barcelona convention (21976A0216(01)): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=celex%3A21976A0216%2801%29
# ospar (21998A0403(01)): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=celex%3A21998A0403%2801%29
# cartagena convention (22002A0731(01)): https://eur-lex.europa.eu/legal-content/en/ALL/?uri=CELEX:22002A0731(01)
# helcom (52021PC0534): https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:52021PC0534

# Our search terms: 
# "ramsar site"
# "wetland of international importance"                            
# "world heritage site (natural or mixed)"                                      
# "unesco-mab biosphere reserve"                                                
# "specially protected areas of mediterranean importance"
# "barcelona convention"
# "sites of community importance"
# "habitats directive"                          
# "special areas of conservation"
# "habitats directive"                          
# "special protection area"
# "birds directive"                                   
# "baltic sea protected area
# "helcom"                                          
# "marine protected area"
# "ospar"                                               
# "specially protected area"
# "cartagena convention" 

search_terms <- c("ramsar site", 
                  "wetland of international importance",
                  "world heritage site (natural or mixed)", 
                  "unesco-mab biosphere reserve",
                  "specially protected areas of mediterranean importance",
                  "barcelona convention",
                  "sites of community importance",
                  "habitats directive",
                  "special areas of conservation",
                  "habitats directive",
                  "special protection area",
                  "birds directive",
                  "baltic sea protected area",
                  "helcom",                                          
                  "marine protected area",
                  "ospar",
                  "specially protected area",
                  "cartagena convention")


