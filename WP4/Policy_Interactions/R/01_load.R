
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

# pull the function David created which does the eurlex search term query 
source(file = "WP4/Policy_Interactions/R/freetext_eurlex")

# Load data ---------------------------------------------------------------

# EUR-Lex Data --------------------------------------

# what we are interested in is legislation (5 types of them):
# https://european-union.europa.eu/institutions-law-budget/law/types-legislation_en

# Thus our searches will be designated within these types of documents: 
# Directives: DIR 
# Regulation: REG 
# Decisions: DEC 
# Recommendations: RECO 
# Opinions: OPIN 

# --------  1st term eur-lex website search ---------------

# Moving forward with the search query as "marine protected" exactly = TRUE 

resource.types <- c("DIR","REG", "DEC",
                    "RECO","OPIN")

mpaCELEX.list<-list()

for (i in 1:length(resource.types)) {  
  mpaCELEX.list[[i]]<- 
    freetext_eurlex("marine protected", 
                    act=resource.types[[i]],
                    lang="en",
                    exactly=TRUE)
  
}

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
  
# duplicate CELEX? shouldn't be since I am guessing a document can 
# only be categorized into one resource types BUT... double check to be sure
mpaCELEX.df[duplicated(mpaCELEX.df$CELEX)]
# no duplicates :) 

mpaCELEX.df <-
  mpaCELEX.df %>%
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX))

# now we have the document ids that mention our term but we want associated document data.
# thus we have to create a legislation document-data key to link the celex to their associated data. Next step below

# ok now we need to remove the celexes that start with the number 5 since these are within the prepatory documents sector and thus not in the Legal acts sector
mpaCELEX.df <- 
  mpaCELEX.df %>%
  mutate(remove = str_detect(CELEX, "^5" )) %>%
  filter(remove == "FALSE") %>%
  select(-remove)

# --------  make a eurlex key to link key terms ---------------

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

# Now we have the eurovoc codes but lets convert them to actual words so it is more useful: 
eurovoc_lookup.key <- elx_label_eurovoc(uri_eurovoc = SPARQL.CELEX.df$eurovoc)

# I also found that these eurocov terms are grouping into a microthusarus, which is basically like themes maybe will use this maybe not but will attach in case... 
# I Downloaded this data from: 
# https://op.europa.eu/en/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/eurovoc
eurovoc.themes <- read.csv("WP4/Policy_Interactions/data/raw_data/eurovoc_export_en.csv")

eurovoc.themes1 <-
  eurovoc.themes %>%
  mutate(eurovoc = paste0("http://eurovoc.europa.eu/",.$ï..ID))%>%
  select(eurovoc,MT)%>%
  distinct(eurovoc,MT)

str(eurovoc.themes1)
str(eurovoc_lookup.key)

eurovoc_lookup.key1<-
  eurovoc_lookup.key %>%
  left_join(.,eurovoc.themes1, by = "eurovoc")
# *note some terms have two MTs...

# now lets join them back to the data set 
SPARQL.CELEX.df <-
  SPARQL.CELEX.df %>% 
  left_join(.,eurovoc_lookup.key1, by = "eurovoc")
# remember each row is not necessarily a unique document! due to multiple key and citations...
# so rows can have duplicate info

# Lets bind the CELEX df to the key df:
mpa.policy.df <- 
  mpaCELEX.df %>%
  left_join(.,SPARQL.CELEX.df, by = c("CELEX" = "celex","resource.type"))
#the df has become larger bc there can be multiple keywords for each document

# checking no missing or duplicates...
n_distinct(unique(mpa.policy.df$CELEX))
# 18 matches the original :) 
n_distinct(unique(mpa.policy.df$labels))
# 76 label terms
n_distinct(unique(mpa.policy.df$MT))
# 23 label themes

# OK now we have the documents with associated document data, now lets get the document text data, which could be useful later.

# -------- (1) extract text data: -------------

CELEXmpa.text.data <- 
  mpaCELEX.df %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  mutate(text = map_chr(url, elx_fetch_data, "text")) %>% 
  as_tibble() 

CELEXmpa.text.data1 <- 
  CELEXmpa.text.data %>%
  mutate(total.text = paste0(.$title,.$text))

# --------  2nd term eur-lex website search ---------------

# World Database of Protected Areas Data ------------

load("WP4/Policy_Interactions/data/raw_data/all.characteristics.mpas.Rdata")

# O.k. so we first are only interested in countries within the EU so lets subset all other countries out of the data set 

# ‘countrycode’: This package helps with converting the codes to country names: 
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

# We are only interested in international and regional designation areas:
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

mpa.DES.key <- 
  EU.mpa.char.edit %>%
  select(mpa,DESIG_ENG)

head(mpa.DES.key)

# Split up the designation titles into the search terms we wanted:
mpa.DES.key1 <- 
  mpa.DES.key %>%
  mutate(site1 = str_extract(DESIG_ENG,".+\\,"),
         site2 = str_extract(DESIG_ENG,"\\(.+\\)"),
         site3 = str_extract(DESIG_ENG,".+\\("),
         site4 = str_extract(DESIG_ENG,"\\,.+"),
         site5 = str_extract(DESIG_ENG,"unesco-mab biosphere reserve")) %>%
  mutate(site1 = str_replace_all(site1,"[:punct:]+",""),
         site2 = str_replace_all(site2,"[:punct:]+",""),
         site3 = str_replace_all(site3,"[:punct:]+",""),
         site4 = str_replace_all(site4,"[:punct:]+","")) %>%
  mutate(site6 = 
           case_when(
             DESIG_ENG == "sites of community importance (habitats directive)" ~ "site of community importance")) %>%
  select(-DESIG_ENG) %>%
  pivot_longer(
    cols = starts_with("site"),
    names_to = "search",
    values_to = "term",
    values_drop_na = TRUE
  ) %>% 
  select(mpa,term) %>%
  filter(term != "natural or mixed") %>%
  mutate(term = str_trim(term, side = "both"))%>%
  mutate(search.term = paste0(.$term,"*?")) # add a ? at the end 
# ALSO NEED TO ADD AN *. only a ? it looks for the non plural... i.e. barcelona conventions only comes up for ? but barcelona convention comes up for *?

unique(mpa.DES.key1$search.term) 
# 18 unique search terms:
#[1] "ramsar site*?"                                           "wetland of international importance*?"                  
#[3] "world heritage site*?"                                   "unesco-mab biosphere reserve*?"                         
#[5] "barcelona convention*?"                                  "specially protected areas of mediterranean importance*?"
#[7] "habitats directive*?"                                    "sites of community importance*?"                        
#[9] "site of community importance*?"                          "special areas of conservation*?"                        
#[11] "birds directive*?"                                       "special protection area*?"                              
#[13] "helcom*?"                                                "baltic sea protected area*?"                            
#[15] "ospar*?"                                                 "marine protected area*?"                                
#[17] "cartagena convention*?"                                  "specially protected area*?"  

resource.types <- c("DIR","REG", "DEC",
                    "RECO","OPIN")

search_terms.mpas <- c(unique(mpa.DES.key1$search.term))

mpaCELEX.list.mpaterms <- structure(vector("list", 5), names=resource.types)

for (i in seq_along(resource.types)) {
  
  for (j in seq_along(search_terms.mpas)) {
    
    mpaCELEX.list.mpaterms[[i]][[j]] <- structure(mpaCELEX.list.mpaterms)

      x <- freetext_eurlex(search_terms.mpas[j], 
                      act=resource.types[i],
                      lang="en",
                      exactly=TRUE)    
      
      if(is.null(x)==TRUE ) { mpaCELEX.list.mpaterms[[i]][[j]] <- NA } 
      else { mpaCELEX.list.mpaterms[[i]][[j]] <- x}

      }
  
}


mpaCELEX.list.mpaterms.1 <- mpaCELEX.list.mpaterms

# Adding the term names
names(mpaCELEX.list.mpaterms.1$DIR) <- search_terms.mpas
names(mpaCELEX.list.mpaterms.1$RECO) <- search_terms.mpas
names(mpaCELEX.list.mpaterms.1$REG) <- search_terms.mpas
names(mpaCELEX.list.mpaterms.1$DEC) <- search_terms.mpas
names(mpaCELEX.list.mpaterms.1$OPIN) <- search_terms.mpas

# lets make it into a df to use.
mpaCELEX.list.mpaterms.2 <- as.data.frame(cbind(mpaCELEX.list.mpaterms.1))

mpaCELEX.list.mpaterms.2 <- 
  mpaCELEX.list.mpaterms.2 %>% 
  rownames_to_column()

mpaCELEX.list.mpaterms.DF <- 
  tibble(mpaCELEX.list.mpaterms.2) %>% 
  unnest_longer(mpaCELEX.list.mpaterms.1) %>% 
  unnest_longer(mpaCELEX.list.mpaterms.1) %>%
  filter(!is.na(mpaCELEX.list.mpaterms.1)) %>%
  rename("search.term" = "mpaCELEX.list.mpaterms.1_id",
         "CELEX" = "mpaCELEX.list.mpaterms.1",
         "resource.type" = "rowname")

n_distinct(mpaCELEX.list.mpaterms.DF$CELEX)
#182 documents pulled
# df dim are larger since some documents can mention more than one term...

# Lets bind the df to the data key df:
mpaCELEX.list.mpaterms.DF2 <- 
  mpaCELEX.list.mpaterms.DF %>%
  left_join(.,SPARQL.CELEX.df, by = c("CELEX" = "celex","resource.type"))
#the df has become larger bc there can be multiple keywords for each document

# check just to be sure it is all good
n_distinct(mpaCELEX.list.mpaterms.DF2$CELEX)
# 182

# remove sector 5 documents (prepatory docs)
mpaCELEX.list.mpaterms.DF2 <- 
  mpaCELEX.list.mpaterms.DF2 %>%
  mutate(remove = str_detect(CELEX, "^5" )) %>%
  filter(remove == "FALSE") %>%
  select(-remove)

# check just to be sure it is all good
n_distinct(mpaCELEX.list.mpaterms.DF2$CELEX)
# 100


# -------- (2) extract text data: -------------

mpaterms.text.data <- 
  mpaCELEX.list.mpaterms.DF2 %>%
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX)) %>%
  distinct(CELEX,.keep_all=TRUE ) %>% 
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  as_tibble() %>%
  mutate(text = map_chr(url, elx_fetch_data, "text")) %>% 
  as_tibble() 

mpaterms.text.data <- 
  mpaterms.text.data %>%
  mutate(total.text = paste0(.$title,.$text)) %>%
  select(resource.type, CELEX, force, url,title,text,total.text)

# bc some document name mulitple mpa designations
mpaterms.text.data1 <- 
  mpaCELEX.list.mpaterms.DF2 %>%
  distinct(CELEX,search.term) %>%
  left_join(.,mpaterms.text.data, by = c("CELEX"))

n_distinct(mpaterms.text.data1$CELEX)
# 100 
# Save files ---------------------------------------------------------------------

# All these data were pulled from query, "cleaned", and saved in this script on Sep 22nd, 2022
# this data was updated Jan 3rd to ensure better data cleansing

# "marine protected" term  search results:
write.csv(x = mpa.policy.df,
          file = "WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv", row.names=FALSE)

# Large df as a Celex-data key:
write.csv(x = SPARQL.CELEX.df, #date sep19th this is "marine protected"
          file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv", row.names=FALSE)

# "marine protected" associated text data:
write.csv(x = CELEXmpa.text.data1,
          file = "WP4/Policy_Interactions/data/01_CELEXmpa.text.data.csv", row.names=FALSE)

# EU mpa characteristics: 
write.csv(x = EU.mpa.char.edit,
          file = "WP4/Policy_Interactions/data/01_EU.mpachar.csv", row.names=FALSE)

# EU mpa designation term search results: 
write.csv(x = mpaCELEX.list.mpaterms.DF,
          file = "WP4/Policy_Interactions/data/01_EUmpa.searchterm.CELEX.csv", row.names=FALSE)

# EU mpa designation term search results w/ associated data 
write.csv(x = mpaCELEX.list.mpaterms.DF2,
          file = "WP4/Policy_Interactions/data/01_EUmpa.searchterm.CELEX2.csv", row.names=FALSE)


# EU mpa designation term associated text data: 
write.csv(x = mpaterms.text.data,
          file = "WP4/Policy_Interactions/data/01_mpaterms.text.data.csv", row.names=FALSE)



