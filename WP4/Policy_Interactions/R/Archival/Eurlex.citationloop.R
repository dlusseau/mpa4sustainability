
# Clear work space ---------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries -----------------------------------------------------------

library("eurlex")
library("dplyr")
library("tibble")
library("stringr")
library("purrr")

# Load data ---------------------------------------------------------------

Q1.data <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")
Q2.data <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_EUmpa.searchterm.CELEX2.csv")


## Trying to make a loop to extract the citation multi-levels --------------

## make a erulex data key to loop through sorting by sector resource types 

sector.type <- seq(1,3)

SPARQL.any.list<-list()

for (i in 1:length(sector.type)) {  
  SPARQL.any.list[[i]]<- 
    elx_make_query(resource_type = "any", 
                   sector = sector.type[i],
                   include_date = TRUE, 
                   include_force = TRUE,
                   include_citations = TRUE) %>% 
    elx_run_query() %>% 
    rename(date = `callret-3`)  #rename column to be more understandable
  
  print(i)
}

# Lets give each list the name based on the resource type: 

SPARQL.any.list.chr <- c("1","2","3")
SPARQL.any.list <- structure(SPARQL.any.list, names=SPARQL.any.list.chr)


# Make it into a nice data frame
SPARQL.any.df <- 
  SPARQL.any.list %>%
  bind_rows(.id = "sector.type")

nas <- 
  SPARQL.any.df %>%
  filter(is.na(celex)) 

# conclude from investigating the NAs that...
#  1.	It is not a rate limiting error
#  2.	For many nas, the classification as NA is valid because the document as been removed from the Journal 
#     OR...
#  3.	There are forced NAs, that is documents that have a valid url but that url returns NA values:
#     a.	These are not wrongly formatted urls
#     b.	It looks like it applies to duplicates and at least in one case the celex duplicate is already present in the data

# so we will remove these rows that are NA celexes due to the resons stated above and then will move forward 


# We only need the celex and the citation info

SPARQL.any.df %>%
  filter(celex %in% Q1.data$CELEX) %>%
  distinct(celex)

Q1.data  %>%
  distinct(CELEX)


### ------------- First Query ----------------- ###


Q1.Seed.doc <- 
  SPARQL.any.df %>%
  filter(celex %in% Q1.data$CELEX) %>%
  mutate(order = 1) %>%
 # rename("celex" = "CELEX")%>%
  distinct(celex,force,date,work,citationcelex,sector.type,order)


Q1.cit.list <- list(Q1.Seed.doc)

for (i in seq(1:20)) {
  
  Q1.cit.list[[i+1]] <-
    SPARQL.any.df %>%
    filter(celex %in% Q1.cit.list[[i]]$citationcelex) %>%
    filter(!(is.na(celex))) %>%
    mutate(order = i+1)
  print(i+1)  
}

# it seems like we get into a loop where the citations keep citing eachother.... so we have reached the end.
# remove everything 18 and up this is duplicates

Q1.cit.list.new <- Q1.cit.list[-21] 
Q1.cit.list.new <- Q1.cit.list.new[-20] 
Q1.cit.list.new <- Q1.cit.list.new[-19] 
Q1.cit.list.new <- Q1.cit.list.new[-18] 
Q1.cit.list.new <- Q1.cit.list.new[-17] 


Q1.ordernames <- as.character(seq(1,16))
Q1.cit.list.new <- structure(Q1.cit.list.new, names=Q1.ordernames)


# Make it into a nice data frame
Q1.cit.df <- 
  Q1.cit.list.new %>%
  bind_rows(.id = "Citation.Order")

### -------- Save ---------- #
library(jsonlite)

Q1_totalcitations.json <- toJSON(Q1.cit.df, pretty=TRUE)

#jsonQ1.2 <- fromJSON(jsonQ1, flatten=TRUE)

write(Q1_totalcitations.json,  file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q1_totalcitations.json")

### ------------- Second Query ----------------- ###


Q2.Seed.doc <- 
  SPARQL.any.df %>%
  filter(celex %in% Q2.data$CELEX) %>%
  mutate(order = 1) %>%
  # rename("celex" = "CELEX")%>%
  distinct(celex,force,date,work,citationcelex,sector.type,order)



Q2.cit.list <- list(Q2.Seed.doc)

for (i in seq(1:20)) {
  
  Q2.cit.list[[i+1]] <-
    SPARQL.any.df %>%
    filter(celex %in% Q2.cit.list[[i]]$citationcelex) %>%
    filter(!(is.na(celex))) %>%
    mutate(order = i+1)
  print(i+1)  
}

# it seems like we get into a loop where the citations keep citing eachother.... so we have reached the end.
# remove everything 18 and up this is duplicates


Q2.cit.list.new <- Q2.cit.list[-21] 
Q2.cit.list.new <- Q2.cit.list.new[-20] 
Q2.cit.list.new <- Q2.cit.list.new[-19] 
Q2.cit.list.new <- Q2.cit.list.new[-18] 
Q2.cit.list.new <- Q2.cit.list.new[-17] 



Q2.ordernames <- as.character(seq(1,16))
Q2.cit.list.new <- structure(Q2.cit.list.new, names=Q2.ordernames)


# Make it into a nice data frame
Q2.cit.df <- 
  Q2.cit.list.new %>%
  bind_rows(.id = "Citation.Order")

### -------- Save ---------- #
library(jsonlite)

Q2_totalcitations.json <- toJSON(Q2.cit.df, pretty=TRUE)

#jsonQ1.2 <- fromJSON(jsonQ1, flatten=TRUE)

write(Q2_totalcitations.json,  file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/Q2_totalcitations.json")

