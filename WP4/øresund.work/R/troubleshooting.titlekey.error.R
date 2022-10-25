# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("tidyr")
library("tibble")
library("dplyr")
library("eurlex")
library("purrr")


# EURLEX KEY
document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

document.key.df1 <- 
  document.key.df %>% 
  select(resource.type, work, celex) %>%
  mutate(celex = as.factor(celex)) %>%
  distinct(celex, .keep_all = TRUE) 

rm(document.key.df) # remove this bc it takes up a lot of space

directive.titles <- 
  document.key.df1 %>%
  filter(resource.type == "DIR") %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)

rm(document.key.df1) # remove this bc it takes up a lot of space

directive.titles[2652,]
directive.titles[2480,]


test<-array(0)
gc()

for (i in seq(dim(directive.titles)[1])) {

  test[i]<-elx_fetch_data(directive.titles$work[i],type="title")
  
  print(i)
  
  flush.console()
  gc()
  
}

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

# archived ---------------------------

gc()
test <- 
  directive.titles %>%
  mutate(title = map_chr(work, elx_fetch_data, "title")) %>% 
  as_tibble() 

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
  flush.console()
  
}
# keep getting: 
# Internal Server Error (HTTP 500).Error in UseMethod("status_code") : 
# no applicable method for 'status_code' applied to an object of class "NULL"


