# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("tidyr")
library("tibble")
library("dplyr")
library("eurlex")
library("purrr")

# Build a legislation-title key -------------------------------------------------

# EURLEX KEY
document.key.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

document.key.df1 <- 
  document.key.df %>% 
  select(resource.type, work, celex) %>%
  mutate(celex = as.factor(celex)) %>%
  distinct(celex, .keep_all = TRUE) 

rm(document.key.df) # remove this bc it takes up a lot of space

# --- Directives --- #

# run on 25-10-2022
directive.titles <- 
  document.key.df1 %>%
  filter(resource.type == "DIR") %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)

test<-array(0) #2659, 4080 problem ones, skip these and do them later (see below)
for (i in 4081:dim(directive.titles)[1]) {
  test[i]<-elx_fetch_data(directive.titles$work[i],type="title")
  print(i)
  flush.console()
}

i<-2659
test[i]<-elx_fetch_data(directive.titles$work[i],type="title")
# ran this above
i<-4080
test[i]<-elx_fetch_data(directive.titles$work[i],type="title")

directive.titles$titles<-test

write.csv(directive.titles, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EurLexKey.directive.titles.csv") 
            
# --- Decisions --- #

decision.titles <- 
  document.key.df1 %>%
  filter(resource.type == "DEC") %>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)

rm(document.key.df1) # remove this bc it takes up a lot of space

dec.test<-array(0) # 127 problem one make title blank url is faulty..., 386 skip these and do them later (see below)

for (i in 5201:dim(decision.titles)[1]) {
  dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
  print(i)
  flush.console()
}

 i<-127 #--> faulty link make title blank
dec.test[127]<- " "
# ran this above
i<-386
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-780
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-822
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-1274
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-1308
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-1325
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-1806
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-1876
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-2160
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-2543
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-2655
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-2968
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-3661
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-4468
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")
# ran this above
i<-5200
dec.test[i]<-elx_fetch_data(decision.titles$work[i],type="title")


decision.titles$titles<-dec.test

saveRDS(decision.titles, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/03.EurLexKey.decision.titles1.6326.rds") 

# --- Regulation --- #

regulation.titles <- 
  document.key.df1 %>%
  filter(resource.type == "REG")%>%
  distinct(celex, .keep_all = TRUE) %>%
  filter(!is.na(.$celex)) %>% # remove na values for celex
  select(-resource.type,-celex)








# Archived ---------------------------

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


regulation.titles <- 
  document.key.df1 %>%
  filter(resource.type == "REG") %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  select(celex,title) %>%
  mutate(celex = as.factor(celex)) 




