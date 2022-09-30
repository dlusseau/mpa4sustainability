
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library('RSelenium') 
library("rvest")
library('readr') 
library('stringr') 
library('tibble') 
library("netstat")
library("dplyr")
library("stringr")
library("tidyr")

# Define functions --------------------------------------------------------

# Load data ---------------------------------------------------------------

# Downloaded from retsinformation.dk using the search terms given
setwd("C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/raw_data/DK_policy")

retsinformation.file.list <- list.files(pattern='*.csv')

#error was coming up...
# https://stackoverflow.com/questions/60906507/how-to-solve-error-error-in-ncharrownamesm-invalid-multibyte-string-ele
# https://readr.tidyverse.org/articles/locales.html
x <- "PopulærTitel" # --> one of the df column names
Encoding(x) #"latin1"
html_encoding_guess("C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/raw_data/DK_policy/fiskeri.csv")
#encoding   confidence
# ISO-8859-1       pt       0.25
# ISO-8859-2       ro       0.12
# ISO-8859-9       tr       0.12
#   UTF-16BE                0.10
#   UTF-16LE                0.10
#  Shift_JIS       ja       0.10
#    GB18030       zh       0.10
#       Big5       zh       0.10
# ISO-8859-1 is another name for latin1

retsinformation.df <- read_delim(retsinformation.file.list, 
                                id = "search.term",
                                delim = ";",
                                locale = locale(encoding="ISO-8859-1"))

retsinformation.df <-
  retsinformation.df %>%
  mutate(search.term = str_extract_all(search.term,"\\w+\\."),
         search.term = str_replace_all(search.term,"[:punct:]+",""))
       
# A loop to get text data for all URLs -------------------------------------

search.term <- deframe(retsinformation.df[,1])
search.term <- search.term[1:1367]

DK.text.list <- structure(vector("list", 1367), names=search.term)

# Lets try to get this data from the url...
URLs <- deframe(retsinformation.df[,30])
URLs

URLs <- URLs[1:1367]
port <- seq(1:1367)

# make sure nothing is open before we start the loop
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) 

for (i in seq(URLs)) {
  
  remDr <- rsDriver(browser='chrome',
                    port= netstat::free_port(), # this didnt work eventually got a port in use error
                    #port = port[i], # this also didnt work I got a port error issue....
                    check = FALSE, 
                    chromever="105.0.5195.19")
  
  browser <- remDr$client
  
  browser$open() # Open the remote browser
  
  browser$navigate(URLs[i]) # navigate to the URL 
  
  Sys.sleep(2) # Stop for 2 second because takes a couple secs for the pg. to load
  
  pagesource <- browser$getPageSource() # retrieve html page source code
  
  html <- read_html(pagesource[[1]])
  
  DK.text.list[[i]] <-
    html%>%
    html_nodes(xpath = '//*[@class="document-content "]') %>%
    html_text2()
  
  print(URLs[i]) # Print the URL we are on
  print(i)       # Print what iteration we are on
  
  browser$quit() # Close the browser session

  rm(remDr) # Remove this obj.
  rm(browser) # Remove this obj.
  
  # so we dont have port use issues we need to kill the java instances found on this thread: https://github.com/ropensci/RSelenium/issues/228 
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) 
  
  gc() # free up some RAM for the large loop
  
  
  }

DK.text.list.1 <- DK.text.list


# lets make it into a df to use.
DK.text.list.1.2 <- as.data.frame(cbind(DK.text.list.1))
#DK.text.list.1.2 <- as.data.frame(unlist(DK.text.list.1))

DK.text.list.2 <- 
  DK.text.list.1.2 %>% 
  rownames_to_column(., var = "search.term") %>%
  rename("text" = "DK.text.list.1") %>%
  mutate(search.term = str_replace_all(search.term,"\\.[:graph:]+",""),
         country = "DK",
         ID = row_number()) %>%
  as_tibble() %>%
  unnest(text,keep_empty = TRUE)

str(DK.text.list.2)

DK.text.list.2 <- DK.text.list.2[1:929,1:4]


write.csv(x = DK.text.list.2,
          file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/01_DK.textDF.csv", row.names=FALSE)


# ok one Thursday evening 29-09-2022 i ran the code but it only got text from URLs 1-927 
# then an error Fejl i read_xml.raw(charToRaw(enc2utf8(x)), "UTF-8", ..., as_html = as_html,  : 
# Excessive depth in document: 256 use XML_PARSE_HUGE option [1]
# so I will try this url alone maybe something wrong with th link...
# put the df with 928 data in the one drive for now. 

# So the issue is this webpage : https://www.retsinformation.dk/eli/retsinfo/2000/20037 which was number 930 in the url vector 
# cannot be read by selenium thus the error occured and the loop stopped...

# archival for now -----------------------------------------
# semi working loop below -----

search.term <- deframe(retsinformation.df[,1])
search.term <- search.term[1:10]

DK.text.list <- structure(vector("list", 10), names=search.term)

# Lets try to get this data from the url...
URLs <- deframe(retsinformation.df[,30])
URLs

URLs <- URLs[1:10]

for (i in seq(URLs)) {
  
remDr <- rsDriver(browser='chrome', 
                  port= free_port(random = TRUE),
                  check = FALSE, 
                  chromever="105.0.5195.19")
  
browser <- remDr$client
  
browser$open() # Open the remote browser

browser$navigate(URLs[i]) # navigate to the URL 

Sys.sleep(2) # Stop for 2 second because takes a couple secs for the pg. to load

pagesource <- browser$getPageSource() # retrieve html page source code

html <- read_html(pagesource[[1]])

DK.text.list[[i]] <-
  html%>%
  html_nodes(xpath = '//*[@class="document-content "]') %>%
  html_text2()

print(i) # Print what iteration we are on

browser$close() # Close the browser
rm(remDr) # Remove this obj.
# so we dont have port use issues we need to kill the java instances found on this thread: https://github.com/ropensci/RSelenium/issues/228 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) 

Sys.sleep(1) 

  }

# this loop works until there was like 500 and then we had another port in use issue

DK.text.list.1 <- DK.text.list


# lets make it into a df to use.
DK.text.list.1.2 <- as.data.frame(cbind(DK.text.list.1))
#DK.text.list.1.2 <- as.data.frame(unlist(DK.text.list.1))

DK.text.list.2 <- 
  DK.text.list.1.2 %>% 
  rownames_to_column(., var = "search.term") %>%
  rename("text" = "DK.text.list.1") %>%
  mutate(search.term = str_replace_all(search.term,"\\.[:graph:]+",""),
         country = "DK",
         ID = row_number()) %>%
  as_tibble() %>%
  unnest(text)

str(DK.text.list.2)

write.csv(x = DK.text.list.2,
          file = "C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/01_DK.textDF.csv", row.names=FALSE)



# How to get the text for one URL ------

URL <- URLs[686]


binman::list_versions("chromedriver")
# $win32
# [1] "105.0.5195.19" "105.0.5195.52" "106.0.5249.21"

remDr <- rsDriver(browser='chrome', port=9871L,check = FALSE, 
                  chromever="105.0.5195.19")

browser <- remDr$client

browser$open()

browser$navigate(URL)

pagesource <- browser$getPageSource()

html <- read_html(pagesource[[1]])

text <-
  html%>%
  html_nodes(xpath = '//*[@class="document-content "]') %>%
  html_text2()

