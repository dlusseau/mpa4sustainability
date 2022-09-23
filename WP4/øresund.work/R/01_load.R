
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library('RSelenium') 
library("rvest")
library('readr') 
library('stringr') 
library('tibble') 
library("netstat")

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
       


# How to get the text for one URL -----------------------------------------

binman::list_versions("chromedriver")
# $win32
# [1] "105.0.5195.19" "105.0.5195.52" "106.0.5249.21"

remDr <- rsDriver(browser='chrome', port=4444L,check = FALSE, 
                  chromever="105.0.5195.19")

browser <- remDr$client

browser$open()

browser$navigate("https://www.retsinformation.dk/eli/lta/2021/2584")

pagesource <- browser$getPageSource()

html <- read_html(pagesource[[1]])

text <-
  html%>%
  html_nodes(xpath = '//*[@class="document-content "]') %>%
  html_text2()

# A loop to get text data for all URLs -------------------------------------

search.term <- deframe(retsinformation.df[,1])
search.term

DK.text.list <- structure(vector("list", 30))

# Lets try to get this data from the url...
URLs <- deframe(retsinformation.df[,30])
URLs

URLs <- URLs[1:30]

for (i in seq(URLs)) {
  
remDr <- rsDriver(browser='chrome', 
                  port=free_port(random = TRUE),
                  check = FALSE, 
                  chromever="105.0.5195.19")
  
browser <- remDr$client
  
browser$open()

browser$navigate(URLs[i])

Sys.sleep(2)

pagesource <- browser$getPageSource()

html <- read_html(pagesource[[1]])

DK.text.list[[i]] <-
  html%>%
  html_nodes(xpath = '//*[@class="document-content "]') %>%
  html_text2()

print(i)

}









