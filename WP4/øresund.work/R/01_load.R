
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("readr")
library("stringr")
library("dplyr")
library("rJava")
library("remotes")
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
library("tabulizer")


# Define functions --------------------------------------------------------


# Load data ---------------------------------------------------------------

setwd("C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/raw_data/DK_policy")

retsinformation.file.list <- list.files(pattern='*.csv')

#error was coming up...
# https://stackoverflow.com/questions/60906507/how-to-solve-error-error-in-ncharrownamesm-invalid-multibyte-string-ele
# https://readr.tidyverse.org/articles/locales.html
x <- "PopulærTitel" # --> one of the df column names
Encoding(x) #"latin1"
guess_encoding("C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/raw_data/DK_policy/fiskeri.csv")
#encoding   confidence
#<chr>           <dbl>
# ISO-8859-1       0.45
# ISO-8859-1 is another name for latin1

retsinformation.df <- read_delim(retsinformation.file.list, 
                                id = "search.term",
                                delim = ";",
                                locale = locale(encoding="ISO-8859-1"))

retsinformation.df <-
  retsinformation.df %>%
  mutate(search.term = str_extract_all(search.term,"\\w+\\."),
         search.term = str_replace_all(search.term,"[:punct:]+",""))
       
# Lets try to get this data from the url...
URLs <- retsinformation.df[,30]
vec.URLs <- as.vector(URLs)
extract_text("http://www.retsinformation.dk/eli/retsinfo/2007/20064", encoding = "ISO-8859-1") 


# this didnt work...
library(rvest)
"https://www.retsinformation.dk/eli/lta/2021/2584"

xx<-  read_html("https://www.retsinformation.dk/eli/lta/2021/2584") 
nodes<-html_nodes(xx,"[id='restylingRoot']")
flat<-unlist(strsplit(html_element(nodes,"[class='document-content']")%>%html_text2(),"\n"))
y <- xx %>% html_nodes("*") 
print(y, n=40)
xx %>% html_elements(".document-content")

xx%>%
  html_element("body") %>%
  html_text2() %>%
  cat()

xx%>%
  html_nodes("div.document-content") %>%
  html_text()

xx %>%
  html_nodes(xpath = '//*[@class="document-content "]') %>%
  html_text2()

xx %>%
  html_nodes(xpath = '//*[@class="Titel2"]') %>%
  html_text2()

xx %>%
  html_nodes(xpath = '//*[@id="restylingRoot"]') %>%
  html_text2()

html_text(y)

str(y[39])

xx <- 
  read_html("https://www.retsinformation.dk/eli/lta/2021/2584") %>%
  html_node(xpath = '//*[@class="document-content"]') %>%
  html_text()


test <- read_html("http://www.retsinformation.dk/eli/retsinfo/2007/20064")
test %>%
  html_nodes("h1")

test %>%
  html_nodes("h1") %>%
  html_text()


remDr <- rsDriver(browser='chrome', port=4444L)
browser <- remDr$client
browser$open()
browser$navigate("url")



library("RSelenium")

remDr <- rsDriver(browser='chrome', port=4444L)
browser <- remDr$client
browser$open()
browser$navigate("url")







