#############################################################################
#                   Harverst Diaraio da Republica                           #
#############################################################################
# 
# general webscrapping
# https://afit-r.github.io/scraping_HTML_text
# we have a static website
# help from : https://www.appsilon.com/post/webscraping-dynamic-websites-with-r
# go to the links
# https://tim-tiefenbach.de/post/2023-web-scraping/

################################################################################
# libraries


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
library("binman")
library("wdman")
library("purrr")
library("openxlsx")

################################################################################
# Functions

#load to following pages function
next_page <- function(browser) {
  # Find the "Load more" button by its CSS selector and ...
  next_page_button <- browser$findElement(using = "css selector", "#next_page img")
  # ... click it
  next_page_button$clickElement()
  # give the website a moment to respond
  
}

#b27-PaginationContainer > button:nth-child(3)

# load until finished


################################################################################

# Set wd ---------------------------------------------------------------

dir_resul <- "E:/MBM/github/Madeira/data/raw_data/diario_da_republica/"

#make R Selenium to download the chromedriver available
RSelenium::rsDriver(browser = "chrome",
                    chromever = "latest_compatible")


binman::list_versions("chromedriver")

# $win32
# [1] "113.0.5672.63" "114.0.5735.16" "114.0.5735.90" "122.0.6261.11"

#if it is not the one that you are using go to https://googlechromelabs.github.io/chrome-for-testing/#stable
#and donwload the one that you need

#make sure you have chrome closed before the next step
remDr <- rsDriver(browser='chrome', port=free_port(), check = FALSE, 
                  chromever='122.0.6261.11')

#if the previous line is not working
remDr <- rsDriver(browser='chrome', port=4455L, check = FALSE, 
                  chromever='122.0.6261.11')


#it it is still not working and later go back again to it
# out_ret_command <- selenium(retcommand=T)
# ver_to_unlink <- gsub("[^0-9.-]", "",gsub(".*win32(.+)LICENSE.chromedriver.*", "\\1", out_ret_command))
# unlink_str <- paste("C:/Users/rutel/AppData/Local/binman/binman_chromedriver/win32/",ver_to_unlink,"/LICENSE.chromedriver", sep = "")
# unlink(unlink_str)
# remDr <- rsDriver(browser='chrome', port=4455L, check = FALSE, 
#                   chromever='122.0.6261.11')

browser <- remDr$client
# browser$open()

browser$navigate("https://dre.tretas.org/") #need to wait until browser is open


#we need to include the keyword we want to look for
keyword<-"turismo"
keyword_buttom <-browser$findElement(using = "css selector", "#id_query_string")
keyword_buttom$sendKeysToElement(list(keyword))

search_buttom <-browser$findElement(using = "css selector", ".big_center")
search_buttom$clickElement() #need to wait until browser is open

order_buttom <-browser$findElement(using = "css selector", ".nobreak:nth-child(2) a")
order_buttom$clickElement() #need to wait until browser is open

#select the number of items per page manually

# Now we get the page source and use rvest to parse it
page_source <- browser$getPageSource()[[1]]
html <- read_html(page_source) 

# another option
# pagesource <- browser$getPageSource()
# html <- read_html(pagesource[[1]],options = "HUGE")

name.text <-1
    # get the text out of this webpage
    for(i in 1:10){
    name.css <- paste0('#search_results > ul > li:nth-child(',i,')')
    name.text[i] <-
      html%>%
      html_nodes(css = name.css) %>%
      html_text(trim = TRUE)
    
    }


#we need to separate into columns an order the text
text_col <-  data.frame(do.call(rbind , str_split(name.text , "\n")))
text_col$X4 <- gsub("[-]", "", text_col$X4) #remove special characters
text_col$X3 <- str_trim(text_col$X3) #remove spaces
text_col$X4 <- str_trim(text_col$X4) #remove spaces
text_col$title <- paste(text_col$X3, text_col$X4, sep=" ") #merge two columns
text_col$X1 <-  substr(text_col$X1,1,nchar(text_col$X1)-2) #substract the - at the end of the date

    if("X7" %in%  names(text_col) == FALSE){
    #extract versÃ£o consolidada
    text_col$X7 <- NA
    } # text_col$X1

#merge into a dataframe and rename the columns
legal_text_only <- cbind (title=text_col$title, admin=text_col$X5, summary=text_col$X7, date=text_col$X1)

#check results
head(legal_text_only)

#get the links
text.link <-
  html%>%
  html_nodes(css = ".result_link")%>%
  html_attr("href")

full_tretas_link <- paste0("https://dre.tretas.org", text.link)

#merge into a dataframe and rename the columns
legal_text <- cbind (legal_text_only, link_tretas=full_tretas_link)

#check results
head(legal_text)

# Use Xpath to scrape the number of results in the page
search_resul <- html |>
  html_nodes(".result_bar div") |>
  html_text()

#get the numbers from the result
matches <- regmatches(search_resul, gregexpr("[[:digit:]]+", search_resul))
as.numeric(unlist(matches))
search_page<- matches %>%  map_chr(c(2)) 

#search through the rest of the pages
  for(j in 2:search_page){
  # # load more content even if it throws an error
  # tryCatch({
    # call load_more()
    next_page(browser)
    
    Sys.sleep(2)
    # Now we get the page source and use rvest to parse it
    page_source <- browser$getPageSource()[[1]]
    html <- read_html(page_source)
    
    Sys.sleep(2)

    name.text <-1
    
   
     # get the text out of this webpage
        for(i in 1:10){
           tryCatch({
        name.css <- paste0('#search_results > ul > li:nth-child(',i,')')
        name.text[i] <-
          html%>%
          html_nodes(css = name.css) %>%
          html_text(trim = TRUE)
           }, error=function(e){cat("ERROR : ", conditionMessage(e), "|n")}) #end trycatch 
              }
   
    #we need to separate into columns an order the text
    text_col <-  data.frame(do.call(rbind , str_split(name.text , "\n")))
    text_col$X4 <- gsub("[-]", "", text_col$X4) #remove special characters
    text_col$X3 <- str_trim(text_col$X3) #remove spaces
    text_col$X4 <- str_trim(text_col$X4) #remove spaces
    text_col$title <- paste(text_col$X3, text_col$X4, sep=" ") #merge two columns
    text_col$X1 <-  substr(text_col$X1,1,nchar(text_col$X1)-2) #substract the - at the end of the date
    
    if("X7" %in%  names(text_col) == FALSE){
    #extract versÃ£o consolidada
    text_col$X7 <- NA
    } # text_col$X1

    #merge into a dataframe and rename the columns
    legal_text_only <- cbind (title=text_col$title, admin=text_col$X5, summary=text_col$X7, date=text_col$X1)
    
    
    text.link <-
      html%>%
      html_nodes(css = ".result_link")%>%
      html_attr("href")
    
    full_tretas_link <- paste0("https://dre.tretas.org", text.link)
    
    #merge into a dataframe and rename the columns
    legal_text_temp <- cbind (legal_text_only, link_tretas=full_tretas_link)
    
        
    #merge all
    all_legal_text <- rbind(legal_text, legal_text_temp)
    legal_text <- all_legal_text
  #   # if no error is thrown, call the next_page_completely() function again
  #   Recall(browser)
  # }, error = function(e) {
  #   # if an error is thrown return nothing / NULL
  # })
  }

# all_legal_text_result<-as.data.frame(all_legal_text)
# 
# openxlsx::write.xlsx(all_legal_text_result, paste0(keyword, ".xlsx"),
#                      asTable = TRUE)

#find year and series
all_legal_text$year <-  str_sub(all_legal_text$name, -10, -7)
all_legal_text$series <-  str_extract(all_legal_text$name, "(?i)Série I++")

setwd(dir_resul)
name_Rfile <- paste0('result_', keyword, "_diario_da_republica_Madeira.RData")
save(all_legal_text, file=name_Rfile)



# load(name_Rfile)
