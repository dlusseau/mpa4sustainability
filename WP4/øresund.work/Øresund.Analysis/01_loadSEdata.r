
# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 


# Load data --------------------------------------------------------------------

library("httr")
library("jsonlite")
library("stringi")
library("stringr")
library("rvest")
library("gtools")
library("plyr")
library("dplyr")

# Made the api queries from there builder:
# https://data.riksdagen.se/dokumentlista/
# but we needed to further adjust it so it ali included the https://www.riksdagen.se/sv/Dokument-Lagar/ filter of Document status which wasnt an option in the builder but we also put it in 
# filter set to have the search word, document type is Svensk författningssamling (SFS) the output was clicked to be in JSON format
  
# "fiske" query ------------------------------------------------------------------

Fiske.API.URL <-
 "https://data.riksdagen.se/dokumentlista/?sok=%22fiske%22&dokstat=g%C3%A4llande+sfs&doktyp=SFS&rm=&from=&tom=&ts=&bet=&tempbet=&nr=&org=&iid=&avd=&webbtv=&talare=&exakt=&planering=&facets=&sort=rel&sortorder=desc&rapport=&utformat=json&a=s#soktraff"

fiske.raw.data <- GET(Fiske.API.URL)
fiske.raw.data$status_code # 200 means it is ok :)
names(fiske.raw.data)

stop_for_status(fiske.raw.data)

fiske.data.list <- stri_encode(as.raw(fiske.raw.data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding

fiske.data.object <- fromJSON(fiske.data.list)

fiske.df <- fiske.data.object$dokumentlista$dokument

# this is only the first page.... need to figure out how to get the other pages each page as 20 results.
# someone else has this Q on stackoverflow: https://stackoverflow.com/questions/54575231/a-continuation-of-extracting-data-from-an-api-using-r 

No.pgs <- as.numeric(fiske.data.object[["dokumentlista"]][["@sidor"]])

nextpage <- fiske.data.object[["dokumentlista"]][["@nasta_sida"]]

# now we know the number of pages for that query so lets make a loop to get the results

full.fisk.df <- NULL

for (i in seq(No.pgs)) {

Fiske.res.url <- str_sub(nextpage, start = 1L, end = -2L) # remove the last character which is the nxt pg number

nxt.pg.url <- paste0(Fiske.res.url,i, sep="") #put the pg number based on the loop iteration number

fiske.raw.data <- GET(nxt.pg.url)
print(fiske.raw.data$status_code)

stop_for_status(fiske.raw.data) # convets http errors to R errors (if we encounter one)
  
fiske.data.list <- stri_encode(as.raw(fiske.raw.data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding
  
fiske.data.object <- fromJSON(fiske.data.list, flatten = TRUE)
  
fiske.df <- as.data.frame(fiske.data.object$dokumentlista$dokument, row.names = NULL)

full.fisk.df <- rbind.fill(full.fisk.df,fiske.df) # use r.bindfill since some had diff columns so rbind will not allow

print(i)

}

## ---------- now get the text from the url given in the df ---------- #

# need to make a loop with all the data like we did for dk text

# found these problem urls yesterday Oct. 20th. one of them the reason is the space value changes once in the web but the other is just faulty 
# for now I will remove the faulty text url and edit the other two so the work. 

#https://data.riksdagen.se/dokument/sfs-1736-0123 1.text after I copy this into chrome it changes to: https://data.riksdagen.se/dokument/sfs-1736-0123%201 # for both the space is replaced with %20
#https://data.riksdagen.se/dokument/sfs-1910-72 s.1.text --> this is just a faulty link in the online search it comes up as a result but says no document is found...

#just to check that these are the only url with spaces... and they are
full.fisk.df %>%
  mutate(detect = str_detect(dokument_url_text, " ")) %>%
  select(detect,dokument_url_text)%>%
  filter(detect == TRUE)
#detect                                 dokument_url_text
#   TRUE //data.riksdagen.se/dokument/sfs-1736-0123 1.text
#   TRUE //data.riksdagen.se/dokument/sfs-1910-72 s.1.text 

# totally remove the faulty link and then edit the two to be the correct path
full.fisk.df1 <- 
  full.fisk.df %>%
  filter(dokument_url_text != "//data.riksdagen.se/dokument/sfs-1910-72 s.1.text") %>%
  mutate(dokument_url_text = str_replace_all(dokument_url_text, " ", "%20"))

URLs <- paste0("https:",full.fisk.df1[,18], sep="") #put https: infront of the url

doc.id <- full.fisk.df1[,16]


SK.fisketext.list <- structure(vector("list", 113), names=doc.id)

for (i in seq(URLs)) {
  
  html <- read_html(URLs[i], options = "HUGE")
  
  SK.fisketext.list[[i]] <-
    html%>%
    html_nodes("body") %>%
    html_text2()
  
  print(URLs[i]) # Print the URL we are on
  print(i)       # Print what iteration we are on
}

# jakt query -----------

jakt.API.URL <-
  "https://data.riksdagen.se/dokumentlista/?sok=%22jakt%22&dokstat=g%C3%A4llande+sfs&doktyp=SFS&rm=&from=&tom=&ts=&bet=&tempbet=&nr=&org=&iid=&avd=&webbtv=&talare=&exakt=&planering=&facets=&sort=rel&sortorder=desc&rapport=&utformat=json&a=s#soktraff"

jakt.raw.data <- GET(jakt.API.URL)
jakt.raw.data$status_code # 200 means it is ok :)
names(jakt.raw.data)

stop_for_status(jakt.raw.data)

jakt.data.list <- stri_encode(as.raw(jakt.raw.data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding

jakt.data.object <- fromJSON(jakt.data.list)

jakt.df <- jakt.data.object$dokumentlista$dokument

No.pgs <- as.numeric(jakt.data.object[["dokumentlista"]][["@sidor"]])

nextpage <- jakt.data.object[["dokumentlista"]][["@nasta_sida"]]


# now we know the number of pages for that query so lets make a loop to get the results

full.jakt.df <- NULL

for (i in seq(No.pgs)) {
  
  jakt.res.url <- str_sub(nextpage, start = 1L, end = -2L) # remove the last character which is the nxt pg number
  
  nxt.pg.url <- paste0(jakt.res.url,i, sep="") #put the pg number based on the loop iteration number
  
  jakt.raw.data <- GET(nxt.pg.url)
  print(jakt.raw.data$status_code)
  
  stop_for_status(jakt.raw.data) # convets http errors to R errors (if we encounter one)
  
  jakt.data.list <- stri_encode(as.raw(jakt.raw.data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding
  
  jakt.data.object <- fromJSON(jakt.data.list, flatten = TRUE)
  
  jakt.df <- as.data.frame(jakt.data.object$dokumentlista$dokument, row.names = NULL)
  
  full.jakt.df <- rbind.fill(full.jakt.df,jakt.df)
  
  print(i)
  
}

## ---------- now get the text from the url given in the df ---------- #

doc.id <- full.jakt.df[,16]

SK.jakttext.list <- structure(vector("list", 35), names=doc.id)

URLs <- paste0("https:",full.jakt.df[,18], sep="") #put https: infront of the url

for (i in seq(URLs)) {
  
  html <- read_html(URLs[i], options = "HUGE")
  
  SK.jakttext.list[[i]] <-
    html%>%
    html_nodes("body") %>%
    html_text2()
  
  print(URLs[i]) # Print the URL we are on
  print(i)       # Print what iteration we are on
}


# sjöfart query -----------

sjofart.API.URL <-
  "https://data.riksdagen.se/dokumentlista/?sok=%22sj%C3%B6fart%22&dokstat=g%C3%A4llande+sfs&doktyp=SFS&rm=&from=&tom=&ts=&bet=&tempbet=&nr=&org=&iid=&avd=&webbtv=&talare=&exakt=&planering=&facets=&sort=rel&sortorder=desc&rapport=&utformat=json&a=s#soktraff"

  
sjofart.raw.data <- GET(sjofart.API.URL)
sjofart.raw.data$status_code # 200 means it is ok :)
names(sjofart.raw.data)

stop_for_status(sjofart.raw.data)

sjofart.data.list <- stri_encode(as.raw(sjofart.raw.data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding

sjofart.data.object <- fromJSON(sjofart.data.list)

sjofart.df <- sjofart.data.object$dokumentlista$dokument

No.pgs <- as.numeric(sjofart.data.object[["dokumentlista"]][["@sidor"]])

nextpage <- sjofart.data.object[["dokumentlista"]][["@nasta_sida"]]

# now we know the number of pages for that query so lets make a loop to get the results

full.sjofart.df <- NULL

for (i in seq(No.pgs)) {
  
  sjofart.res.url <- str_sub(nextpage, start = 1L, end = -2L) # remove the last character which is the nxt pg number
  
  nxt.pg.url <- paste0(sjofart.res.url,i, sep="") #put the pg number based on the loop iteration number
  
  sjofart.raw.data <- GET(nxt.pg.url)
  print(sjofart.raw.data$status_code)
  
  stop_for_status(sjofart.raw.data) # convets http errors to R errors (if we encounter one)
  
  sjofart.data.list <- stri_encode(as.raw(sjofart.raw.data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding
  
  sjofart.data.object <- fromJSON(sjofart.data.list, flatten = TRUE)
  
  sjofart.df <- as.data.frame(sjofart.data.object$dokumentlista$dokument, row.names = NULL)
  
  full.sjofart.df <- rbind.fill(full.sjofart.df,sjofart.df)
  
  print(i)
  
}


## ---------- now get the text from the url given in the df ---------- #

#just to check that there are urls with spaces

full.sjofart.df %>%
  mutate(detect = str_detect(dokument_url_text, " ")) %>%
  select(detect,dokument_url_text)%>%
  filter(detect == TRUE)

full.sjofart.df1 <- 
  full.sjofart.df  

doc.id <- full.sjofart.df1[,16]

SK.sjofart.list <- structure(vector("list", 126), names=doc.id)

URLs <- paste0("https:",full.sjofart.df1[,18], sep="") #put https: infront of the url

for (i in seq(URLs)) {
  
  html <- read_html(URLs[i], options = "HUGE")
  
  SK.sjofart.list[[i]] <-
    html%>%
    html_nodes("body") %>%
    html_text2()
  
  print(URLs[i]) # Print the URL we are on
  print(i)       # Print what iteration we are on
}


# Save ---------------------------------------------------------------------------------------

# dfs of result document data
full.sjofart.df1 <- full.sjofart.df1 %>% select(-filbilaga.fil) # this column the others do not have and it is no nec. for us so will remove it
write.csv(full.sjofart.df1, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.sjofart.df.csv", row.names=FALSE)
write.csv(full.jakt.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.jakt.df.csv", row.names=FALSE)

full.fisk.df2 <- full.fisk.df1 %>% select(-filbilaga.fil) # this column the others do not have and it is no nec. for us so will remove it
write.csv(full.fisk.df2, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.fisk.df.csv", row.names=FALSE)

# list of result text
saveRDS(SK.sjofart.list, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.sjofart.list" )
saveRDS(SK.jakttext.list, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.jakttext.list" )
saveRDS(SK.fisketext.list, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.fisketext.list" )





