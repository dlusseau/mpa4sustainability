
# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Define functions -------------------------------------------------------------

# Load data --------------------------------------------------------------------

library("httr")
library("jsonlite")
library("stringi")
library("stringr")
library("rvest")
library("gtools")
library("plyr")
library("dplyr")

# Using the Swedish Parlaments open data website. Has an API that searches for Svensk författningssamling (SFS). 
# from all the digginig in I have done. I think this is the exact same thing as what is stated on https://lagrummet.se/lagrummet/rattsinformation/lagar-och-forordningar

# Made the api queries from there builder:
# https://data.riksdagen.se/dokumentlista/
  
# "Fiske" query ------------------------------------------------------------------

Fiske.API.URL <-
"https://data.riksdagen.se/dokumentlista/?sok=%22fiske%22&doktyp=SFS&rm=&from=&tom=&ts=&bet=&tempbet=&nr=&org=&iid=&avd=&webbtv=&talare=&exakt=&planering=&facets=&sort=rel&sortorder=desc&rapport=&utformat=json&a=s#soktraff"
  
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

# this works nicely... just need to make a loop with all the data like we did for dk text

#found these problem urls yesterday Oct. 20th. two of them the reason is the space value changes once in the web but the other is just faulty 
# for now I will remove the faulty text url and edit the other two so the work. 

#https://data.riksdagen.se/dokument/sfs-1723-1016 1.text after I copy this into chrome it changes to: https://data.riksdagen.se/dokument/sfs-1723-1016%201.text
#https://data.riksdagen.se/dokument/sfs-1736-0123 1.text after I copy this into chrome it changes to: https://data.riksdagen.se/dokument/sfs-1736-0123%201 # for both the space is replaced with %20
#https://data.riksdagen.se/dokument/sfs-1910-72 s.1.text --> this is just a faulty link in the online search it comes up as a result but says no document is found...

#just to check that these are the only url with spaces... and they are
full.fisk.df %>%
  mutate(detect = str_detect(dokument_url_text, " ")) %>%
  select(detect,dokument_url_text)%>%
  filter(detect == TRUE)

# totally remove the faulty link and then edit the two to be the correct path
full.fisk.df1 <- 
  full.fisk.df %>%
  filter(dokument_url_text != "//data.riksdagen.se/dokument/sfs-1910-72 s.1.text") %>%
  mutate(dokument_url_text = str_replace_all(dokument_url_text, " ", "%20"))
  
URLs <- paste0("https:",full.fisk.df1[,18], sep="") #put https: infront of the url

doc.id <- full.fisk.df1[,16]


SK.fisketext.list <- structure(vector("list", 260), names=doc.id)

for (i in seq(URLs)) {
  
  html <- read_html(URLs[i], options = "HUGE")
  
  SK.fisketext.list[[i]] <-
    html%>%
    html_nodes("body") %>%
    html_text2()
  
  print(URLs[i]) # Print the URL we are on
  print(i)       # Print what iteration we are on
}

# jaga query -----------

jaga.API.URL <-
"https://data.riksdagen.se/dokumentlista/?sok=%22jakt%22&doktyp=SFS&rm=&from=&tom=&ts=&bet=&tempbet=&nr=&org=&iid=&avd=&webbtv=&talare=&exakt=&planering=&facets=&sort=rel&sortorder=desc&rapport=&utformat=json&a=s#soktraff"

jaga.raw.data <- GET(jaga.API.URL)
jaga.raw.data$status_code # 200 means it is ok :)
names(jaga.raw.data)

stop_for_status(jaga.raw.data)

jaga.data.list <- stri_encode(as.raw(jaga.raw.data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding

jaga.data.object <- fromJSON(jaga.data.list)

jaga.df <- jaga.data.object$dokumentlista$dokument

# this is only the first page.... need to figure out how to get the other pages each page as 20 results.
# someone else has this Q on stackoverflow: https://stackoverflow.com/questions/54575231/a-continuation-of-extracting-data-from-an-api-using-r 

No.pgs <- as.numeric(jaga.data.object[["dokumentlista"]][["@sidor"]])

nextpage <- jaga.data.object[["dokumentlista"]][["@nasta_sida"]]


# now we know the number of pages for that query so lets make a loop to get the results

full.jaga.df <- NULL

for (i in seq(No.pgs)) {
  
  jaga.res.url <- str_sub(nextpage, start = 1L, end = -2L) # remove the last character which is the nxt pg number
  
  nxt.pg.url <- paste0(jaga.res.url,i, sep="") #put the pg number based on the loop iteration number
  
  jaga.raw.data <- GET(nxt.pg.url)
  print(jaga.raw.data$status_code)
  
  stop_for_status(jaga.raw.data) # convets http errors to R errors (if we encounter one)
  
  jaga.data.list <- stri_encode(as.raw(jaga.raw.data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding
  
  jaga.data.object <- fromJSON(jaga.data.list, flatten = TRUE)
  
  jaga.df <- as.data.frame(jaga.data.object$dokumentlista$dokument, row.names = NULL)
  
  full.jaga.df <- rbind.fill(full.jaga.df,jaga.df)
  
  print(i)
  
}

## ---------- now get the text from the url given in the df ---------- #

# this works nicely... just need to make a loop with all the data like we did for dk text

doc.id <- full.jaga.df[,16]

SK.jakttext.list <- structure(vector("list", 94), names=doc.id)

URLs <- paste0("https:",full.jaga.df[,18], sep="") #put https: infront of the url

for (i in seq(URLs)) {
  
  html <- read_html(URLs[i], options = "HUGE")
  
  SK.jakttext.list[[i]] <-
    html%>%
    html_nodes("body") %>%
    html_text2()
  
  print(URLs[i]) # Print the URL we are on
  print(i)       # Print what iteration we are on
}


# sjötrafik query -----------

sjotrafik.API.URL <-
"https://data.riksdagen.se/dokumentlista/?sok=%22sj%C3%B6trafik%22&doktyp=SFS&rm=&from=&tom=&ts=&bet=&tempbet=&nr=&org=&iid=&avd=&webbtv=&talare=&exakt=&planering=&facets=&sort=rel&sortorder=desc&rapport=&utformat=json&a=s#soktraff"

sjotrafik.raw.data <- GET(sjotrafik.API.URL)
sjotrafik.raw.data$status_code # 200 means it is ok :)
names(sjotrafik.raw.data)

stop_for_status(sjotrafik.raw.data)

sjotrafik.data.list <- stri_encode(as.raw(sjotrafik.raw.data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding

sjotrafik.data.object <- fromJSON(sjotrafik.data.list)

sjotrafik.df <- sjotrafik.data.object$dokumentlista$dokument

# this is only the first page.... need to figure out how to get the other pages each page as 20 results.
# someone else has this Q on stackoverflow: https://stackoverflow.com/questions/54575231/a-continuation-of-extracting-data-from-an-api-using-r 

No.pgs <- as.numeric(sjotrafik.data.object[["dokumentlista"]][["@sidor"]])

nextpage <- sjotrafik.data.object[["dokumentlista"]][["@nasta_sida"]]

# now we know the number of pages for that query so lets make a loop to get the results

full.sjotrafik.df <- NULL

for (i in seq(No.pgs)) {
  
  sjotrafik.res.url <- str_sub(nextpage, start = 1L, end = -2L) # remove the last character which is the nxt pg number
  
  nxt.pg.url <- paste0(sjotrafik.res.url,i, sep="") #put the pg number based on the loop iteration number
  
  sjotrafik.raw.data <- GET(nxt.pg.url)
  print(sjotrafik.raw.data$status_code)
  
  stop_for_status(sjotrafik.raw.data) # convets http errors to R errors (if we encounter one)
  
  sjotrafik.data.list <- stri_encode(as.raw(sjotrafik.raw.data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding
  
  sjotrafik.data.object <- fromJSON(sjotrafik.data.list, flatten = TRUE)
  
  sjotrafik.df <- as.data.frame(sjotrafik.data.object$dokumentlista$dokument, row.names = NULL)
  
  full.sjotrafik.df <- rbind.fill(full.sjotrafik.df,sjotrafik.df)
  
  print(i)
  
}


## ---------- now get the text from the url given in the df ---------- #

# this works nicely... just need to make a loop with all the data like we did for dk text

# https://data.riksdagen.se/dokument/sfs-1891-35 s.1.text --> faulty link
# totally remove the faulty link and then edit the two to be the correct path
full.sjotrafik.df1 <- 
  full.sjotrafik.df %>%
  filter(dokument_url_text != "//data.riksdagen.se/dokument/sfs-1891-35 s.1.text")

doc.id <- full.sjotrafik.df1[,16]

SK.sjotrafik.list <- structure(vector("list", 39), names=doc.id)

URLs <- paste0("https:",full.sjotrafik.df1[,18], sep="") #put https: infront of the url

for (i in seq(URLs)) {
  
  html <- read_html(URLs[i], options = "HUGE")
  
  SK.sjotrafik.list[[i]] <-
    html%>%
    html_nodes("body") %>%
    html_text2()
  
  print(URLs[i]) # Print the URL we are on
  print(i)       # Print what iteration we are on
}


# Save ---------------------------------------------------------------------------------------

# dfs of result document data
write.csv(full.sjotrafik1.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.sjotrafik.df.csv", row.names=FALSE)
write.csv(full.jaga.df, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.jaga.df.csv", row.names=FALSE)

full.fisk.df2 <- full.fisk.df1 %>% select(-filbilaga.fil) # this column the others do not have and it is no nec. for us so will remove it
write.csv(full.fisk.df2, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.full.fisk.df.csv", row.names=FALSE)

# list of result text
saveRDS(SK.sjotrafik.list, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.sjotrafik.list" )
saveRDS(SK.jakttext.list, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.jakttext.list" )
saveRDS(SK.fisketext.list, file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/01SE.fisketext.list" )





