
library(httr)
library(jsonlite)
library(stringi)

# "Fiske" query
Fiske.API.URL <-
"https://data.riksdagen.se/dokumentlista/?sok=%22fiske%22&doktyp=SFS&rm=&from=&tom=&ts=&bet=&tempbet=&nr=&org=&iid=&avd=&webbtv=&talare=&exakt=&planering=&facets=&sort=rel&sortorder=desc&rapport=&utformat=json&a=s#soktraff"
  
raw_data <- GET(Fiske.API.URL)
raw_data$status_code # 200 means it is ok :)
names(raw_data)

stop_for_status(raw_data)

data_list <- stri_encode(as.raw(raw_data$content), 'UTF-8') #data_list <- rawToChar(raw_data$content) --> same as this but takes into account the encoding

data_object <- fromJSON(data_list)

datafram <- data_object$dokumentlista$dokument

# this is only the first page.... need to figure out how to get the other pages each page as 20 results. says there is like around 200 hits...
# someone else has this Q on stackoverflow: https://stackoverflow.com/questions/54575231/a-continuation-of-extracting-data-from-an-api-using-r 

#this is the next page node: loop around this
#"@nasta_sida": "https://data.riksdagen.se/dokumentlista/?a=s&utformat=json&sok=%22fiske%22&doktyp=SFS&sort=rel&sortorder=desc&p=2",
#"@traff_till": "20", # hits per page
#"@traffar": "261", # total hits

result.pgs <- as.numeric(data_object[["dokumentlista"]][["@traffar"]])/as.numeric(data_object[["dokumentlista"]][["@traff_till"]])
#need to round up to the closes integer
result.pgs <- ceiling(result.pgs)
# we have 14 pages of search results

