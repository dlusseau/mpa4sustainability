#######################################################################################
###### MPA4SUSTAINABILITY
########################################################################################

#obtaining the pdfs from Madeira first
# we have no alternative to a brute force scrape as it is a nested list of pdfs.
# note that pdfs pre 1994 (excluding) are non-searchable images, no search possible

#########################################################################################################
 #### harvesting
 
setwd("F:/Madeira_law/")


library(tidyverse)
library(rvest)

pages<-c("1serie/","2serie/","3serie/","4serie/")

for p( in 1:length(pages)) {

 JORAM<-read_html(paste0("https://joram.madeira.gov.pt/joram/",pages[p])
 
 folder.list<-JORAMserI%>%html_nodes("pre") %>% html_nodes("pre") %>% html_nodes("a")%>%html_text("href")
 
 folder.retain<-folder.list[grep("Ano",folder.list)] # gets ride of parent folders
 
 folder.retain<-folder.retain[1:grep("1994",folder.retain)] # gets ride of pre 1994
 
 URLs<-paste0(JORAM,folder.retain)
 
 URLs<-gsub(" ","%20",URLs)
 
 for (i in length(URLs)) {
 
 folder<-read_html(URLs[i])
 file.list<-folder%>%html_nodes("pre") %>% html_nodes("pre") %>% html_nodes("a")%>%html_text("href")
 file.retain<-file.list[grep("pdf",file.list)] #only the pdf files
 file.url.retain<-paste0(URLs[i],file.retain) 
 mapply(function (x,y) download.file(x,y,mode="wb"), file.url.retain, paste0("F:/Madeira_law/",pages[p],file.retain))
 
 
 } #end URLs
 
 } #end pages
 
 
 #########################################################################################################
 #### processing
 
setwd("F:/Madeira_law/")
pages<-c("1serie/","2serie/","3serie/","4serie/")


library(tidyverse)
library(pdftools)
library(pdfsearch)

for p( in 1:length(pages)) {
files<-list.files(pages[p],full.names=T)

result <- keyword_search(paste0("F:/Madeira_law/",files[1]), keyword = c('biodiversidade'),path = TRUE)
result$file<-files[1]

for (f in 2:length(files)) {
temp<-keyword_search(paste0("F:/Madeira_law/",files[f]), keyword = c('biodiversidade'),path = TRUE)
temp$file<-files[f]
result<-rbind(result,temp)

} #end files

} #end page
 
 
 