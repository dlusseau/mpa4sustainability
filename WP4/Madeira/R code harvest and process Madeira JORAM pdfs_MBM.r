#######################################################################################
###### MPA4SUSTAINABILITY
########################################################################################

#obtaining the pdfs from Madeira first
# we have no alternative to a brute force scrape as it is a nested list of pdfs.
# note that pdfs pre 1994 (excluding) are non-searchable images, no search possible

#########################################################################################################
 #### harvesting
 

mydir<-"F:/Madeira_law/"

setwd(mydir)


library(tidyverse)
library(rvest)

pages<-c("1serie/","2serie/","3serie/","4serie/")

#in series II there is a file that gave an error 
#"https://joram.madeira.gov.pt/joram/2serie/Ano%20de%202008/IISerie-053-2008-03-17Supl.pdf"
#we need to download it manually 



for (p in 1:length(pages)) {

  JORAM<-paste0("https://joram.madeira.gov.pt/joram/",pages[p])
  JORAMser<-read_html(paste0("https://joram.madeira.gov.pt/joram/",pages[p]))
 
 folder.list<-JORAMser%>%html_nodes("pre") %>% html_nodes("pre") %>% html_nodes("a")%>%html_text("href")
 
 folder.retain<-folder.list[grep("Ano",folder.list)] # gets ride of parent folders
 
 # folder.retain<-folder.retain[1:grep("Ano de 1994",folder.retain)] # gets ride of pre 1994
 
 URLs<-paste0(JORAM,folder.retain)
 
 URLs<-gsub(" ","%20",URLs)
 
 dir.create(paste0(mydir,pages[p]))
 # ano23<-read_html(paste0("https://joram.madeira.gov.pt/joram/1serie/Ano%20de%202023/"))
 
 for (i in 1:length(URLs)) {
 
 folder<-read_html(URLs[i])
 file.list<-folder%>%html_nodes("pre") %>% html_nodes("pre") %>% html_nodes("a")%>%html_text("href")
 file.retain<-file.list[grep("pdf",file.list)] #only the pdf files
 file.retain<-gsub(" ","%20",file.retain)
 #to remove the file with an error
  if(p==2 & i==17){
   file.retain[-420]
 }
 file.url.retain<-paste0(URLs[i],file.retain) 
 dir.create(paste0(mydir,pages[p],folder.retain[i]))
 mapply(function (x,y) download.file(x,y,mode="wb"), file.url.retain, paste0(mydir,pages[p],folder.retain[i],file.retain))
 
    } #end URLs
 
} #end pages




 #########################################################################################################
 #### processing
 
setwd("F:/Madeira_law/")
mydir<-"F:/Madeira_law/"
pages<-c("1serie/","2serie/","3serie/","4serie/")


library(tidyverse)
library(pdftools)
library(pdfsearch)

for (p in 1:length(pages)) {

folders<-list.dirs(pages[p], full.names = TRUE, recursive = FALSE)   

for (i in 1:length(folders)) { 
files<-list.files(folders[i],full.names=T)

result <- keyword_search(paste0(mydir,files[i]), keyword = c('biodiversidade'),path = TRUE)
result$file<-files[i]

for (f in 2:length(files)) {
temp<-keyword_search(paste0(mydir,files[f]), keyword = c('biodiversidade'),path = TRUE)
temp$file<-files[f]
result<-rbind(result,temp)


} #end files

} #end folders

} #end page
 
 
  
 
 
