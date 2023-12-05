#######################################################################################
###### MPA4SUSTAINABILITY
########################################################################################

# reading with OCR the pdf files that are images

#########################################################################################################

library(tesseract)
library(pdftools)
library(tidyverse)
library(rvest)



mydir<-"F:/Madeira_law/"
setwd("F:/Madeira_law/")

#downloading portuguese
# tesseract_download("por") #we only need to download it once

#check that is installed
tesseract_info() 

# Now load the dictionary
port <- tesseract("por", options = list(tessedit_pageseg_mode = 1))


#load the pdf
pages<-c("1serie/","2serie/","3serie/","4serie/")


for (p in 1:length(pages)) {

folders<-list.dirs(pages[p], full.names = TRUE, recursive = FALSE) 
 #different series had different years with scan images
  if(pages[p]=="1serie/"){
   folder.retain<-folders[1:grep("Ano de 1994",folders)] # gets ride of pre 1994
   }# end 1series
  if(pages[p]=="2serie/"){
   folder.retain<-folders[1:grep("Ano de 1999",folders)] # gets ride of pre 1999
   }# end 2series
  if(pages[p]=="3serie/"){
   folder.retain<-folders[1:grep("Ano de 2001",folders)] # gets ride of pre 2001
   }# end 3series
  if(pages[p]=="4serie/"){
   folder.retain<-folders[1:grep("Ano de 2000",folders)] # gets ride of pre 2000
   }# end 4series

for (i in 1:length(folder.retain)) { 
files<-list.files(folder.retain[i],"*.pdf",full.names=T)

# files<-list.files(mydir,full.names=T)
for (f in 1:length(files)){
  pngfile <- pdftools::pdf_convert(files[f], dpi = 600)
  text <- tesseract::ocr(pngfile, engine = port)
  #leave out the hypenitation, unify the parragraph
  text.retain <- gsub('-\n','',text)
  text.retain <- gsub('\n\n','\r\r',text.retain)
  text.retain <- gsub('\n',' ',text.retain)
  text.retain <- gsub('\r\r','\n\n',text.retain)
  unlink(pngfile)
  # cat(text)
  titl<-gsub(mydir, '', files[f])
  titl<-gsub('.pdf', '', titl)
  cat(text.retain, file = paste0(titl,".txt"))
  # df <- data.frame("text" = text.retain)
  
} #end files

} #end folders

} #end page
 

