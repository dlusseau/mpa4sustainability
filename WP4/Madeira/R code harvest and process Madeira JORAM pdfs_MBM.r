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
# setwd("//192.168.10.34/Share_MBM_Staff/mpa4sutainability/WP4/github/Madeira/Madeira_law/")
setwd(mydir)
pages<-c("1serie/","2serie/","3serie/","4serie/")


library(tidyverse)
library(pdftools)
library(pdfsearch)
library(tokenizers)

keyword = c('mergulho')
pages<-c("1serie/","2serie/","3serie/","4serie/")

#create empty data.frame
result.txt<-data.frame()

for (p in 1:length(pages)) {
  
  folders<-list.dirs(pages[p], full.names = TRUE, recursive = FALSE)   
  
  
  
  for (i in 1:length(folders)) {
    # Create an empty data frame to save the keyworkds found
    # result.txt <- data.frame()
    # result <- data.frame()
    
    files<-list.files(folders[i],full.names=T)
    
    # Filter txt files
    txt_files <- files[tools::file_ext(files) == "txt"]
    
    
    if(length(txt_files) > 0){
      # Iterate over txt files and check for corresponding pdf files
      for (j in 1:length(txt_files)) {
        # Extract the file name without extension
        file_name <- tools::file_path_sans_ext(txt_files[j])
        
        # Check if there is a corresponding pdf file
        pdf_file <- file.path(folders[i], paste0(file_name, ".pdf"))
        
        if (any(basename(pdf_file) == list.files(folders[i]))) {
          # # Process the txt and pdf files
          # Specify the path to your text file
          file_path <- paste0(mydir,txt_files[j])
          
          # Read the contents of the text file into a character vector
          text_content <- readLines(file_path)
          
          # Use grep to find lines containing the keyword
          result.txt.key <- grep(keyword, text_content, ignore.case = FALSE)
          
          if((length(result.txt.key)==0)){
            cat("No keyword found for", txt_files[j], "\n")
          } else {
            # Read the text file line by line
            text_lines <- readLines(file_path, warn = FALSE)
            
            # Initialize a variable to store the paragraphs containing the keyword
            target_paragraphs <- character()
            
            # Flag to indicate if the current paragraph contains the keyword
            # in_target_paragraph <- FALSE
            file.name<-txt_files[j]
            
            # Iterate through each line
            for (d in 1:length(text_lines)) {
              # Check if the line contains the keyword
              if (grepl(keyword, text_lines[d])==TRUE) {
                # in_target_paragraph <- TRUE
                # Add the line to the current paragraph
                target_paragraphs <-  text_lines[d]
                line_num<-d
                token_text<-NA
                #create a new row to add
                result.txt.temp <- data.frame (keyword=keyword, page_num=1, line_num=line_num, line_text=target_paragraphs, 
                                               token_text=token_text, file=file.name)
              }#end text lines TRUE
            } #end text lines ALL
            result.txt<-rbind(result.txt.temp, result.txt)
            assign (paste0('result_txt_',str_sub(pages[p], 1, 6),"_", str_sub(folders[i], -11, -1)), result.txt)
          } #end else
        }else{
          # for (p in 1:length(files)) {
          result <- keyword_search(paste0(mydir,files[1]), keyword = keyword,path = TRUE, 
                                   ignore_case=TRUE)
          result$file<-files[1]
          
          for (w in 2:length(files)) {
            temp<-keyword_search(paste0(mydir,files[w]), keyword = keyword,path = TRUE, 
                                 ignore_case=TRUE)
            temp$file<-files[w]
            result<-rbind(result,temp)
          } #end temp
          # } #end files 
          #rename the object
          #rename the tibble if not empty
          assign (paste0('result_',str_sub(pages[p], 1, 6),"_", str_sub(folders[i], -11, -1)), result)
          
        } #else
        
      } #end txt files for
    } #end txt files
    else {
      # for (k in 1:length(files)) {
      result <- keyword_search(paste0(mydir,files[1]), keyword = keyword, path = TRUE, 
                               ignore_case=TRUE)
      result$file<-files[1]
      
      for (f in 2:length(files)) {
        temp<-keyword_search(paste0(mydir,files[f]), keyword = keyword,path = TRUE, 
                             ignore_case=TRUE)
        temp$file<-files[f]
        result<-rbind(result,temp)
      } #end temp
      # } #end files 
      #rename the tibble if not empty
      assign (paste0('result_',str_sub(pages[p], 1, 6),"_", str_sub(folders[i], -11, -1)), result)
      
    } #else
    
  } #end folder
} #end pages 


#save the file
setwd(dir_resul)
list_objects <- ls()
list_objects


# # Find indices of objects starting with "result_text_"
# matching_indices <- which(grepl("^result_text_", list_objects))
# 
# print(matching_indices)

result_txt.name <- list_objects[115:122]

result_txt_temp<-data.frame()
result_txt<-data.frame()

for (j in 1:length(result_txt.name)){
  
  
  result_txt_temp <- get(result_txt.name[j])
  result_txt_temp$year <- str_sub(result_txt_temp$file, 15, 18)
  result_txt_temp$series <- str_sub(result_txt_temp$file, 1, 6)
  
  result_txt <- rbind (result_txt, result_txt_temp)
  
} #result_txt.name

# openxlsx::write.xlsx(result.txt, paste0('result_', keyword, "_old_JORAM_Madeira.csv"),
#               asTable=TRUE)




#get the names in the workspace
# check which ones of the pdfs we want to save
list_names <- list_objects[21:114]


result_JORAM_temp<-data.frame()
result_JORAM<-data.frame()


for (i in 1:length(list_names)){
  
  result_JORAM_temp <- get(list_names[i])
  
  if(nrow(result_JORAM_temp)>0){
    result_JORAM_temp$year <- str_sub(result_JORAM_temp$file, 15, 18)
    result_JORAM_temp$series <- str_sub(result_JORAM_temp$file, 1, 6)
    
    for (k in 2:length(list_names)){
      result_JORAM_temp2 <- get(list_names[k])
      if(nrow(result_JORAM_temp2)>0){
        result_JORAM_temp2$year <- str_sub(result_JORAM_temp2$file, 15, 18)
        result_JORAM_temp2$series <- str_sub(result_JORAM_temp2$file, 1, 6)
        result_JORAM_temp <- rbind (result_JORAM_temp, result_JORAM_temp2) 
      } # end empty          
    } # end folders2
  } # end empty
  result_JORAM <- rbind (result_JORAM, result_JORAM_temp)
} # end list_names

result_JORAM_txt <- as_tibble(result_txt)
result_JORAM_txt$token_text <- tokenize_words(result_JORAM_txt$line_text)

result_JORAM_final <- rbind (result_JORAM, result_JORAM_txt)
# #save the file from the pdfs
# name_file <- paste0('result_', keyword, "_JORAM_Madeira.csv")
# openxlsx::write.xlsx(result_JORAM, name_file,
# asTable=TRUE)
name_Rfile <- paste0('result_', keyword, "_JORAM_Madeira.RData")
save(result_JORAM_final, file=name_Rfile)

# load(name_Rfile)
