

text<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.EUSElinks.NOTFINISHED.csv")
code<-read.csv("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.Removal.codelist.csv")

# Ok i made some slight edits 
# 1) makes the code detection at a document sentence level instead of just document level,
#    this ensure the title celex and bad codes are grouped at a more reference level. 
# 2) # I noticed that if none of the bad codes were found it returns empty row instead of the good sentence,
#     so made an if-else statment to put in the loop. 

text1 <- 
  text %>%
  mutate(doc.sentence.id = paste(doc.id,element_id,sentence_id, sep = "_"))

se.doc<-unique(text1$doc.sentence.id)

#let's expand code
code.exp<-code[1,]

#a naughty way to grow a data frame in a loop, to be avoided (eg have a proper memory allocation step instead) if the data.frame was much bigger (>2 orders of magnitude)

for (i in 2:dim(code)[1]) {
  
  removal<-unlist(strsplit(code$removal.codes[i],", "))
  
  temp<-data.frame(celex=rep(code$celex[i],length(removal)),removal.codes=removal)
  code.exp<-rbind(code.exp,temp)
  
}

#now we have a unique row for celex - removal codes

se.doc<-unique(text1$doc.sentence.id)

i=1
temp<-text1[text1$doc.sentence.id==se.doc[i],]

bad.celex<-temp$celex[which(temp$celex%in%code$celex)] #those are the "bad" celex codes in this text?
remove.temp<-unique(code.exp$removal.codes[which(code.exp$celex%in%bad.celex)]) #those are the associated removal codes

#text.clean<-temp[-which(temp$code%in%remove.temp),] #we find the codes that match the removal codes and remove those rows

#Anna's edits:
# ok if none of the bad codes were found it returns empty so made an if else statment to put in the loop

if(dim(temp[-which(temp$code%in%remove.temp),])[1]==0){
  text.clean <-temp
}else{
text.clean<-temp[-which(temp$code%in%remove.temp),] #we find the codes that match the removal codes and remove those rows
}

#data frame initialised

for (i in 2:length(se.doc)) {
  
  temp<-text1[text1$doc.sentence.id==se.doc[i],]
  
  bad.celex<-temp$celex[which(temp$celex%in%code$celex)] #those are the "bad" celex codes in this text?
  remove.temp<-unique(code.exp$removal.codes[which(code.exp$celex%in%bad.celex)]) #those are the associated removal codes
 
   if(dim(temp[-which(temp$code%in%remove.temp),])[1]==0){
     text.clean <-rbind(text.clean,temp)
   }else{

  text.clean<-rbind(text.clean,temp[-which(temp$code%in%remove.temp),]) #we find the codes that match the removal codes and remove those rows
   }
  
}

write.csv(text.clean,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/MPA4Sustainability/WP4/sweden/02.EUSElinks.NOTFINISHED_removal_code_removed.csv")