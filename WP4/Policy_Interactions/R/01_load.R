
# Clear work space ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------

library("eurlex")
library("dplyr")
library("tibble")
library("stringr")
library("purrr")

# Define functions --------------------------------------------------------

#this isnt working :( 
#source(file = "/WP4/Policy_Interactions/R/freetext_eurlex")

require(rvest)

freetext_eurlex<- function(text,act="DIR",lang="en",exactly=FALSE) {
  if (is.character(text)) {
    text<-gsub(" ", "+",text)
    site<-paste0("https://eur-lex.europa.eu/search.html?scope=EURLEX&lang=",lang,"&text=")
    session<-paste0("&lang=en&type=quick&qid=1662534219143&FM_CODED=",act,"&page=")
    
    #page 1
    
    if (exactly==FALSE) {
      query<-paste0(site,text,session,"1") 
    } else {
      query<-paste0(site,'"',text,'"',session,"1") 
    }
    
    result<-read_html(query)
    nodes<- html_nodes(result, "[class='SearchResult']")
    flat<-unlist(strsplit(html_element(nodes,"[class='col-sm-6']")%>%html_text2(),"\n"))
    CELEX<-flat[which(flat=="CELEX number:")+1]
    
    #how many pages by default 10 results per page
    returns<-html_nodes(result, "[class='checkbox']")%>%html_text2()
    ndocs<-as.numeric(sub(".*of ", "", returns[1]))
    npage<-ceiling(ndocs/10)
    print(paste0("there are ",npage," pages of results"))
    flush.console()
    
    if ((npage>1)&is.na(npage)==FALSE) {
      for (i in 2:npage) {
        
        if (exactly==FALSE) {
          query<-paste0(site,text,session,i) 
        } else {
          query<-paste0(site,'"',text,'"',session,i) 
        }
        
        result<-read_html(query)
        nodes<- html_nodes(result, "[class='SearchResult']")
        flat<-unlist(strsplit(html_element(nodes,"[class='col-sm-6']")%>%html_text2(),"\n"))
        temp<-flat[which(flat=="CELEX number:")+1]
        
        CELEX<-c(CELEX,temp)
        
        
      }
    }
    
    
  } else {
    stop("the query must be a character string, don't forget the quotations marks", call.=TRUE) 
  }
  
  return(CELEX)
}
# Load data ---------------------------------------------------------------

# term eur-lex search ---------------

# query term: marine protected area* (no parenthesis!)

# Resource Types we want with associated list of act codes (FM_CODE) 
  # Directives: DIR
  # Legislative acts: ACT_LEGIS --> issues about the leg acts: https://eur-lex.europa.eu/search.html?lang=en&text=marine+protected+area*&qid=1662553896352&type=quick&scope=EURLEX&FM_CODED=ACT_LEGIS
  # Regulation: REG
  # Delegated regulation: REG_DEL
  # Delegated act: Cannot find it seems like the code is ACT_DEL, but no documents associated  
  # National implementations Cannot find it...
  # Decisions adopted by bodies created by international agreements: ACT_ADOPT_INTERNATION 
  # Treaties: TREATY
  # Convention: CONVENTION
  # other acts: ACT_OTHER

resource.types <- c("DIR","REG_DEL","REG", "ACT_LEGIS",
                    "DEC_ADOPT_INTERNATION","TREATY", 
                    "CONVENTION","ACT_OTHER")

mpaCELEX.list<-list()

for (i in 1:length(resource.types)) {  
  mpaCELEX.list[[i]]<- 
    freetext_eurlex("marine protected area*",
                    act=resource.types[[i]],
                    lang="en",
                    exactly=FALSE)

}

# Lets give each list the name based on the resource type: 
mpaCELEX.list <- structure(mpaCELEX.list, names=resource.types)

# Make it into a nice data frame
mpaCELEX.df <- 
  mpaCELEX.list %>%
  unlist(.) %>%
  as.data.frame() %>%
  rownames_to_column(.) %>%
  rename(.,  CELEX = .) %>%
  rename(.,  resource.type = rowname) %>%
  mutate(resource.type = str_extract(resource.type,"[:alpha:]+"))

# We have 774 EU policy documents relating to marine protexted area*

# duplicate CELEX?? shouldn't be since I am guessing a document can 
# only be categorized into one resource types BUT... double check to be sure
mpaCELEX.df[duplicated(mpaCELEX.df$CELEX)]
# no duplicates :) 

mpaCELEX.df <-
  mpaCELEX.df %>%
  mutate(url = paste0("http://publications.europa.eu/resource/celex/",.$CELEX))

# eurlex package search ---------------

#error when trying to do all 774... takes too long...can to ~100 results...
CELEX_text.data <- 
  mpaCELEX.df[1:100,] %>%
  mutate(title = map_chr(url, elx_fetch_data, "title")) %>% 
  mutate(title = map_chr(url, elx_fetch_data, "text")) %>% 
  as_tibble() 

