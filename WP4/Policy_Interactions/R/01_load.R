
# Clear work space ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------

library("eurlex")
library("dplyr")


# Define functions --------------------------------------------------------

#this isnt working :( 
#source(file = "/Users/annajorgensen/Desktop/Anna's R code/mpa4sustainability/WP3/eurlex_precursor_freetext_search")

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

# In eur-lex search ---------------

# query term: marine protected area* (no parenthesis!)

# Resource Types we want with associated list of act codes (FM_CODE) 
  # Directives: DIR
  # Legislative acts: ACT_LEGIS 
  # Regulation: REG
  # Delegated regulation: REG_DEL
  # Delegated act: Cannot find it seems like the code is ACT_DEL, but no documents associated  
  # National implementations Cannot find it
  # Decisions adopted by bodies created by international agreements: ACT_ADOPT_INTERNATION 
  # Treaties: TREATY
  # Convention: CONVENTION
  # other acts: ACT_OTHER

mpaCELEX<-freetext_eurlex("marine protected area*",act="DIR",lang="en",exactly=TRUE)
mpaCELEX

resource.types <- c("DIR","REG_DEL",
                    "DEC_ADOPT_INTERNATION","TREATY", 
                    "CONVENTION","ACT_OTHER")

#"ACT_LEGIS","REG",

mpaCELEX<-list()

for (i in 1:length(resource.types)) {  
  mpaCELEX[[i]]<- 
    freetext_eurlex("marine protected area*",
                    act=resource.types[[i]],
                    lang="en",
                    exactly=FALSE)

}


#lets give each list the name based on the designations: 
mpaCELEX.list <- structure(mpaCELEX, names=resource.types)



# eurlex download ---------------

test_query <- elx_make_query("any", 
                             include_eurovoc = TRUE, 
                             include_force = TRUE,
                             include_date = TRUE,) 

results <- 
  elx_run_query(query = test_query) %>% 
  rename(date = `callret-3`) #rename column to be more understandable

