#############################################################################
#                   Process  Diaraio da Republica                           #
#############################################################################
# 
# O Identificador Europeu da LegislaÃ§Ã£o (ELI) Ã© um sistema de disponibilizaÃ§Ã£o 
# de legislaÃ§Ã£o em linha num formato normalizado, tendo em vista facilitar a 
# sua consulta, intercÃ¢mbio e reutilizaÃ§Ã£o alÃ©m-fronteiras. Trata-se de uma 
# iniciativa conjunta dos Estados-Membros da UE 
# format of eli to get the url of the diario da republica
# https://eur-lex.europa.eu/eli-register/portugal.html?locale=pt
# https://data.dre.pt/eli/diario/{sÃ©rie}/{nÃºmero no ano}/{ano}/{suplemento}/{lÃ­ngua}/{formato do ficheiro}

################################################################################
# libraries


# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library('RSelenium') 
library("rvest")
library('readr') 
library('stringr') 
library('tibble') 
library("netstat")
library("dplyr")
library("stringr")
library("tidyr")
library("binman")
library("wdman")
library("purrr")
library("openxlsx")

################################################################################

# Set wd ---------------------------------------------------------------

# dir_data <- "E:/MBM/github/Madeira/data/raw_data/diario_da_republica/"
# dir_resul <- "E:/MBM/github/Madeira/data/data/diario_da_republica/"

dir_data <- "E:/MBM/github/Madeira/data/raw_data/diario_da_republica/"
dir_resul <- "E:/MBM/github/Madeira/data/diario_da_republica/"
#make R Selenium to download the chromedriver available
RSelenium::rsDriver(browser = "chrome",
                    chromever = "latest_compatible")


binman::list_versions("chromedriver")

#load data
setwd(dir_data)
keyword <- "turismo"
name_Rfile <- paste0('result_', keyword, "_diario_da_republica_Madeira.RData")
load(name_Rfile)

#set as data frame
all_legal_text_results <- as.data.frame(all_legal_text)
all_legal_text_results$year <- str_sub(all_legal_text_results$date, 1, 4)
#get the links
law_tretas_web <- all_legal_text_results$link_tretas

#set and empty list
ind.text<-1

#load the webpage
rSel <- rsDriver(port=4454L)
remDr <- rSel$client


#loop to obtain link to Diario da Republica and links to other portuguese laws

  for (i in 1:length(law_tretas_web)){
  # require(RSelenium)

  
  enlace<-law_tretas_web[i]
  # enlace <- "https://dre.tretas.org/dre/2387135/decreto-34134-de-24-de-novembro"
  remDr$navigate(enlace)
  
  # rSel$server$stop()
  # Now we get the page source and use rvest to parse it
  page_source <- remDr$getPageSource()[[1]]
  html <- read_html(page_source)
  
  
  #get DR direct link
  link.DR <-
    html%>%
    html_nodes(css = "h1+ ul > li > a:nth-child(1)") %>%
    html_attr("href")
  
  if(rlang::is_empty(link.DR)){
    link.DR=NA
  }
    
  all_legal_text_results$link_DR[i] <- link.DR
  
  #get full text
  full.text <-
    html%>%
    html_nodes(css = ".result_notes:nth-child(8)") %>%
    html_text(trim = TRUE)
  
  if(rlang::is_empty(full.text)){
    full.text=NA
  }
    
  all_legal_text_results$full.text[i] <- full.text
  
  
  #get annex direct link
  link.annex <-
    html%>%
    html_nodes(css = "#content > div.content-text > div:nth-child(8) > ul > li > a") %>%
    html_attr("href")
   if(rlang::is_empty(link.annex)){
      link.annex=NA
   }else{
   link.annex <- paste0("https://dre.tretas.org", link.annex)    
    }
  all_legal_text_results$link.annex[i] <- link.annex
 
   #get requirements for eli of diario da republica
  ind.text<-
    html%>%
    html_nodes(css = "h1+ ul > li:nth-child(2)") %>%
    html_text(trim = TRUE)
  
  if(length(ind.text)<1){
    all_legal_text_results$title_DR [i] <- NA
  }else{
    all_legal_text_results$title_DR [i] <- substring(ind.text, 
                                                   regexpr(":", ind.text) + 2)
  }
  
 
  
  # get links to others from the tretas webpage
  links.others <-
  html%>%
  html_nodes(css = ".result_notes:nth-child(13) .result_date+ a")%>%
  html_attr("href")

  if(length(links.others)>0){
    links.others <- paste0("https://dre.tretas.org/", links.others)
    links.others <- list(links.others)
  }
  if(length(links.others)==0){
    links.others <- NA
  }

  all_legal_text_results$links.others[i] <- links.others
  
    # get also the name of the laws
  links.others.title <-
      html%>%
      html_nodes(css = ".result_notes:nth-child(13) .result_date+ a")%>%
      html_text2()
  
  if(length(links.others.title)>0){
  links.others.title <- list(links.others.title)
  }
  
  if(length(links.others.title)==0){
    links.others.title <- NA
  }
  
  all_legal_text_results$links.others.title[i] <- links.others.title
  
  links.thisone <-
      html%>%
      html_nodes(css = ".result_date+ a")%>%
      html_attr("href")
  
  if(length(links.thisone)>0){
  links.thisone <- paste0("https://dre.tretas.org/", links.thisone)
  links.thisone <- list(links.thisone)
  }
  
  
  if(length(links.thisone)==0){
    links.thisone <- NA
  }
  
  all_legal_text_results$links.thisone[i] <- links.thisone
  
  # get also the name of the laws
  links.thisone.title <-
      html%>%
      html_nodes(css = ".result_notes:nth-child(15) .result_date+ a")%>%
      html_text2()
  
  if(length(links.thisone.title)>0){
  links.thisone.title <- list(links.thisone.title)
  }
  
  if(length(links.thisone.title)==0){
    links.thisone.title <- NA
  }
  
  all_legal_text_results$links.thisone.title[i] <- links.thisone.title
  
  # get the details for the ELI link
    
  name_law <- all_legal_text_results$title_DR [i]
  if(is.na(name_law=="")){
    diario_name <- "d"
  }else{
    diario_name <- stringr::str_extract(name_law, "^.{19}")
  }
  
  if(is.na(name_law) | name_law==""){
    link_ELI <- NA
  }else{
    
 
  if(diario_name == toupper(diario_name)){
    serie <- substring(name_law, regexpr("-", name_law) + 2,
                       regexpr(", Nº",name_law) -1)
    # DIARIO DA REPUBLICA - 1.Âª SERIE A, NÂº [21], de 21.01.1991, PÃ¡g. 434
      serie.num <- substring(serie, regexpr(".", serie) - 1,
                             regexpr(".", serie))
      serie.char <- substring(serie, regexpr("IE", serie) + 3)
  
      if(serie.char==""){
        serie.all <- serie.num
      } else {
        serie.all <- paste0 (serie.num, serie.char)
      } # else
    num.ano <- substring(name_law, regexpr("\\[", name_law) +1,
                         regexpr("\\]", name_law) - 1)
  if(num.ano==""){
    num.ano <- substring(name_law, regexpr("Nº", name_law) + 3,
                         regexpr("-S", name_law) - 1)
  }
  if(num.ano==""){
    num.ano <- substring(name_law, regexpr("Nº", name_law) + 3,
                         regexpr(", de", name_law) - 1)
  }
    sup <- substring(name_law, regexpr(",", name_law) + 2,
                         regexpr("Suplemento", name_law) - 3)
  }else{

      serie <- substring(name_law, regexpr("Série", name_law),
                           regexpr("de", name_law) - 2)
        # Diário da República nº70/1992, Série I-B de 1992-03-24
      
      if(grepl("-", serie, fixed = TRUE)==TRUE){
        serie.num <- substring(serie, regexpr("Série", serie) + 6, 
                             regexpr("-", serie) - 1)
      }else{
        serie.num <- substring(serie, regexpr("Série", serie) + 6)
      } # else
      
      #transform romans to numbers
      if(serie.num=="I"){
          serie.num <- 1
        }
        if(serie.num=="II"){
          serie.num <- 2
        }
        if(serie.num=="III"){
          serie.num <- 3
        }
        if(grepl("-", serie, fixed = TRUE)==TRUE){
        serie.char <- substring(serie, regexpr("-", serie) + 1)
        serie.char <- tolower(serie.char)
        serie.all <- paste0 (serie.num,serie.char)
        } else {
          serie.all <- serie.num
        }

        num.ano <- substring(name_law, regexpr("º", name_law) + 2,
                             regexpr("/", name_law) - 1)
        sup <- substring(name_law, regexpr("º", name_law) + 1,
                         regexpr("-Supl", name_law))
     } # else
  
  if(sup==""){
    sup<-0
    } # sup=="
  
      
      
  ling <- "pt"
  form <- "html"
      if(name_law==""){
        link.ELI <- NA
      }else{
        link.ELI <- paste0("https://data.dre.pt/eli/diario/", serie.all, "/", num.ano, "/",
                    all_legal_text_results$year[i],  "/", sup, "/",ling,
                    "/", form)
      }
  
  }# else
  
  all_legal_text_results$link_ELI[i] <- link.ELI

      

    
    #
} # end of law_tretas_web


head(all_legal_text_results)
view(all_legal_text_results)

# setwd(dir_resul)
# name_Rfile <- paste0('result_postprocess_', keyword, "_diario_da_republica_Madeira.RData")
# save(all_legal_text_results, file=name_Rfile)



### Loop to get the names of the European laws that they are realted to
#get the links of DR into a string
law_dre_web <- all_legal_text_results$link_DR
law_dre_web_alt <- all_legal_text_results$link_ELI


#load the webpage
rSel <- rsDriver(port=4456L)
remDr <- rSel$client


#loop to obtain links to other EU laws within DR
for (i in 1:length(law_dre_web)){
  # require(RSelenium)
  
  
  enlace<-law_dre_web[i]
  if(is.na(enlace)){
    enlace<-law_dre_web_alt[i]
  }
  # enlace <- "https://diariodarepublica.pt/dr/legislacao-consolidada/decreto-lei/2007-34563475"
  remDr$navigate(enlace)
  
  # rSel$server$stop()
  # Now we get the page source and use rvest to parse it
  Sys.sleep(3)
  page_source <- remDr$getPageSource()[[1]]
  html <- read_html(page_source)
  
  
  EU.link <-
    html%>%
    html_nodes(css="#b4-DireitoUniaoEuropeia a")%>%
    html_attr("href")
  
  if(length(EU.link)==0){
    EU.laws <- NA
  }else{
    full_EU.link <- paste0("https://diariodarepublica.pt", EU.link)
    url <- full_EU.link
  remDr$navigate(url)
  
  Sys.sleep(3)
  page_source <- remDr$getPageSource()[[1]]
  html <- read_html(page_source)
  
  EU.laws <-
    html%>%
    html_nodes(css="#b8-Table div")%>%
    html_text()%>%
    unique()
  
  EU.laws <- list(EU.laws)
  
  if(length(EU.laws)==0){
    EU.laws <- NA
  }
  }
  
  all_legal_text_results$EU.laws[i] <- EU.laws
  
} 


setwd(dir_resul)
name_Rfile <- paste0('result_postprocess_', keyword, "_diario_da_republica_Madeira.RData")
save(all_legal_text_results, file=name_Rfile)
