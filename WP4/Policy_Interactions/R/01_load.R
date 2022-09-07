
# Clear work space ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------

library("readr")

# Load data ---------------------------------------------------------------

# In eur-lex search ---------------

# query term: marine protected area* (no parenthesis!!)

# Resource Types downloaded from the search term:
  # Directives: done
  # Legislative acts: done - sent to email w/in next couple days...
  # Regulation: done - sent to email w/in next couple days...
  # Delegated regulation (del.reg): done 
  # Delegated act: --> not found in the search
  # National implementations --> not found in the search
  # Decisions adopted by bodies created by international agreements: done 
  # Treaties: done
  # Convention: done
  # other acts: done

setwd("/Users/annajorgensen/Desktop/Anna's R code/mpa4sustainability/WP4/Policy_Interactions/data/raw_data/EUR-Lex_Search")

EURLEX.file.list <- list.files(pattern='*.csv' )

EURLEX.df <- read_csv(EURLEX.file.list, id = "resource.type")


