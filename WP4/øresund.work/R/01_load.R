
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------

library("readr")
library("stringr")
library("dplyr")
library("rJava")
library("remotes")
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
library("tabulizer")


# Define functions --------------------------------------------------------


# Load data ---------------------------------------------------------------

setwd("C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/raw_data/DK_policy")

retsinformation.file.list <- list.files(pattern='*.csv')

#error was coming up...
# https://stackoverflow.com/questions/60906507/how-to-solve-error-error-in-ncharrownamesm-invalid-multibyte-string-ele
# https://readr.tidyverse.org/articles/locales.html
x <- "PopulærTitel" # --> one of the df column names
Encoding(x) #"latin1"
guess_encoding("C:/Users/aeljor/Desktop/mpa4sustainability/WP4/øresund.work/data/raw_data/DK_policy/fiskeri.csv")
#encoding   confidence
#<chr>           <dbl>
# ISO-8859-1       0.45
# ISO-8859-1 is another name for latin1

retsinformation.df <- read_delim(retsinformation.file.list, 
                                id = "search.term",
                                delim = ";",
                                locale = locale(encoding="ISO-8859-1"))


retsinformation.df <-
  retsinformation.df %>%
  mutate(search.term = str_extract_all(search.term,"\\w+\\."),
         search.term = str_replace_all(search.term,"[:punct:]+",""))
       


