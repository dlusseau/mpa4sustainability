
# Clear work space ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------


# Define functions --------------------------------------------------------

# No defined function for this script

# Load data ---------------------------------------------------------------

# Our document-data key
document.key.df <- read.csv(file = "WP4/Policy_Interactions/data/01_SPARQL.key.df.csv")

# EU mpa designation term associated text data: 
MPA.DESG.text <- read.csv(file = "WP4/Policy_Interactions/data/01_mpaterms.text.data.csv")

# "marine protected" associated text data:
mar.protected.text <- read.csv(file = "WP4/Policy_Interactions/data/01_CELEXmpa.text.data.csv")
