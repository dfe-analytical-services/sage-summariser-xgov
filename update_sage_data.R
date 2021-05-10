# Script to execute daily to keep the internal 
# SQLite database up to date. Execution is managed
# by Github action and 

library(vctrs)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(tidytext)
library(tibble)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyWidgets)
library(shinyLP)
# library(DBI) # connecting to sql
# library(dbplyr) # connecting to sql
# library(config) # connecting to sql 
# library(RSQLite)# connecting to sql 
library(glue)
library(SnowballC)
library(lubridate)

# PDF reading 
library(pdftools)

# Web scraping
library(rvest)
library(httr)
library(polite)

# PDF summarising 
library(tidytext)
library(textrank)
library(textreuse)
library(lexRankr)

# Progress bar
library(progress)

# Load all functions:
source_out <- sapply(list.files('R/', full.names = TRUE), source)

# Run update --------------------------------------------------------------
update_sage_tables()




