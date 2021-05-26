# Load libraries:
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

# accessibility
# library(shinya11y)

# Deployment
library(rsconnect)

# Load all functions:
source_out <- sapply(list.files('R/', full.names = TRUE), source)

#--- Connect to SQL: ----
# Load all tables from RDS file
all_tables <- readRDS("data/SAGE_tables.rds")
 
#---- Token parameters ----

# Topic labels
topic_labels <- all_tables[["SAGE_papers_topic_labels"]] %>% 
  arrange(label) 

# List of meetings
meeting_list <- all_tables[["SAGE_papers"]] %>%
  distinct(sage_meeting_num) %>%
  filter(!is.na(sage_meeting_num)) %>%
  arrange(desc(sage_meeting_num)) %>%
  mutate(sage_meeting_num = paste0("SAGE ", sage_meeting_num)) %>% 
  pull(sage_meeting_num)

#---- Initial values ----
# We bind to this as PDFs get uploaded
# I don't know if I need to specify all of the column names in advance 
uploaded_pdf_data <- tibble(.rows = 0)
