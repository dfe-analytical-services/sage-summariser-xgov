# These functions help us scrape the SAGE info for both full scrapes, 
# and for updates. 
# 
# Check out the "R scripts/Updating sage tables.R" for the code of how to use the below 
# functions to run an update 

# READ ME --------------------------------------------------------------------
# SAGE papers are organised by Month web page > Meetings web page >
# actual publication web page. The latter contains the link to the pdf

# We use {polite} to handle the crawl delays

# The sections can be described as follows:
# == General functions ==
# These functions are helpful throughout the whole webscraping process 
# This is mostly due to the fact the web pages are structured in a very similiar 
# way at different levels
# 
# == Creating SAGE paper SQL table ==  
# The SAGE paper table is the main one, and it is one row per publication with 
# links to the landing page, and the pdf. 
# We create this first as everything else can be derived from it
# 
# == Generating tables from PDFs ===
# Here we define functions we need to generate the other tables from the PDF
# data (such as texts and words) 
# 
# == Updating SQL tables ==
# Now we combine it all together to create a function which can update the SQL 
# tables with the latest published data 



# General functions ----------------------------------------------------------------------
# This function creates the bow to the gov.uk website 
# which lets us do polite webscraping 
#' @importFrom polite bow, nod
bow_to_gov <- function(){
  bow(
    url = "https://www.gov.uk/",  # base URL
    user_agent = "SAGE summariser xgov",  # identify ourselves
    force = TRUE
  )
}

# The gov.uk pages are structured very similiarly, 
# so we use this function to reduce code duplication.
# given a path, it scrapes the main hyperlinks, their 
# publication date and their name.
get_link_data_from_path <- function(path, bow = bow_to_gov()){
  
  # Agree modification of session path with host
  session <- nod(
    bow = bow,
    path = path)
  
  # Scrape the page from the new path
  scraped_page <- scrape(session)
  
  # There are sections with headers and then links within it
  meeting_sections <- scraped_page %>% 
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "gem-c-document-list", " " ))]')
  
  # Get names 
  meeting_section_names <- scraped_page %>% 
    html_nodes(xpath = '//*[contains(@class, "group-title")]') %>% 
    html_text()
  
  # Meeting section contents 
  hyperlink_sections <- meeting_sections %>% 
    map(html_children)
  
  # Create a function to create links metadata table 
  link_data_from_node <- function(node){
    link <-  map_chr(node, ~(.x %>% html_children %>%  html_attr("href") %>% .[1] ))
    last_updated <- map_chr(node, ~(.x %>% html_children %>%  html_text %>% .[2] %>% str_extract("[0-9]* [A-Z][a-z]* 202[0-9]")))
    link_name <- map_chr(node, ~(.x %>% html_children %>%  html_text %>% .[1]))
    
    tibble(link_name = link_name,
           last_updated = last_updated, 
           link = link) %>%  
      mutate(last_updated = as_date(last_updated, format = "%d %B %Y"))
  }
  
  # Get links data from meeting sections 
  links_data <- hyperlink_sections %>%
    map(link_data_from_node)
  
  # Combine these two together and return
  tibble(link_section_name = meeting_section_names,
         links_data = links_data) %>% 
    unnest(links_data)
  
}

# This function extracts all the SAGE monthly meeting urls, 
# as well as the date they were last updated
# https://www.gov.uk/government/collections/scientific-evidence-supporting-the-government-response-to-coronavirus-covid-19 
get_sage_monthly_pages <- function(bow = bow_to_gov()){
  
  sage_path <- "government/collections/scientific-evidence-supporting-the-government-response-to-coronavirus-covid-19"
  
  sage_monthly_pages <- get_link_data_from_path(sage_path, bow) %>% 
    filter(stringr::str_detect(link, "sage-meetings")) %>% # Only want the sage meeting links
    rename(meeting_link = link) %>% 
    select(-link_section_name)
  
  return(sage_monthly_pages)
}  

# Creating the SAGE paper SQL table -----------------------------------------------------------------------
# This function gets all the publication urls from the monthly urls 
# You can use the min_date input to only scrape what has been updated since
# a certain date 
get_publication_urls <- function(bow = bow_to_gov(), min_date = "2020-01-01"){
  
  sage_monthly_pages <- get_sage_monthly_pages(bow = bow)
  
  # Filter so we only scrape what has been updated 
  sage_monthly_pages <- sage_monthly_pages %>% 
    filter(last_updated >= min_date) %>% 
    mutate(publication_urls = map(meeting_link, get_link_data_from_path, bow = bow)) 
  
  # Get the publication urls and filter on min date
  publication_urls <- sage_monthly_pages %>% 
    select(meeting_link, publication_urls) %>% 
    unnest(publication_urls) %>% 
    filter(last_updated >= min_date) %>% 
    rename(publication_name = link_name,
           publication_url = link,
           sage_meeting_name = link_section_name) %>% 
    mutate(
      # Get meeting number from name 
      sage_meeting_num = sage_meeting_name %>% 
        str_extract("Meeting [0-9]{1,3}"),
      # Get date from name 
      sage_meeting_date = sage_meeting_name %>% 
        str_remove(paste0(sage_meeting_num, ", ")) %>% 
        as_date(format = "%d %B %Y"),
      # Make meeting number numeric 
      sage_meeting_num = sage_meeting_num %>% 
        str_remove("Meeting ") %>% as.numeric()) 
  
}

# From the publication URLs, we must extract the pdf_urls and data 
get_pdf_url_from_publications_page <- function(publication_url, bow = bow_to_gov(), pb = NULL){
  
  # If we are using a progress bar, progress it
  if(!is.null(pb)) pb$tick()
  
  # Agree modification of session path with host
  session <- nod(
    bow = bow,
    path = publication_url)
  
  # Scrape the page from the new path
  scraped_page <- scrape(session)
  
  # Get PDF url 
  pdf_url <- scraped_page %>% 
    html_nodes(xpath = '//*[(@id = "documents")]//a') %>%  
    html_attr("href") %>% .[1]
  
  return(pdf_url)
  
}

# This function puts all the above ones together in order to create the [Shiny_pa].[dbo].[SAGE_papers]
# table 
create_sage_papers_table <- function(bow = bow_to_gov(), min_date = "2020-01-01"){
  
  # Get publication urls
  publication_urls <- get_publication_urls(bow, min_date)
  
  # Create progress bar
  pb <- progress_bar$new(total = nrow(publication_urls))
  
  # Scrape pdf urls
  pdf_urls <- publication_urls %>% 
    mutate(pdf_url = map_chr(publication_url, get_pdf_url_from_publications_page,  bow = bow, pb = pb))
  
  # Get into the right format for the table 
  sage_papers <- pdf_urls %>% 
    select(publication_link = publication_url,
           published_date = last_updated,
           pdf_name = publication_name,
           pdf_url,
           sage_meeting_num, # corresponding_sage_meeting 
           sage_meeting_date) %>% 
    mutate(publication_type = case_when(stringr::str_detect(pdf_url, "assets.publishing.service.gov.uk") ~ "gov_asset",
                                        TRUE ~ "external"))
  
  return(sage_papers)
}

# Generating tables from PDFs --------------------------------------------------------------
# Now we have the PDF uRLs, we want to get the following data from them:
# * The text 
# * The words (to make search easier)
# * The topics 
# * The summary 

# Some of these functions are already defined in "pdf mining functions.R"

# This function helps us clean text 
clean_text <- function(text){
  text %>% 
    stringr::str_squish() %>%
    str_replace_all("‘|’", replacement = "'") %>% 
    str_replace_all( "–", replacement = "-") %>% 
    str_remove_all("^● ") %>% 
    str_remove_all("^• ") %>% 
    str_replace_all("●", replacement = "-") %>% 
    str_replace_all("•", replacement = "-")
}

# The bow is just in case the pdf url passed is actually a link to anther webpage 
extracting_pdf_text <- function(pdf_url, bow = bow_to_gov(), pb = NULL){
  
  # If we are using a progress bar, progress it
  if(!is.null(pb)) pb$tick()
  
  # define empty return 
  empty_return <- tibble(pdf_url = NA_character_, .rows = 0)
  
  # If the URL is a PDF, we extract the text
  if(tolower(tools::file_ext(pdf_url)) == "pdf"){
    
    # Download the file. For some reason it seems to need this rather than reading directly from the URL
    suppressMessages({
      suppressWarnings({
        tf <- tempfile(fileext = ".pdf")
        GET(pdf_url, write_disk(tf))
      })
    })
    
    # Use the function we defined for uploaded docs
    # We have to use tryCatch as some links are broken
    # 
    # We also need to reduce the character count if it is over 8000 bytes
    # so it can be saved to SQL 
    pdf_text <- tryCatch({
      get_pdf_text(tf) %>% 
        mutate(text = clean_text(text), # clean characters 
               text = ifelse(nchar(text, type = "bytes") <= 8000, text, 
                             substring(text, 1, 6000))) # 6000 so we don't get an error with bytes 
      
    }, 
    error = function(e) empty_return
    )
    
    return(pdf_text)
    
  }
  
  # If the URL isn't a PDF, we see if it's another webpage
  # If it is, we extract the text from the description on the webpage
  # If it's not, we return an empty tibble 
  
  # This is true if there is no file extension, i.e. it is another webpage 
  # The final condition is to make sure it is a gov.uk webpage
  if(tolower(tools::file_ext(pdf_url)) == "" & str_sub(pdf_url, 1, 18) == "https://www.gov.uk"){
    
    # Try Catch in case there is an error
    pdf_text <- tryCatch({
      # Agree modification of session path with host
      session <- nod(
        bow = bow,
        path = pdf_url)
      
      # Scrape the page from the new path
      scraped_page <- scrape(session)
      
      # Get PDF url 
      webpage_text <-  scraped_page %>% 
        html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "gem-c-title__text--long", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "gem-c-lead-paragraph", " " ))]') %>%  
        html_text() %>%
        str_replace_all("\n", " ") %>% 
        str_replace_all("  *", " ") %>% 
        trimws() %>% 
        paste(collapse = ". ")
      
      pdf_text <- tibble(pdf_url = pdf_url, 
                         page = 1,
                         text = webpage_text)
      
      pdf_text
    }, 
    error = function(e) empty_return
    )
    
  } else pdf_text <- empty_return
  
  return(pdf_text)
  
}

# This function takes the text df as an input, and outputs the 5 sentence summary 
summarising_pdf_text <- function(pdf_text, nb_sentences_in_summary = 5, 
                                 max_nb_sentences_in_doc = 300, pb = NULL){
  
  # If we are using a progress bar, progress it
  if(!is.null(pb)) pb$tick()
  
  # If the pdf_text table is empty, we just return an empty table
  if(nrow(pdf_text) == 0) return(tibble(.rows = 0))
  
  # Read in the full text
  full_text <- pdf_text$text %>% 
    paste0(collapse = " ") %>% 
    enframe(name = NULL, value = "text")
  
  # Create the sentence and word data frames
  # needed to run the TextRank algorithm
  sentence_df <- full_text %>%
    unnest_tokens(sentence, text, token = "sentences") %>%
    mutate(sentence_id = row_number()) %>%
    select(sentence_id, sentence)
  
  word_df <- sentence_df %>%
    unnest_tokens(word, sentence) %>% 
    anti_join(stop_words, by = "word")
  
  
  # Run TextRank to rank representative sentences
  nb_sentences <- nrow(sentence_df)
  
  # If there are fewer sentences in the whole text
  # than we want in the whole summary, we keep everything
  if(nb_sentences <= nb_sentences_in_summary){
    article_summary <- sentence_df %>% 
      mutate(index = sentence_id, total_nb_sentences = nb_sentences) %>%
      select(index, summary_sentence = sentence, total_nb_sentences) %>%
      mutate(summary_sentence = stringr::str_squish(summary_sentence)) %>%
      mutate(summary_sentence = stringr::str_to_sentence(summary_sentence))
    message("Text is too short, skipping summarisation and saving whole text.")
  } else {
    # If there are more sentences than minimum for summarisation
    # but fewer than the maximum above which computation becomes
    # too time consuming. 
    if(nb_sentences <= max_nb_sentences_in_doc) {
      article_summary <- textrank_sentences(data = sentence_df,
                                            terminology = word_df)
      
      # Keep only the nb_sentences_in_summary most important sentences
      # Also do a bit of formatting
      article_summary <- article_summary %>%
        .[["sentences"]] %>%
        as_tibble() %>%
        arrange(desc(textrank)) %>%
        slice(c(1:nb_sentences_in_summary)) %>%
        arrange(textrank_id) %>%
        select(summary_sentence = sentence)
    } else {
      # If the document is longer than the max number of sentences
      # we are prepare to wait for the summarisation, we need to speed
      # up the computation. To do so we switch to lexRank.
      
      article_summary <- lexRankr::lexRank(sentence_df %>% select(sentence) %>% pull(sentence), 
                                           docId = rep(1, nrow(sentence_df)), n = nb_sentences_in_summary, continuous = TRUE,
                                           removeNum = FALSE, Verbose = TRUE) %>%
        arrange(as.numeric(gsub("_","",sentenceId))) %>%
        .[["sentence"]] %>% 
        as_tibble() %>% 
        select(summary_sentence = value)
      
      message("Text is too long. Switching to lexRank summarising")
    }
    
    article_summary <- article_summary %>%
      mutate(summary_sentence = stringr::str_squish(summary_sentence)) %>%
      mutate(summary_sentence = stringr::str_to_sentence(summary_sentence)) %>%
      mutate(index = row_number(), total_nb_sentences = nb_sentences) %>%
      select(index, summary_sentence, total_nb_sentences)
  }
  
  return(article_summary)
  
}

# Updating SQL tables ---------------------------------------------------------------------
# These functions use the above to update the SQL tables (either from scratch or on a day 
# by day basis)
# 
# The from scratch upload is simple. Just scrape everything and save 
# 
# The day by day process looks like the following:
# 1) Look at the stored RDS files for the latest data 
# 2) Scrape all data since the latest date (inclusive)
# 3) Remove anything that has already been saved 
# 4) Create sub tables  
# 5) Identify SAGE minutes documents to append it to the SAGE_meeting_minutes table
# 6) Update RDS files 

# This function looks to SQL to get the latest saved papers
get_latest_saved_papers <- function(con){
  con %>% 
    tbl("SAGE_papers") %>% 
    filter(published_date == max(published_date, na.rm = T)) %>% 
    collect()
}

# This function brings everything together to update the SQL datasets
# Currently rescrape_all won't work properely, as the SQL tables will still 
# just be appended so I have removed it as a paramter
update_sage_tables <- function(bow = bow_to_gov()){
  
  update_start_time <- Sys.time()
  
  rescrape_all <- FALSE
  
  all_tables <- readRDS("data/SAGE_tables.rds")
  
  topic_model <- readRDS("models/topic_lda_education.rds")
  
  # 1) If we are not rescraping everything, then we want to know what the latest data 
  # we scraped is 
  if(!rescrape_all){
    # Look for latest saved
    latest_saved <- all_tables[["SAGE_papers"]] %>% 
      filter(published_date == max(published_date, na.rm = T)) 
    min_date <- max(latest_saved$published_date)
  } else min_date <- "2020-01-01"
  
  # 2) Scrape all data since then (including that day)
  message("Scraping latest data...")
  new_data <- create_sage_papers_table(bow = bow, min_date = min_date)
  
  # 3) Remove any already saved 
  new_data <- new_data %>% 
    filter(!pdf_url %in% latest_saved$pdf_url)
  
  # additional filter here for the rare instances
  # where a link to a pdf doesn't contain the .pdf extension.
  # This can happen for external publications. At the moment
  # this generates an error on the x-gov version 
  # further down the code workflow because it returns an empty
  # tibble when extracting text. A better way of checking whether we truly have a pdf
  # is HEAD(url) %>% .$headers$`content-type` rather than checking extension
  # We don't get an error in the DfE version (because of newer package versions on x-gov)
  # but these papers don't get displayed still as they don't have summaries or topics
  new_data <- new_data %>% 
    filter(!((tools::file_ext(pdf_url)) == "" & str_sub(pdf_url, 1, 18) != "https://www.gov.uk"))
  
  # Here we check if there are any docs to add. 
  # If there aren't we skip all the rest of the steps
  if(nrow(new_data) != 0){
    
    # 4) Create sub tables 
    # Extract text 
    message("Exracting text from PDFs...")
    pb <- progress_bar$new(total = nrow(new_data))
    new_data <- new_data %>% 
      mutate(pdf_text = map(pdf_url, extracting_pdf_text, pb = pb))
    
    # Extract words from text 
    message("Exracting words from PDF text...")
    pb <- progress_bar$new(total = nrow(new_data))
    new_data <- new_data %>% 
      mutate(pdf_words = map(pdf_text, get_pdf_words, stop_words = stop_words, pb = pb))
    
    # Extract topics from text  
    message("Exracting topics from PDF text...") 
    pb <- progress_bar$new(total = nrow(new_data))
    new_data <- new_data %>% 
      mutate(pdf_topics = map(pdf_text, get_pdf_topic, topic_lda = topic_model, pb = pb))
    
    # Extract summaries from text 
    message("Summarising PDF text...") 
    pb <- progress_bar$new(total = nrow(new_data) + 1) # The +1 is because the summarising can take a while so we want the PB to remain for the final summarising
    new_data <- new_data %>% 
      mutate(pdf_summaries = map(pdf_text, summarising_pdf_text, pb = pb))
    
    # Create the tables ready to append 
    SAGE_papers <- new_data %>% 
      select(publication_link:publication_type) %>% 
      mutate(publication_link = paste0("https://www.gov.uk", publication_link),
             pdf_name = clean_text(pdf_name)) 
    
    SAGE_papers_text <- new_data %>% 
      select(pdf_url, pdf_text) %>% 
      mutate(pdf_text = map(pdf_text, ~.x %>% select(-pdf_url))) %>%  # Removing the temporary url
      unnest(pdf_text)
    
    SAGE_papers_words <-  new_data %>% 
      select(pdf_url, pdf_words) %>% 
      unnest(pdf_words) %>% 
      group_by(pdf_url, word) %>% 
      count() %>% 
      ungroup()
    
    SAGE_papers_topics <- new_data %>% 
      select(pdf_url, pdf_topics) %>% 
      unnest(pdf_topics) %>% 
      select(-document) %>% 
      rename(document = pdf_url)
    
    SAGE_papers_summaries <- new_data %>% 
      select(pdf_url, pdf_summaries) %>% 
      unnest(pdf_summaries) %>%  
      select(index, summary_sentence, pdf_url, total_nb_sentences) %>% 
      mutate(summary_sentence = clean_text(summary_sentence))
    
    # 5) Identify SAGE minutes
    SAGE_meeting_minutes <- SAGE_papers %>% 
      filter(str_detect(pdf_name, "SAGE [0-9]{1,3} minutes")) %>% 
      select(sage_meeting_num, 
             corresponding_sage_minutes_landing_page = publication_link)
    
    # 6) Update RDS data  
    all_tables[["SAGE_papers"]] <- bind_rows(all_tables[["SAGE_papers"]], SAGE_papers)
    all_tables[["SAGE_papers_text"]] <- bind_rows(all_tables[["SAGE_papers_text"]], SAGE_papers_text)
    all_tables[["SAGE_papers_words"]] <- bind_rows(all_tables[["SAGE_papers_words"]], SAGE_papers_words)
    all_tables[["SAGE_papers_topics"]] <- bind_rows(all_tables[["SAGE_papers_topics"]], SAGE_papers_topics)
    all_tables[["SAGE_meeting_minutes"]] <- bind_rows(all_tables[["SAGE_meeting_minutes"]], SAGE_meeting_minutes)
    all_tables[["SAGE_papers_summaries"]] <- bind_rows(all_tables[["SAGE_papers_summaries"]], SAGE_papers_summaries)
  }
  
  # 7) Update meta table 
  update_end_time <- Sys.time()
  total_papers = all_tables[["SAGE_papers"]] %>%
    summarise(n = n()) %>% pull(n)
  SAGE_update_meta <- tibble(update_date = Sys.Date(),
                             update_start_time = update_start_time,
                             update_end_time = update_end_time,
                             update_length = as.numeric(update_end_time - update_start_time),
                             update_user = Sys.info()[['user']],
                             num_new_papers = nrow(new_data),
                             num_total_papers = total_papers)
  
  all_tables[["SAGE_update_meta"]] <- bind_rows(all_tables[["SAGE_update_meta"]], SAGE_update_meta)
  saveRDS(all_tables, "data/SAGE_tables.rds")
}
