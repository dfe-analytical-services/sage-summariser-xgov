# These functions help us get data from the PDFs
# They are used both when PDFs get uploaded, and during the update process
# (ALthough the update process now happens from a separate repo)

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

get_pdf_published_date <- function(pdf_url){
  
  pdf_url %>% 
    pdftools::pdf_info() %>% 
    .$modified %>% 
    lubridate::as_date()
  
}
  
get_pdf_text <- function(pdf_url){
  pdf_content <- pdf_url %>%
    pdf_text() %>% 
    enframe(name = "page") %>%
    rename(text = value) %>% 
    mutate(pdf_url = pdf_url)
  
  # For now, just save all text
  # We keep the pages to make it easier to save to SQL (as there is an 8000 character limit)
  pdf_content %>% 
    mutate(text = text %>% 
             str_replace_all("\r\n", " ") %>% 
             str_replace_all("  *", " ") %>% 
             clean_text()) %>% 
    select(pdf_url, everything())
  
}

get_pdf_words <- function(pdf_text, stop_words, pb = NULL){
  
  # If we are using a progress bar, progress it
  if(!is.null(pb)) pb$tick()
  
  pdf_text %>%
    unnest_tokens(word, text, token = "words") %>% 
    filter(!word %in% stop_words) %>% 
    mutate(word = tm::stemDocument(word)) %>% 
    select(word)
}


get_pdf_topic <- function(pdf_text, topic_lda, pb = NULL){
  
  # If we are using a progress bar, progress it
  if(!is.null(pb)) pb$tick()
  
  pdf_text %>% 
    group_by(pdf_url) %>% 
    summarise(text = paste(text, collapse = " ")) %>% 
    predict_topic_scores(topic_lda = topic_lda)
}


# This function takes the text df as an input, and outputs the 5 sentence summary 
get_pdf_summary <- function(pdf_text, nb_sentences_in_summary = 5, 
                                 max_nb_sentences_in_doc = 300, pb = NULL){
  
  # If we are using a progress bar, progress it
  if(!is.null(pb)) pb$tick()
  
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

