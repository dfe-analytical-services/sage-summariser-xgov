# Function to predict topic of a newly uploaded document
library(topicmodels)
library(tidytext)


# Requires the papers_to_classify object to have a pdf_url id column, and a column called "text" containing all of
# the text, concatenated
# You also need to provide a pre-trained LDA model object, which can be loaded from the models/ sub-folder
predict_topic_scores <- function(papers_to_classify, topic_lda) {
  
  # Cleaning: remove numbers, stopwords, file extensions
  # Tokenize - words
  tokens_word <- papers_to_classify %>%
    select(pdf_url, text) %>%
    mutate(text = tolower(text),
           text = str_replace(text, "covid-19", "covid19"),
           text = str_replace(text, "pre-", "pre"),
           text = str_replace(text, "sars-cov-2", "sarscov2"),
           text = str_replace(text, "spi-m-o", "spimo"),
           text = str_replace(text, "spi-m", "spim"),
           text = str_replace(text, "covid-o", "covido")) %>%
    unnest_tokens(output = "phrase", 
                  input = text, 
                  to_lower = TRUE,
                  token = "words") %>%
    anti_join(stop_words, by = c("phrase" = "word")) %>%
    mutate(phrase = gsub('[[:punct:] ]+', '', phrase),
           phrase = trimws(phrase)) %>%
    filter(is.na(as.numeric(phrase)),
           phrase != "na",
           phrase != "",
           phrase != " ",
           phrase != "NA")
  
  # Tokenize - bigrams
  tokens_bigram <- tokens_word %>%
    group_by(pdf_url) %>%
    mutate(phrase = paste(lag(phrase), phrase)) %>%
    ungroup() %>%
    filter(!str_detect(phrase, "NA "))
  
  # Tokenize - trigrams
  tokens_trigram <- tokens_word %>%
    group_by(pdf_url) %>%
    mutate(phrase = paste(lag(phrase, 2), lag(phrase), phrase)) %>%
    ungroup() %>%
    filter(!str_detect(phrase, "NA "))
  
  
  # Create dtm
  topic_dtm <- tokens_word %>%
    bind_rows(tokens_bigram) %>%
    bind_rows(tokens_trigram) %>%
    count(pdf_url, phrase, sort = TRUE) %>%
    ungroup() %>%
    cast_dtm(pdf_url, phrase, n)
  
  # Create topic scores from model
  new_topics <- topicmodels::posterior(topic_lda, newdata = topic_dtm)
  
  # Add to data
  new_docs <- as_tibble(rownames = "document", x = new_topics$topics)
  
  # Convert to long and return
  new_docs %>%
    gather(topic_number, value, -document) %>%
    arrange(document, as.numeric(topic_number)) %>%
    group_by(document) %>%
    mutate(main_topic = which.max(value)) %>%
    ungroup %>%
    arrange(document, desc(value)) %>%
    select(document, main_topic, topic_number, value)
  
}
