
server <- function(input, output, session){
  
  # Set reactive values
  v <- reactiveValues(uploaded_pdf_data = uploaded_pdf_data)
  
  # Get data ----
  get_publications_data_rds <- reactive({

    # Get meeting paper was discussed at
    doc_meetings <- all_tables[["SAGE_meeting_minutes"]] %>%
      distinct() # just in case duplicate rows have creeped in

    # Get document topics and labels
    doc_topics <- all_tables[["SAGE_papers_topics"]] %>%
      filter(value > 0.1) %>%
      inner_join(all_tables[["SAGE_papers_topic_labels"]])

    # Get document summaries
    doc_summaries <- all_tables[["SAGE_papers_summaries"]] %>%
      arrange(index) %>%
      filter(index <= 5) %>%
      mutate(summary_sentence = clean_text(summary_sentence)) %>%
      distinct() %>% 
      mutate(summary_sentence_short = stringr::str_trunc(summary_sentence, 500)) %>% 
      mutate(summary_sentence = if_else(nchar(summary_sentence) > 500,
             summary_sentence_short, summary_sentence)) %>% 
      group_by(pdf_url) %>%
      summarise(summary = str_flatten(summary_sentence, collapse = "</li><li>")) %>%
      ungroup() %>%
      mutate(summary = str_remove(summary, "• "),
             summary = str_remove(summary, "• "),
             summary = str_replace_all(summary, "�", "'"),
             summary = paste0("<ul><li>", summary, "</li></ul>"))

    # Join to publications
    publications_tbl <- all_tables[["SAGE_papers"]] %>%
      inner_join(doc_topics, by = c("pdf_url"="document")) %>%
      left_join(doc_meetings, by = "sage_meeting_num") %>%
      left_join(doc_summaries, by = "pdf_url")

  })
  
  # We combine the SQL data with the uploaded data here 
  get_publications_data <- reactive({
    
    publications_tbl <- get_publications_data_rds() %>%
      mutate(source = "Published")
    
    if(nrow(v$uploaded_pdf_data) > 0){
      
      # Topics 
      labels <- all_tables[["SAGE_papers_topic_labels"]]
    
      new_topics <- map_dfr(v$uploaded_pdf_data$pdf_topics, function(x){
        x %>%
          filter(value > 0.1) %>%
          inner_join(labels)
      })

      # Join on new topics 
      # Then bind on RDS data
      publications_tbl_new <- v$uploaded_pdf_data %>% 
        inner_join(new_topics, by = c("pdf_url"="document")) 
      
      publications_tbl <- publications_tbl_new %>% 
        mutate(source = "Uploaded by you") %>%
        select(any_of(names(publications_tbl))) %>%
        bind_rows(publications_tbl)
    }
    
    publications_tbl
    
  })
  
  get_words_data_rds <- reactive({
    
    # Get search words
    search_words <- get_search_words()
    
    # Find the docs
    all_tables[["SAGE_papers_words"]] %>%
      mutate(word = iconv(word, 'UTF-8', 'ASCII')) %>% 
      filter(tolower(word) %in% local(search_words$clean))
    
  })
  
  get_words_data <- reactive({
    
    # Get search words
    search_words <- get_search_words()
    
    # Get table loaded earlier from RDS file
    word_tbl <- get_words_data_rds()
    
    # Bind on data from uploaded PDFs
    if(nrow(v$uploaded_pdf_data) > 0){
      word_tbl <- v$uploaded_pdf_data %>%
        select(pdf_url, pdf_words) %>%
        unnest(pdf_words) %>%
        filter(tolower(word) %in% local(search_words$clean)) %>%
        bind_rows(word_tbl)
      
    }
    
    word_tbl
    
    
  })
  
  # Get search words ----
  get_search_words <- reactive({
    
    if(nchar(input$search_words) == 0) return(tibble(.rows = 0))
    
    input$search_words %>% 
      str_split(" ") %>% 
      .[[1]] %>% 
      .[!. %in% stop_words] %>% 
      tibble(input = .) %>% 
      mutate(clean = input %>% 
               tolower() %>% 
               tm::stemDocument()  
               )
      
  })
  
  # ---- Home page ----
  
  observeEvent(input$getStarted, {
    updateTabItems(session, "tabs",
                   selected = "search")
  })
  
  observeEvent(input$tabs, { 
    if (input$tabs == "accessibility") {
      updateTabItems(session, "tabs", "accessibility")
    }
  })
  
  # ---- Search page ----
  
  # ** Render search results ----
  output$search_outputs <- DT::renderDataTable({
    
    # Get all data, filter by date
    publication_data <- get_publications_data()
    #browser()
    
    # Filter by topic if necessary
    if (length(input$topic_filter) > 0) {
      publication_data <- publication_data %>%
        group_by(pdf_url) %>%
        filter(any(label %in% input$topic_filter)) %>%
        ungroup
    }
    
    # Filter by meeting if necessary
    if (length(input$meeting_filter) > 0) {
      publication_data <- publication_data %>%
        mutate(sage_meeting_num_char = paste0("SAGE ", sage_meeting_num)) %>% 
        filter(sage_meeting_num_char %in% input$meeting_filter)
    }
    
    # Filter by source if necessary
    if (length(input$source_filter) > 0) {
      publication_data <- publication_data %>%
        filter(source %in% input$source_filter)
    }
    
    # Convert labels to html for output
    # Filter by date
    publication_data <- publication_data %>%
      distinct() %>% 
      mutate(topic_class = ifelse(label %in% input$topic_filter,
                                  "bg-topic-highlight",
                                  "bg-topic"),
             label = paste0("<p class='badge ", topic_class ,"'>", label)) %>%
      group_by(source, publication_link, published_date, pdf_name, pdf_url, publication_type, summary,
               corresponding_sage_minutes_landing_page, sage_meeting_num) %>%
      summarise(main_topic = label[which.max(value)],
                labels_html = paste(unique(label), collapse = "</p>")) %>%
      mutate(labels_html = paste0("<div style = 'line-height: 200%;'>", labels_html, "</p></div>")) %>%
      ungroup %>%
      distinct() %>% 
      filter(published_date >= input$date_filter[1],
             published_date <= input$date_filter[2])
    # at this point the pdf might not be shown because the 
    
    # Get search words 
    search_words <- get_search_words()
    
    # If search words is empty, return all data 
    if(nrow(search_words)==0){
       
      # Display it
      filtered_data <- publication_data %>% 
        mutate(words_included = "") %>% 
        arrange(desc(published_date))
      
    } else{
 
      # Get documents with matching words
      words_data <- get_words_data()
      
      shiny::validate(
        need(nrow(words_data) > 0, message = "No results - try changing the filters or search term")
      )
      
      # See how many words are included
      word_occurences <- words_data %>%
        distinct() %>% 
        mutate(searched_word = map_chr(word, ~search_words$input[search_words$clean ==tolower(.x)])) %>% 
        group_by(pdf_url) %>% 
        summarise(words_included = searched_word %>% paste(collapse = ", "),
                  number_search_words_appearing = n()) 
      
      # Match on the number of word occurrences
      filtered_data <- publication_data %>% 
        inner_join(word_occurences) %>% 
        arrange(desc(number_search_words_appearing), desc(published_date)) 
      
      
    }
    
    # Check any rows in filtered data
    shiny::validate(
      need(nrow(filtered_data) > 0, message = "No results - try changing the filters or search term")
    )
    
    # Convert title into html so it can be a hyperlink
    #browser()
    filtered_data %>% 
      mutate(Title = ifelse(is.na(pdf_url), 
                            glue("<h3 style = 'margin-bottom: 0;'>{pdf_name}</h3>"),
                            glue("<h3 style = 'margin-bottom: 0; font-size:34px; color: #000000;'><a href='{pdf_url}' target='_blank'>{pdf_name}</a></h3>")),
             Content = paste0(Title,
                              ifelse(is.na(publication_link), 
                                     "<br>",
                                     glue("<div style = 'margin-bottom: 10px; font-size:18px;'><a href='{publication_link}' target='_blank'>{publication_link}</a></div>")),
                              labels_html,
                              "<br>",
                              "<b>Source</b><br>", source,
                              "<br><br>",
                              ifelse(is.na(sage_meeting_num),
                                     "",
                                     paste0("<b>Discussed at</b><br>",
                                            "<p>SAGE ", sage_meeting_num, "</p>",
                                            "<br><br>")),
                              ifelse(!is.na(summary),
                                     paste0("<b>Example sentences</b><br>", summary),
                                     "<br><br><br>"),
                              "<br>",
                              "<br>"
                              )) %>% 
      select(`Published/modified Date` = published_date,
             Content,
             `Words found` = words_included) %>% 
      DT::datatable(escape = FALSE, 
                    selection = "none",
                    style = "bootstrap",
                    rownames = FALSE,
                    class = "row-border",
                    options = list(searching = FALSE,
                                   pageLength = 50, 
                                   autoWidth = TRUE,
                                   lengthChange = TRUE
                                   )
      )
    # filtered_data %>% filter(stringr::str_detect(pdf_name, "PHE: Serological surveillance"), sage_meeting_num == 36) %>%
    # select(labels_html) %>% slice(1) %>% pull()
    
  })
  
  # Upload PDFs ----
  
  # * Get topic model ----
  get_topic_model <- reactive({
    readRDS("models/topic_lda_education.rds")
  })
  
  # * Upload modal ----
  # Clicking the button triggers the modal dialogue 
  observeEvent(input$upload, {
    showModal(
      modalDialog(
        title = "Upload your own PDFs",
        
        fluidPage(
          align = "center",
          fileInput("input_file", "Choose PDFs to upload",
                    multiple = TRUE,
                    accept = c(".pdf", ".PDF")),
          "You can upload multiple PDFs at once.",
          br(),
          "These PDFs will only be visible to you.",
          br(),
          strong("They will disappear when you close this page, and will not be saved in the database")
        ),
        
        footer = NULL,
        easyClose = TRUE
      )
    )
  })
    
    observeEvent(input$input_file, {
      # This require stops this event triggering when the previous modal closes
      req(nrow(input$input_file) > 0)
      showModal(
        modalDialog(
          title = "Upload your PDFs",
          
          fluidPage(
            align = "center",
            h1("Documents uploading...")
          ),
          
          footer = NULL,
          easyClose = FALSE
        )
      )
      
      withProgress(min = 0, max = 4, value = 1, message = 'Extracting content from PDFs...', expr = {
        
        new_files <- input$input_file %>%
          mutate(pdf_url = map_chr(datapath, function(x){
            # create temp file
            temp_file <- tempfile(fileext = ".pdf")
            # Copy file to new temp
            file.copy(x, temp_file, overwrite = T) # Copies dowloaded pdf to temp file.
            # Outputs temp file
            temp_file
          })) %>%
          rename(pdf_name = name) %>% 


          mutate(
            # Extract date
            published_date = as_date(map_dbl(pdf_url, get_pdf_published_date)),
            # Extract text
            pdf_text = map(pdf_url, get_pdf_text),
            # Extract words
            pdf_words = map(pdf_text, get_pdf_words, stop_words = stop_words),
            # Set some variables 
            publication_type = "uploaded"
          )

        setProgress(value = 2, message = "Analysing PDF topics...")
        new_files <- new_files %>% 
          mutate(pdf_topics = map(pdf_text, get_pdf_topic, topic_lda = get_topic_model()))

        setProgress(value = 3, message = "Summarising PDF content...")
        new_files <- new_files %>% 
          mutate(pdf_summary = map(pdf_text, get_pdf_summary, nb_sentences_in_summary = 5, max_nb_sentences_in_doc = 300),
                 # Now we apply the same code we apply when loading in the summary table 
                 pdf_summary = map(pdf_summary, function(x){
                   x %>% 
                     mutate(summary_sentence = clean_text(summary_sentence)) %>% 
                     summarise(summary = str_flatten(summary_sentence, collapse = "</li><li>")) %>%
                     ungroup() %>%
                     mutate(summary = str_remove(summary, "• "),
                            summary = str_remove(summary, "• "),
                            summary = paste0("<ul><li>", summary, "</li></ul>"))
                 })) %>% 
          # Finnally we unnest
          unnest(pdf_summary)
        
        # Update uploaded_pdf_data
        v$uploaded_pdf_data <- v$uploaded_pdf_data %>%
          bind_rows(new_files)
        
        # Update the source filter
        updateSelectizeInput(
          session,
          inputId = "source_filter",
          selected = "Uploaded by you"
        )
        
        # Go to the results tab
        updateTabItems(session, "tabs",
                       selected = "search")
        
        # Show the "complete" modal
        showModal(
          modalDialog(
            title = "Upload your PDFs",
            
            fluidPage(
              align = "center",
              h1("Upload complete"),
              br(),
              "Your PDFs will now appear when you search.",
              br(),
              "You can use the 'source' filter on the search page to see just your PDFs, published papers, or both"
            ),
            easyClose = TRUE
          )
        )
      })
    
  })

  
}

