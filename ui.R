fluidPage(
  #use_tota11y(),
  dashboardPage(title = "Sage Summariser",
              
              # Header
              #tags$style(HTML(".sidebar-menu li a { font-size: 20px; }")),
              dashboardHeader(title = "Sage Summariser"
              ),
              
              # Sidebar menu items
              dashboardSidebar(collapsed = FALSE,
                               sidebarMenu(id = "tabs", # for icons: https://fontawesome.com/icons?d=gallery&m=free
                                           menuItem("Home", tabName = "home", icon = icon("home")),
                                           menuItem("Search for papers", tabName = "search", icon = icon("search")),
                                           br(),
                                           actionButton("upload", "Upload PDFs")
                                           
                               )
              ),
              
              # Main body
              dashboardBody(
                
                # Custom styling
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                  tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Domine"),
                  tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Roboto"),
                  tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Libre Baskerville"),
                  tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Open Sans"),
                  tags$style(HTML(".sidebar-menu li a { font-size: 20px;}"))
                ),
                
                # Tabs
                tabItems(
                  
                  # ---- Home page ----
                  # When the user returns from authenticating token, this will be the page 
                  # they are redirected to 
                  tabItem(tabName = "home",
                          
                          fluidRow(
                            #column(width = 1),
                            column(width = 12, 
                                   align="center",
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   jumbotron(header = h1("SAGE Summariser",
                                                         style = "color: #FFF;"), #img(src='logo.png', width = 100)
                                             content = list(h2("Your gateway to The Science",
                                                              style = "color: #FFF;"),
                                                            br(),
                                                            actionBttn("getStarted", "Get started", size = "lg")),
                                             button = FALSE)
                                   
                            )
                            #column(2)
                          )
                  ),
                
                  # ---- Search ----
                  tabItem(tabName = "search",
                          
                          # Center align Container
                          fluidRow(
                            div(class = "container", id="search_container",
                                
                                div(class="row justify-content-md-center",
                                    
                                    box(title = NULL,
                                        solidHeader = FALSE,
                                        width = 12, 
                                        id = 'search_container',
                                        
                                        # Heading
                                        h2("Discover"),
                                        
                                        # Search bar and results
                                        textInput("search_words", "Search for key words"),
                                        br(),
                                        column(
                                          width = 3,
                                          selectizeInput(
                                            inputId = 'topic_filter',
                                            label = "Filter by topic",
                                            choices = topic_labels$label,
                                            multiple = TRUE)
                                        ),
                                        column(
                                          width = 3,
                                          dateRangeInput(
                                            inputId="date_filter",
                                            label = "Filter by date last modified",
                                            start = "2020-01-01",
                                            end = NULL,
                                            min = "2020-01-01",
                                            max = NULL
                                          )
                                        ),
                                        column(
                                          width = 3,
                                          selectizeInput(
                                            inputId = "meeting_filter",
                                            label = "Filter by SAGE meeting",
                                            choices = meeting_list,
                                            multiple = TRUE
                                          )
                                        ),
                                        
                                        column(
                                          width = 3,
                                          selectizeInput(
                                            inputId = "source_filter",
                                            label = "Filter by source",
                                            choices = c("Published", "Uploaded by you"),
                                            multiple = TRUE
                                          )
                                        ),
                                        
                                        # Search results
                                        withSpinner(DT::dataTableOutput("search_outputs"), type = 8) 
                                    )
                                )
                            )
                          )
                          
                  )
                )
              )
)
)                
  
