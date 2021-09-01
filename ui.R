#fluidPage(
#  use_tota11y(),
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
                                           menuItem("Accessibility", tabName = "accessibility"),
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
                  tags$html(lang = "en"),
                  #tags$style(HTML(".box-title h3 {font: #00bfa5;}")),
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
                                   custom_jumbotron(header = "SAGE Summariser",#, h1("SAGE Summariser",
                                                         # style = "color: #FFF;"), #img(src='logo.png', width = 100)
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
                                    
                                    custom_box(title = "Discover",
                                        solidHeader = FALSE,
                                        width = 12, 
                                        id = 'search_container',
                                        
                                        # Heading
                                        #h2("Discover"),
                                        
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
                                    )# end of box
                                )# end of div
                            )# end of second div
                          )),#end of fluid row and tab item,
                          tabItem(tabName = "accessibility",
                                  
                                  # Center align Container
                                  fluidRow(
                                    div(class = "container", id="search_container",
                                        
                                        div(class="row justify-content-md-center",
                                             custom_box(title = "Accessibility statement for the SAGE summariser",
                                                 solidHeader = NULL,
                                                 width = 12, 
                                                 accessibility_statement =  TRUE,
                                            h2("Using this website", style = "color: #000000;"),
                                            p("This website is run
                                            by the Department for Education (DfE). We want as many people as possible to be able to use this website. 
                                            For example, that means you should be able to: ", 
                                              style = "font-size: 18px;"),
                                            tags$li("navigate most of the website using just a keyboard and speech recognition software", style = "font-size: 18px;"),
                                            tags$li("navigate most of the website using a screen reader", style = "font-size: 18px;"),
                                            br(),
                                            h2("How accessible this website is", style = "color: #000000;"),
                                            p("We know that some parts of this website are not fully accessible. For example:", style = "font-size: 18px;"),
                                            tags$li("The sidebar does not fully comply with ARIA standards.", style = "font-size: 18px;"),
                                            tags$li("Some of the form controls on the search page may be difficult to navigate
                                            because of missing labels.", style = "font-size: 18px"),
                                            br(),
                                            h2("Reporting accessibility problems with this website", style = "color: #000000;"),
                                            p("We’re always looking to improve the accessibility of this website. If you find
                                              any problems not listed on this page or think we’re not meeting accessibility
                                              requirements, contact the", tags$a(href = 'mailto:das.lab@education.gov.uk', "DfE's Data Science Lab team"),
                                                                                 ". We will review your request and get back to you in 7 days.",
                                                                                 style = "font-size: 18px;"),
                                            br(),
                                            h2("Enforcement procedure", style = "color: #000000;"),
                                            p("The Equality and Human Rights Commission (EHRC) is responsible for
                                              enforcing the Public Sector Bodies (Websites and Mobile Applications) (No. 2)
                                              Accessibility Regulations 2018 (the ‘accessibility regulations’).
                                              If you’re not happy with how we respond to your complaint,", 
                                              tags$a(href = "https://www.equalityadvisoryservice.com/", 
                                                     "contact the Equality Advisory and Support Service (EASS)."), style = "font-size: 18px;"),
                                            br(),
                                            h2("Technical information about this website’s accessibility", style = "color: #000000;"),
                                            p("The Department for Education is committed to making its website accessible, in accordance
                                              with the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations 2018.", style = "font-size: 18px;"),
                                            br(),
                                            h3("Compliance status", style = "color: #000000;  font-weight: bold;"),
                                            p("This website is partially compliant with the Web Content Accessibility Guidelines version 2.1 AA standard,
                                            due to the exemptions listed below.", style = "font-size: 18px;"),
                                            br(),
                                            h2("Non-accessible content", style = "color: #000000;"),
                                            p("The content listed below is non-accessible for the following reasons.", style = "font-size: 18px;"),
                                            br(),
                                            h3("Disproportionate burden", style = "color: #000000;  font-weight: bold;"),
                                            h4("Navigation and interactive tools", style = "font-weight: bold;"),
                                            p("Using the sidebar with assistive technology may be difficult because some ARIA attributes
                                            do not match their roles.", style = "font-size: 18px;"),
                                            br(),
                                            p("Some of our interactive forms are difficult to navigate using a keyboard  
                                            because their form controls are missing a ‘label’ tag.", style = "font-size: 18px;"),
                                            br(),
                                            p("We have developed the site using the open-source technology R Shiny and as such are limited but the functionalities it offers. 
                                            Re-building the site using dedicated technologies for web development would be a disproportionate burden
                                                    within the meaning of the accessibility regulations. 
                                                    As the R shiny package improves its accessibility features we will reflect those on the site.",
                                              style = "font-size: 18px;"),
                                            br(),
                                            h2("How we tested this website", style = "color: #000000;"),
                                            p("This website was last tested on 22nd of June 2021. The test was carried out by the DfE.",
                                              style = "font-size: 18px;"),
                                            p("Testing was carried out using:"),
                                            tags$li(tags$a(href = "https://wave.webaim.org/", "the Web Accessibility Evaluation Tool (WAVE) website "), style = "font-size: 18px;"),
                                            tags$li("the Chrome extension 'Lighthouse'", style = "font-size: 18px;"),
                                            tags$li(tags$a(href = "https://github.com/ewenme/shinya11y", "the R package shinya11y"), style = "font-size: 18px;")
                                        ))))
                          )# end of tabItem
                )
              )
)
#)
