
library(shiny)

options(shiny.host = "127.0.0.1", shiny.port = 7676) 

runApp(host = getOption("shiny.host"), 
       port = getOption("shiny.port"),
       launch.browser = FALSE)

#shinyApp(ui = ui, server = server, global = global)

# http://127.0.0.1:7676  