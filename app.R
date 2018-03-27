# Load neccesary packages -----------------------------------------------------------

if(!"shiny" %in% installed.packages()) 
{ 
    install.packages("shiny") 
}
library(shiny)

# UI
ui <- fluidPage(
)

# Server
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)

