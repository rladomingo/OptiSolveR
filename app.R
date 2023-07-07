# Source UI and server files
source("ui.R")
source("server.R")

# Load R packages
if (!require(shiny)) install.packages('shiny')
if (!require(shinythemes)) install.packages('shinythemes')
if (!require(shinyMatrix)) install.packages('shinyMatrix')

# Create Shiny object
shinyApp(ui = ui, server = server)