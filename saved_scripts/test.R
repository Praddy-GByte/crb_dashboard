library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Test"),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server) 