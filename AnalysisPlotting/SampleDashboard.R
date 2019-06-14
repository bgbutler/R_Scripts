library(shinydashboard)
library(shiny)
ui <- dashboardPage(
    dashboardHeader(title = "Quick Example"),
    dashboardSidebar(textInput("text", "Text")),
    dashboardBody(
        valueBox(100, "Basic example"),
        tableOutput("mtcars")
    )
)
server <- function(input, output) {
    output$mtcars <- renderTable(head(mtcars))
}
shinyApp(ui, server)