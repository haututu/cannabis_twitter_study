library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title="Cannabis Referendum Tracker", titleWidth = 300),
    dashboardSidebar(
        width = 300
    ),
    dashboardBody(
        tabBox(
            tabPanel("Plot",
                     p("some pretty plot")
                     ),
            tabPanel("Table",
                     p("some pretty table")
            )
        )
    )
)

server <- function(input, output) {
    
}

shinyApp(ui, server)