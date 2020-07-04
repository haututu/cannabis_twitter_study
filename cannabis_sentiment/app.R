library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)

dat <- readr::read_csv("cannabis_data.csv") %>%
    na.omit()

ui <- dashboardPage(
    dashboardHeader(title="Cannabis Referendum Tracker", titleWidth = 300),
    dashboardSidebar(
        width = 300,
        prettyRadioButtons(inputId = "retweets",
                           label = "Tweet filter",
                           choices = c("Include retweets" = TRUE, "Unique Tweets" = FALSE)),
        prettyRadioButtons(inputId = "topic",
                           label = "Topic filter",
                           choices = c("Referendum only" = "referendum", "Cannabis generally" = "cannabis")),
        prettyRadioButtons(inputId = "measure",
                           label = "Measure",
                           choices = c("Count" = "count", "Proportion" = "proportion")),
        prettyRadioButtons(inputId = "rollup",
                           label = "Time scale for total Tweets",
                           choices = c("Day" = "day", "Month" = "month", "Year" = "year")),
        uiOutput("date_slider")
        ),
    dashboardBody(
        tabBox(
            tabPanel("Plot",
                     plotlyOutput("plot")
                     ),
            tabPanel("Table",
                     p("some pretty table")
            ),
            width=12
        )
    )
)

server <- function(input, output) {
    
    output$date_slider <- renderUI({
        sliderInput("date_slider", label = "Date Range", min = min(dat$date), max = max(dat$date), value = c(min(dat$date), max(dat$date)))
    })
    
    output$plot <- renderPlotly({
        ggplotly(
            dat %>%
                filter(retweets == input$retweets & 
                           topic == input$topic &
                           measure == input$measure & 
                           rollup == input$rollup) %>%
                filter(between(date, input$date_slider[1], input$date_slider[2])) %>%
                ggplot(aes(x=date, y=value, color=valence, fill=valence)) +
                geom_area() +
                theme_classic()
            )
    })
}

shinyApp(ui, server)