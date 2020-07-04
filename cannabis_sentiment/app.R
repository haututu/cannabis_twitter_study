library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(wesanderson)

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
                           selected = "year",
                           choices = c("Day" = "day", "Month" = "month", "Year" = "year")),
        uiOutput("date_slider")
        ),
    dashboardBody(
        tabBox(
            tabPanel("Plot",
                     plotlyOutput("plot_area")
                     ),
            tabPanel("Table",
                     p("some pretty table")
            ),
            width=12
        ),
        box(
            title="Summary",
            plotOutput("plot_strip"),
            "Stacked summary thing",
            width=6
        ),
        box(
            title="Background and method",
            "Blah blah preambly stuff and method",
            width=6
        )
    )
)

server <- function(input, output) {
    
    output$date_slider <- renderUI({
        sliderInput("date_slider", label = "Date Range", min = min(dat$date), max = max(dat$date), value = c(min(dat$date), max(dat$date)))
    })
    
    output$plot_area <- renderPlotly({
        ggplotly(
            dat %>%
                filter(retweets == input$retweets & 
                           topic == input$topic &
                           measure == input$measure & 
                           rollup == input$rollup) %>%
                filter(between(date, input$date_slider[1], input$date_slider[2])) %>%
                ggplot(aes(x=date, y=value, fill=valence)) +
                scale_fill_manual(values = wes_palette("Zissou1")) +
                geom_area() +
                theme_classic()
            )
    })
    
    output$plot_strip <- renderPlot({
            dat %>%
                filter(retweets == input$retweets & 
                           topic == input$topic &
                           measure == "count" & 
                           rollup == "day") %>%
                filter(between(date, input$date_slider[1], input$date_slider[2])) %>%
                group_by(valence) %>%
                summarise(value = sum(value)) %>%
                ungroup() %>%
                mutate(total = "Total") %>%
                ggplot(aes(x=total, y=value, fill=valence)) +
                scale_fill_manual(values = wes_palette("Zissou1")) +
                geom_bar(position="stack", stat="identity") +
                theme_void() +
                theme(legend.position="bottom")
    })
}

shinyApp(ui, server)