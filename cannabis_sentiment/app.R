
source("global.R")

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
        box(
            title="Timeseries",
            plotlyOutput("plot_area") %>% withSpinner(type=8),
            width=12,
            solidHeader=TRUE,
            collapsible=TRUE
        ),
        box(
            title="Summary",
            div(
                plotOutput("plot_strip") %>% withSpinner(),
                inline=TRUE,
                style="display:inline-;float: left; width: 30%"
            ),
            div(
                p(text_summary), 
                inline=TRUE,
                style="display:inline-block;float: right; width: 70%"
              ),
            width=6,
            solidHeader=TRUE,
            collapsible=TRUE
            ),
        box(
            title="Background and method",
            div(
                box(
                    title = "Background",
                    p(text_background),
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    width = 12
                ),
                box(
                    title = "Method",
                    p(text_method),
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    collapsed=TRUE,
                    width = 12
                ),
                box(
                    title = "How to use",
                    p(text_how_to),
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    collapsed=TRUE,
                    width = 12
                ),
                box(
                    title = "About Us",
                    p(text_about_us),
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    collapsed=TRUE,
                    width = 12
                )
                ),
            width=6,
            solidHeader=TRUE,
            collapsible=TRUE
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