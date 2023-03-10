#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)

UAH <- read_delim("UAH-lower-troposphere-long (1) 2.csv")

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Temperate of Various Regions by Month and Year"),
    
      tabsetPanel(
        tabPanel("About",
        dataTableOutput("About"),
          p("This website aims to relay information regarding various regions temperate by", strong("year"), "and", strong("month"),".", 
            "The data being used, called", strong("UAH"), "provides valuable information on the temperature of regions changing
            over decades. The", em("plot"), "tab contains a graph that shows the average temperate in the regions by month. The",
            em("table"), "tab contains a table showing which years had the highest and lowest temperate by year. This data set contains 14310 rows
            and four columns. The table above shows a sample of the UAH data.")),


        tabPanel("Plot",
          p("This plot shows data regarding the average temperates in each region in the years between 1978-2023.
            The selection options on the left allow you to select which month`s data you would like to view."),
          sidebarLayout(
            sidebarPanel(
              checkboxGroupInput("month", label = "Select Month",
                                choices = c("1":"12"),
                                selected = c("1":"12")),
              radioButtons("color", "Select Color:",
                           choices = c("deepskyblue", "red", "lightpink", "khaki", "yellow", "green", "snow", "orange", "plum1", "azure", "deeppink", "darkgreen"),
                           selected = "deepskyblue"),
              textOutput("month_text")
                                
            ),
            mainPanel(plotOutput("Plot"))
        )),

        tabPanel("Table",
            p("This table shows information regarding the temperatures in various regions, organized by month and year. Use the slider
              on the left side of the page to choose which range of years you would like to view."),
         sidebarLayout(
           sidebarPanel(
             sliderInput("year_range", label = "Select year range",
                         min = min(UAH$year),
                         max = max (UAH$year),
                         value = c(1978, 2023)),
             textOutput("year_text"),
             
           ),
           mainPanel(
             dataTableOutput("dataTable"),
           )
       ))
    ))
 

# Server logic 
server <- function(input, output) {
  
  output$About <- renderDataTable({
    UAH %>% 
      head(5)
  })

  filtered_data <- reactive({
    UAH %>%
      filter(month %in% input$month)
  })
  
  avg_temp <- reactive({
    filtered_data() %>%
      group_by(region) %>%
      summarize(avg_temp = mean(temp))
  })
  
  output$Plot <- renderPlot({
    
    point_color <- switch(input$color,
                    deepskyblue = "deepskyblue",
                    red = "red",
                    lightpink = "lightpink",
                    khaki = "khaki",
                    yellow = "yellow",
                    green = "green",
                    snow = "snow",
                    orange = "orange",
                    plum1 = "plum1",
                    azure = "azure",
                    deeppink = "deeppink",
                    darkgreen = "darkgreen")
    avg_temp() %>%                 
    ggplot(aes(x = region, y = avg_temp)) +
    geom_point(color = point_color) +
    labs(x = "Region", y = "Average Temperature", 
         title = paste("Average Temperature in Each Region", input$month))
                    
  })

  output$dataTable <- renderDataTable({
    UAH %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2])
  })
  
  output$month_text <- renderText({
    paste("Selected month(s): ", paste(input$month, collapse = ", "))
  })
output$year_text <- renderText({
  paste("Currently selected: ", input$year_range[1], "-", input$year_range[2])
})  
}
# Run the application 
shinyApp(ui = ui, server = server)
