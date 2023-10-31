#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
data <- read_excel("accident.xlsx")
data$Deaths <- data$FATALS
data$Month_Name <- data$MONTHNAME
data$Weekday_Name <- data$DAY_WEEKNAME
data$State_Name <- data$STATENAME
data1 <- data[c("State_Name", "Month_Name", "Deaths","Weekday_Name")]
library(maps)
library(mapproj)
library(ggplot2)
data1$State_Name <- tolower(data1$State_Name)
library(dplyr)
data2 <- data1 %>%
  group_by(State_Name) %>%
  summarize(Total_Deaths = sum(Deaths))


# Calculate the percentage of total deaths
data1 <- data1 %>%
  left_join(data2, by = "State_Name") %>%
  mutate(PCT_Deaths = (Total_Deaths / sum(Total_Deaths)) * 100)
distinct_data1 <- data1 %>% 
  distinct(State_Name, .keep_all = TRUE)
states <- map_data("state") 
map <- full_join(distinct_data1, states, by = c("State_Name" = "region"))  # Merge states and state.x77 data
map <- map[order(map$order), ]

# Define UI 
ui <- (fluidPage(
  titlePanel("Car Accident Deaths in the United States"),
  navbarPage(" ",
             tabPanel("Bar Plot",
                      sidebarLayout(
                      sidebarPanel(
                        selectInput("month2", "Select Month(s):",
                              choices = unique(data1$Month_Name), selected = unique(data1$Month_Name)[1], multiple = TRUE),
                        selectInput("day_week2", "Select Day of Week(s):",
                              choices = unique(data1$Weekday_Name), selected = unique(data1$Weekday_Name)[1], multiple = TRUE),
                        selectInput("state2", "Select State(s):",
                              choices = unique(data1$State_Name), selected = unique(data1$State_Name)[1], multiple = TRUE)),
                    mainPanel(
                      h4("Percentage of Deaths by State Bar Plot:"),
                      plotOutput("plot")))),
             tabPanel("Map Plot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("month", "Select Month(s):",
                                      choices = unique(data1$Month_Name), selected = unique(data1$Month_Name)[1], multiple = TRUE),
                          selectInput("day_week", "Select Day of Week(s):",
                                      choices = unique(data1$Weekday_Name), selected = unique(data1$Weekday_Name)[1], multiple = TRUE),
                          selectInput("state", "Select State(s):",
                                      choices = unique(data1$State_Name), selected = unique(data1$State_Name)[1], multiple = TRUE)),
                        mainPanel(
                          h4("Percentage of Death by State Map:"),
                          plotOutput("map"))))
))
)


server <- (function(input, output, session) {
  data_map <- reactive({
    data1[data1$Month_Name == input$month & data1$Weekday_Name == input$day_week & data1$State_Name == input$state, ]
  })
  
  output$map <- renderPlot({
    ggplot(map, aes(x = long, y = lat, group = group, fill = PCT_Deaths)) +
      geom_polygon() +
      geom_path() +
      scale_fill_gradientn(colours = rev(heat.colors(10)), name = "Percentage of Total Deaths") +
      coord_map() +
      labs(x = "Longitude", y = "Latitude")
  })
  
  
  
  data_plot <- reactive({
    data1[data1$Month_Name == input$month2 & data1$Weekday_Name == input$day_week2 & data1$State_Name %in%  input$state2, ]
  })
  
  
  output$plot <- renderPlot({
    data3 <- data_plot()
    
    ggplot(data3, aes(x = State_Name, y = PCT_Deaths, fill = State_Name)) +
      geom_bar(stat = "identity") +
      labs(x = "State", y = "Percentage of Deaths") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_discrete(name = "State")
  })
  
  
})

# Run the application 
shinyApp(ui, server)



