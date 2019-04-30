#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tools)
library(stringr)
library(ggplot2)
library(ggthemes)
library(tidyverse)

source("data_helper.R")

# Define UI
ui <- fluidPage(theme = shinytheme("slate"),
  # Application title
  titlePanel("Organ Donations and Transplants"),
  
  helpText("A Gov 1005 Final Project by Shafi Rubbani, Spring 2019"),
  
  # Tabs contain different page layout
  tabsetPanel(
    type = "tabs",
    tabPanel("Plot",
             # Sidebar with inputs for plot
             sidebarLayout(
               mainPanel(plotOutput("donationsPlot")),
               sidebarPanel(
                 selectInput(
                   inputId = "country",
                   label = "Country",
                   choices = countries,
                   selected = "US",
                   multiple = TRUE
                 ),
                 selectInput(
                   inputId = "organ",
                   label = "Organ",
                   choices = organs,
                   selected = "kidney",
                   multiple = FALSE
                 ),
                 selectInput(
                   inputId = "measure",
                   label = "Measure",
                   choices = c("Number" = "num",
                               "Per Million People" = "pmp"),
                   selected = "num",
                   multiple = FALSE
                 )
               )
             )),
    tabPanel("Summary", "Taco"),
    tabPanel("About", "Tuesday")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$donationsPlot <- renderPlot({
    req(input$country)
    
    all_transplants %>% 
      filter(!is.na(transplants)) %>% 
      filter(organ == input$organ) %>% 
      filter(measure == input$measure) %>% 
      filter(country == input$country) %>% 
      ggplot(aes(x = year, y = transplants, color = country)) +
      geom_point() +
      labs(
        x = "Year",
        y = paste("Transplants (", input$measure, ")", sep = ""),
        #title = (length(input$country) == 1),
        color = "Country",
        title = paste(toTitleCase(input$organ),
                      " Transplant Trends in ",
                      if(length(input$country) == 1) {toupper(input$country)}
                      else {"Different Countries"},
                      sep = ""),
        caption = "Source: IRODaT Free Database") +
      theme_economist()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)