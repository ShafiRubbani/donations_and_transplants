#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)

source("data_helper.R")

# Define UI for application that draws a histogram
ui <- 

# fluidPage(
#   titlePanel("Organ Donations and Transplants"),
#   helpText("A Gov 1005 Final Project by Shafi Rubbani, Spring 2019"),
#   sidebarLayout(
#     sidebarPanel(
#       helpText("In this project, I explore the magic of Taco Tuesday, ay! Taquito Thursday, oh!")
#     ),
#     mainPanel(
#       tabsetPanel(type = "tabs",
#         tabPanel("Plot",
#                  sidebarLayout(position = "right",
#                    sidebarPanel(
#                      selectInput(inputId = "country",
#                                  label = "Country",
#                                  choices = countries,
#                                  selected = "AU",
#                                  multiple = FALSE),
#                      selectInput(inputId = "organ",
#                                  label = "Organ",
#                                  choices = organs,
#                                  selected = "kidney",
#                                  multiple = FALSE),
#                      selectInput(inputId = "measure",
#                                  label = "Measure",
#                                  choices = c(
#                                    "Number" = "num",
#                                    "Per Million People" = "pmp"
#                                  ),
#                                  selected = "num",
#                                  multiple = FALSE)
#                    ),
#                    mainPanel()
#                  )
#                  ),
#         tabPanel("Taco"),
#         tabPanel("Tuesday")
#       )
#     )
#   )
# )

# tabPanel("Plot"),
# tabPanel("Taco"),
# tabPanel("Tuesday")

fluidPage(
  # Application title
  titlePanel("Organ Donations and Transplants"),
  helpText("A Gov 1005 Final Project by Shafi Rubbani, Spring 2019"),
  
  # Show a plot of the generated distribution
  tabsetPanel(type = "tabs",
                tabPanel("Plot",
                         # Sidebar with a slider input for number of bins
                         sidebarLayout(
                           mainPanel(plotOutput("donationsPlot")),
                           sidebarPanel(
                             selectInput(inputId = "country",
                                         label = "Country",
                                         choices = countries,
                                         selected = "AU",
                                         multiple = FALSE),
                             selectInput(inputId = "organ",
                                         label = "Organ",
                                         choices = organs,
                                         selected = "kidney",
                                         multiple = FALSE),
                             selectInput(inputId = "measure",
                                         label = "Measure",
                                         choices = c(
                                           "Number" = "num",
                                           "Per Million People" = "pmp"
                                         ),
                                         selected = "num",
                                         multiple = FALSE)
                           )
                         ),
                tabPanel("Summary"),
                tabPanel("About")
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$donationsPlot <- renderPlot({
    all_transplants %>% 
      filter(!is.na(transplants)) %>% 
      filter(organ == input$organ) %>% 
      filter(measure == input$measure) %>% 
      filter(country == input$country) %>% 
      ggplot(aes(x = year, y = transplants)) +
      geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)