#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Download relevant library

library(shiny)
library(shinythemes)
library(tools)
library(stringr)
library(ggplot2)
library(ggthemes)
library(gt)
library(tidyverse)

# Source helper file

source("data_helper.R")

# Define UI with dark theme

ui <- fluidPage(theme = shinytheme("slate"),
                
  # Application title
  
  titlePanel("Organ Donations and Transplants"),
  
  helpText("A Gov 1005 Final Project by Shafi Rubbani, Spring 2019"),
  
  # Tabs contain different page layout
  
  tabsetPanel(
    type = "tabs",
    tabPanel("Summary", "Taco"),
    tabPanel("Transplant Rates Across Countries",
             
             # Sidebar with inputs for plot
             
             sidebarLayout(
               
               # Display transplants plot
               
               mainPanel(plotOutput("transplantsPlot")),
               
               # Inputs: country or countries, organ, and measure (absolute number vs. per million people)
               
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
    tabPanel("Transplant Proportions",
             
             # Sidebar with inputs for plot
             
             sidebarLayout(
               
               # Display transplants table
               
               mainPanel(gt_output("transplantsTable")),
               sidebarPanel(
                 selectInput(
                   inputId = "country",
                   label = "Country",
                   choices = countries,
                   selected = "US",
                   multiple = FALSE
                 )
               )
             )),
    tabPanel("About", "Tuesday")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$transplantsPlot <- renderPlot({
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
        color = "Country",
        title = paste(toTitleCase(input$organ),
                      " Transplant Trends in ",
                      if(length(input$country) == 1) {toupper(input$country)}
                      else {"Different Countries"},
                      sep = ""),
        caption = "Source: IRODaT Free Database") +
      theme_economist()
  })
  
  output$transplantsTable <- render_gt({
    req(input$country)
    
    transplant_table <- all_transplants %>% 
      filter(country == "US") %>% 
      filter(measure == "num") %>% 
      spread(key = organ, value = transplants)
    
    transplant_table[is.na(transplant_table)] <- 0
    
    transplant_table %>% 
      mutate(total = heart + kidney + liver + lung + pancreas) %>% 
      mutate(heart = heart / total) %>% 
      mutate(kidney = kidney / total) %>% 
      mutate(liver = liver / total) %>% 
      mutate(lung = lung / total) %>% 
      mutate(pancreas = pancreas / total) %>% 
      select(-country, -measure, -total) %>% 
      na_if(0) %>% 
      arrange(desc(year)) %>% 
      gt() %>% 
      #Set title
      tab_header(title = paste("Transplant Proportions in ", input$country)) %>% 
      #Label columns
      cols_label(
        year = "Year",
        heart = "Heart",
        kidney = "Kidney",
        liver = "Liver",
        lung = "Lung",
        pancreas = "Pancreas"
      ) %>%
      tab_source_note("Source: IRODaT Free Database") %>% 
      #Format proportions as percentages
      fmt_percent(columns = vars(heart,
                                 kidney,
                                 liver,
                                 lung,
                                 pancreas),
                  decimals = 0) %>% 
      tab_options(
        table.background.color = "#D5E5EB"
      )
  })
  
  output$about <- renderUI({
    
    ###
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)