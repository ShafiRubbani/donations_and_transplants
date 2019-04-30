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
    tabPanel("Donation Rates Across Countries",
             
             # Sidebar with inputs for plot
             
             sidebarLayout(
               
               # Display transplants plot
               
               mainPanel(plotOutput("donationsPlot")),
               
               # Inputs: country or countries, organ, and measure (absolute number vs. per million people)
               
               sidebarPanel(
                 selectInput(
                   inputId = "country1",
                   label = "Country",
                   choices = countries,
                   selected = "US",
                   multiple = TRUE
                 ),
                 selectInput(
                   inputId = "type",
                   label = "Type",
                   choices = c("Actual" = "actual",
                               "Utilized" = "utilized",
                               "Living" = "living"),
                   selected = "actual",
                   multiple = FALSE
                 ),
                 selectInput(
                   inputId = "measure1",
                   label = "Measure",
                   choices = c("Number" = "num",
                               "Per Million People" = "pmp"),
                   selected = "num",
                   multiple = FALSE
                 )
               )
             )),
    tabPanel("Transplant Rates Across Countries",
             
             # Sidebar with inputs for plot
             
             sidebarLayout(
               
               # Display transplants plot
               
               mainPanel(plotOutput("transplantsPlot")),
               
               # Inputs: country or countries, organ, and measure (absolute number vs. per million people)
               
               sidebarPanel(
                 selectInput(
                   inputId = "country2",
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
                   inputId = "measure2",
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
                   inputId = "country3",
                   label = "Country",
                   choices = countries,
                   selected = "US",
                   multiple = FALSE
                 )
               )
             )),
    tabPanel("Acknowledgments", htmlOutput("acknowledgments"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$donationsPlot <- renderPlot({
    req(input$country1)
    
    all_donations %>% 
      filter(!is.na(donations)) %>% 
      filter(measure == input$measure1) %>% 
      filter(type == input$type) %>% 
      filter(country %in% input$country1) %>% 
      ggplot(aes(x = year, y = donations, color = country)) +
      geom_point() +
      scale_x_continuous(breaks = seq(1993, 2018, by = 2)) +
      labs(
        x = "Year",
        y = paste("Donations (", input$measure1, ")", sep = ""),
        color = "Country",
        title = paste("Organ Donation Trends in ",
                      if(length(input$country) == 1) {toupper(input$country)}
                      else {"Different Countries"},
                      sep = ""),
        caption = "Source: IRODaT Free Database") +
      theme_economist()
  })
  
  output$transplantsPlot <- renderPlot({
    req(input$country2)
    
    all_transplants %>% 
      filter(!is.na(transplants)) %>% 
      filter(organ == input$organ) %>% 
      filter(measure == input$measure2) %>% 
      filter(country %in% input$country2) %>% 
      ggplot(aes(x = year, y = transplants, color = country)) +
      geom_point() +
      scale_x_continuous(breaks = seq(1993, 2018, by = 2)) +
      labs(
        x = "Year",
        y = paste("Transplants (", input$measure, ")", sep = ""),
        color = "Country",
        title = paste(toTitleCase(input$organ),
                      " Transplant Trends in ",
                      if(length(input$country2) == 1) {toupper(input$country2)}
                      else {"Different Countries"},
                      sep = ""),
        caption = "Source: IRODaT Free Database") +
      theme_economist()
  })
  
  output$transplantsTable <- render_gt({
    req(input$country3)
    
    transplant_table <- all_transplants %>% 
      filter(country == input$country3) %>% 
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
      tab_header(title = paste("Transplant Proportions in ", input$country3)) %>% 
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
  
  output$acknowledgments <- renderUI({
    
    paragraph1 <- p(tags$strong("Organ Donations and Transplants"),
                    "was a project created for Gov 1005: Data, a course taught by David Kane at Harvard College. The syllabus for the course can be found ",
                    tags$a(href="https://www.davidkane.info/files/gov_1005_spring_2019.html", "here."),
                    "I want to thank Preceptor, Albert Rivero, and Jacob Brown for all their help and guidance throughout the semester.", sep = "")
    
    paragraph2 <- p("This website was produced using Shiny for R. Many thanks to the folks at RStudio for all the work they do to support the R community.")
    
    paragraph3 <- p("The data for this project was gathered the International Registry for Organ Donations and Transplants. Without their generously provided free database, the data for all the visualizations shown here would have been impossible to gather. The IRODaT website can be found",
                    tags$a(href="http://www.irodat.org/", "here."))
    
    HTML(paste(br(),
               paragraph1,
               paragraph2,
               paragraph3))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)