#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load relevant libraries

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

# Define UI with flatly theme

ui <- fluidPage(theme = shinytheme("flatly"),
                
  # Application title
  
  titlePanel("Organ Donations and Transplants"),
  
  # A subtitle containing identifying information
  
  helpText("A Gov 1005 Final Project by Shafi Rubbani, Spring 2019"),
  
  # Tabs contain different page layouts
  
  tabsetPanel(
    type = "tabs",
    
    # The summary panel contains plots and tables giving snapshots of the
    # international organ donation and transplantation scene
    
    tabPanel("Summary", mainPanel(
      # helpText("The following countries had the highest donation rates (per million people) in 2017."),
      plotOutput("top5_donations"),
      br(),
      plotOutput("top5_transplants"),
      br(),
      gt_output("donations_change"),
      br(),
      gt_output("transplants_change")
      )),
    
    # The donation rates panel contains a plot showing organ donation trends over
    # time. By selecting multiple countries, users can compare donation trends.
    
    tabPanel("Donation Rates Across Countries",
             
             # Sidebar with inputs for plot
             
             sidebarLayout(
               
               # Display donations plot
               
               mainPanel(plotOutput("donationsPlot")),
               
               # Inputs: country or countries, donation type, and measure (absolute number vs. per million people)
               
               sidebarPanel(
                 helpText("Select Actual to count donations after death, Utilized to count donations that were used, and Living to count living donations."),
                 selectInput(
                   inputId = "country1",
                   label = "Country",
                   choices = named_countries,
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
    
    # The transplant rates panel contains a plot showing transplant trends over
    # time. By selecting multiple countries, users can compare transplant trends.
    
    tabPanel("Transplant Rates Across Countries",
             
             # Sidebar with inputs for plot
             
             sidebarLayout(
               
               # Display transplants plot
               
               mainPanel(plotOutput("transplantsPlot")),
               
               # Inputs: country or countries, organ, and measure (absolute number vs. per million people)
               
               sidebarPanel(
                 helpText("Use the dropdown menus to observe transplant rates by organ and compare rates across countries."),
                 selectInput(
                   inputId = "country2",
                   label = "Country",
                   choices = named_countries,
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
    
    # The proportions tab enables users to look at a breakdown of transplant
    # proportions in recent years.
    
    tabPanel("Transplant Proportions",
             
             # Sidebar with inputs for plot
             
             sidebarLayout(
               
               # Display transplants table
               
               mainPanel(gt_output("transplantsTable")),
               
               # Inputs: country
               
               sidebarPanel(
                 helpText("The table shows organ transplant proportions in recent years."),
                 selectInput(
                   inputId = "country3",
                   label = "Country",
                   choices = named_countries,
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
  
  output$top5_donations <- renderPlot({
    all_donations %>% 
      filter(!is.na(donations)) %>% 
      filter(measure == "pmp") %>% 
      filter(type == "actual") %>% 
      filter(year == 2017) %>% 
      arrange(desc(donations)) %>% 
      mutate(name = fct_reorder(name, donations)) %>% 
      head(5) %>% 
      ggplot(aes(x = name, y = donations, fill = name)) +
      geom_col(show.legend = FALSE) +
      labs(
        x = "Country",
        y = "Donations (pmp)",
        fill = "Country",
        title = paste("Top 5 Donation Rates in 2017"),
        caption = "Source: IRODaT Free Database") +
      theme_igray()
  })
  
  output$top5_transplants <- renderPlot({
    all_transplants %>% 
      filter(!is.na(transplants)) %>% 
      filter(measure == "pmp") %>% 
      filter(year == 2017) %>% 
      group_by(country, name) %>% 
      summarize(transplants = sum(transplants)) %>% 
      ungroup() %>% 
      arrange(desc(transplants)) %>% 
      mutate(name = fct_reorder(name, transplants)) %>% 
      head(5) %>% 
      ggplot(aes(x = name, y = transplants, fill = name)) +
      geom_col(show.legend = FALSE) +
      labs(
        x = "Country",
        y = "Transplants (pmp)",
        fill = "Country",
        title = paste("Top 5 Transplant Rates in 2017"),
        caption = "Source: IRODaT Free Database") +
      theme_igray()
  })
  
  output$donations_change <- render_gt({
    all_donations %>% 
      filter(type == "actual") %>% 
      filter(measure == "pmp") %>% 
      filter(year %in% c(2012, 2017)) %>% 
      spread(key = year, value = donations) %>% 
      filter(!is.na(`2012`)) %>% 
      filter(!is.na(`2017`)) %>% 
      mutate(change = `2017` - `2012`) %>% 
      select(-type, -measure, -donor_status) %>% 
      arrange(desc(change)) %>% 
      head(5) %>%
      select(-country) %>%
      gt() %>%
      #Set title
      tab_header(title = "Largest Donation Rate Increases (pmp) 2012 to 2017") %>%
      #Label columns
      cols_label(
        name = "Country",
        `2012` = "Donation Rate 2012",
        `2017` = "Donation Rate 2017",
        change = "Difference"
      ) %>%
      tab_source_note("Source: IRODaT Free Database")
  })
  
  output$transplants_change <- render_gt({
    all_transplants %>% 
      filter(!is.na(transplants)) %>% 
      filter(measure == "pmp") %>% 
      filter(year %in% c(2012, 2017)) %>% 
      group_by(country, year, name) %>% 
      summarize(transplants = sum(transplants)) %>% 
      ungroup() %>% 
      mutate(name = fct_reorder(name, transplants)) %>% 
      spread(key = year, value = transplants) %>% 
      filter(!is.na(`2012`)) %>% 
      filter(!is.na(`2017`)) %>% 
      mutate(change = `2017` - `2012`) %>% 
      arrange(desc(change)) %>% 
      head(5) %>% 
      select(-country) %>%
      gt() %>%
      #Set title
      tab_header(title = "Largest Transplant Rate Increases (pmp) 2012 to 2017") %>%
      #Label columns
      cols_label(
        name = "Country",
        `2012` = "Transplant Rate 2012",
        `2017` = "Transplant Rate 2017",
        change = "Difference"
      ) %>%
      tab_source_note("Source: IRODaT Free Database")
  })
  
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
                      if(length(input$country1) == 1) {toupper(input$country1)}
                      else {"Different Countries"},
                      sep = ""),
        caption = "Source: IRODaT Free Database") +
      theme_igray()
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
        y = paste("Transplants (", input$measure2, ")", sep = ""),
        color = "Country",
        title = paste(toTitleCase(input$organ),
                      " Transplant Trends in ",
                      if(length(input$country2) == 1) {toupper(input$country2)}
                      else {"Different Countries"},
                      sep = ""),
        caption = "Source: IRODaT Free Database") +
      theme_igray()
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
      select(-country, -measure, -total, -name) %>% 
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
                  decimals = 0) 
  })
  
  output$acknowledgments <- renderUI({
    
    paragraph1 <- p(tags$strong("Organ Donations and Transplants"),
                    "was a project created for Gov 1005: Data, a course taught by David Kane at Harvard College. The syllabus for the course can be found ",
                    tags$a(href="https://www.davidkane.info/files/gov_1005_spring_2019.html", "here."),
                    "I want to thank Preceptor, Albert Rivero, and Jacob Brown for all their help and guidance throughout the semester.", sep = "")
    
    paragraph2 <- p("This website was produced using Shiny for R. Many thanks to the folks at RStudio for all the work they do to support the R community.")
    
    paragraph3 <- p("The data for this project was gathered the International Registry for Organ Donations and Transplants. Without their generously provided free database, the data for all the visualizations shown here would have been impossible to gather. The IRODaT website can be found",
                    tags$a(href="http://www.irodat.org/", "here."))
    
    paragraph4 <- p("The code behind these visualizations can be found in ",
                    tags$a(href="https://github.com/ShafiRubbani/donations_and_transplants", "this GitHub repository."))
    
    HTML(paste(br(),
               paragraph1,
               paragraph2,
               paragraph3,
               paragraph4))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)