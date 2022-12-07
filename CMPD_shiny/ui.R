#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://mastering-shiny.org/action-layout.html
#

library(shiny)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(plotly)

df <- read_csv("finalDF2.csv")
means_out <- read_csv("finalDF2.csv")
data_input <- read_csv("finalDF2.csv")

year1      <- df %>% select(YEAR) %>% distinct() %>% pull()
division1  <- df %>% select(CMPD_PATROL_DIVISION) %>% distinct() %>% pull()


not_sel <- "Not Selected"


about_page <- tabPanel(
  title = "About",
  titlePanel("About"),icon = icon("archive"),
  "Created with R Shiny",
  br(),
  "2022 December", br(),
  column(10,includeMarkdown("about.md"))
)


dashboard_page <- tabPanel(
  title = "Dashboard",  icon = icon("dashboard"),
  titlePanel("Charlotte Mecklenburg Police Department (CMPD) Dashboard"),
  fluidPage(
    img(src = 'pic_skyline1.jpeg', height = 700, width = 900, align = "right")
  )
)


explore_page <- tabPanel(
  title = "Data Exploration",  icon = icon("list-alt"),
  titlePanel("Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
    #Get Rows
    selectInput("YearGet1", label = "Choose Year", year1, multiple = TRUE, selected = year1),
    selectInput("DivisionGet1", label = "Choose Division", 
                division1, multiple = TRUE, selected = division1),
    #Get Columns
    uiOutput("colControls"),
    div(style="text-align:left","Select Columns:"),
    textOutput("selectedTextc"),
    #Get select
    selectInput("num_var_1", "Numerical Variable 1", choices = c(not_sel)),
    selectInput("num_var_2", "Numerical Variable 2", choices = c(not_sel)),
    selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
    br(),br(),br(),
    actionButton("run_button", "Run Analysis", icon = icon("play")),
  ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Data Selection", icon = icon("table"),
          DTOutput("tbl")
        ),
        tabPanel(
          title = "Visualization", icon = icon("bar-chart-o"),
          plotOutput("plot")
        ),
        tabPanel(
          title = "Summary", icon = icon("list-alt"),
              fluidRow(
                column(width = 4, strong(textOutput("num_var_1_title"))),
                column(width = 4, strong(textOutput("num_var_2_title"))),
                column(width = 4, strong(textOutput("fact_var_title")))
              ),
              fluidRow(
                column(width = 4, tableOutput("num_var_1_summary_table")),
                column(width = 4, tableOutput("num_var_2_summary_table")),
                column(width = 4, tableOutput("fact_var_summary_table"))
              ),

        )
      )
    )
  )
)

  

    


#Modeling Tab
model_page <- tabPanel("Modeling", icon = icon("laptop"), titlePanel("Analysis"),
                       sidebarLayout(
                         sidebarPanel(title = "Inputs",

                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Modeling Info",
                                    column(10,includeMarkdown("info.md"))),
                        
                           tabPanel("Modeling Fitting"),
                      
                           tabPanel("Prediction", "Prediction")
   )
  )
 )
)


# Data Tab
data_page <- tabPanel(
  title = "Data",  icon = icon("archive"),
  fluidPage(titlePanel("Data"), 
    fluidRow(
    selectInput("YearGet", label = "Choose Year", year1, multiple = TRUE, selected = year1),
    selectInput("DivisionGet", label = "Choose Division", division1, multiple = TRUE, selected = division1),
    downloadButton("download1","Download entire Table  as csv"),
    mainPanel(DT::dataTableOutput("iris_dto"))
        )
  )
)


# Main that will render pages
# https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7

ui <- navbarPage(
  title = "Data Analyser",
  theme = shinytheme('united'),
  dashboard_page,
  about_page,
  explore_page,
  model_page,
  data_page
)

  
