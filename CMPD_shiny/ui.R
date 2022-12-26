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
library(caret)
library(randomForest)

df <- read_csv("df2022.csv")

NIBRS1     <- df %>% select(NIBRS) %>% distinct() %>% pull()
division1  <- df %>% select(DIVISION) %>% distinct() %>% pull()
location1  <- df %>% select(LOCATION) %>% distinct() %>% pull()
Month1     <- df %>% select(MONTH) %>% distinct() %>% pull()
not_sel    <- "Not Selected"



setBackgroundColor(
  color = "Cornsilk",
  gradient = c("linear"),
  direction = c("bottom"),
  shinydashboard = TRUE
)

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
    selectInput("NIBRSGet1", label = "Choose Crime", NIBRS1, multiple = TRUE, selected = NIBRS1),
    selectInput("DivisionGet1", label = "Choose Division", 
                division1, multiple = TRUE, selected = division1),
    selectInput("LocationGet1", label = "Choose Location", 
                location1, multiple = TRUE, selected = location1),
    selectInput("MonthGet1", label = "Choose Summer Month", 
                Month1, multiple = TRUE, selected = Month1),
    #Get Columns
    uiOutput("colControls"),
    div(style="text-align:left","Select Columns:"),
    textOutput("selectedTextc"),
    #Get select
    selectInput("num_var_1", "Numerical Variable 1", choices = c(not_sel)),
    selectInput("num_var_2", "Numerical Variable 2", choices = c(not_sel)),
    selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
    br(),
    actionButton("run_button", "Run Analysis", icon = icon("play")),
  ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Instructions", #icon = icon("fa-solid fa-info"),
          column(10,includeMarkdown("help.md"))
        ),
        tabPanel(
          title = "Data Selection", icon = icon("table"),
          DTOutput("tbl")
        ),
        tabPanel(
          title = "Visualization", #icon = icon("fa fa-bar-chart-o"),
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
model_page <- tabPanel("Modeling", icon = icon("laptop"), titlePanel("Modeling Data"),
                       sidebarLayout(
                         sidebarPanel(title = "Inputs",
                              h3(" Add some instructions"),
                              #Get proportions
                              selectInput(inputId = "n_prop",
                              label = "Choose Partition Proportion:",
                              choices = c(0.65, 0.70, 0.75, 0.80),
                              selected = .75),
                              #Get Predictors
                              uiOutput("colPredict"),
                              div(style="text-align:left","Select Predictors:"),
                              textOutput("selectedTextp"),
                              #Get Preprocess
                              checkboxInput("preprocessMe", 
                                            "PreProcess with center & scale?", 
                                            value = TRUE),
                              #Get proportions
                              selectInput(inputId = "predictYr",
                              label = "Choose Year for Analysis:",
                              choices = c(2022, 2021, 2020, 2019,2018, 2017),
                              selected = 2019),
                              br(),
                              actionButton("run_model", "Run Analysis", icon = icon("play")),
                              
                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Modeling Info",
                                    column(10,includeMarkdown("info.md")),
                                    DTOutput("tbl2"),
                                    ),

                           tabPanel("Modeling Fitting",
                                    #plotOutput("treeplot"),
                                    #verbatimTextOutput("glmsummary"),
                                    plotOutput("rfplot"),
                                    ), 
                      
                           tabPanel("Prediction", "Prediction")
   )
  )
 )
)

# Data Tab
data_page <- tabPanel(
  title = "Data",  #icon = icon("fa-solid fa-database"),
  fluidPage(titlePanel("Data for Download"), 
    fluidRow(
    selectInput("MonthGet", label = "Choose Summer Month(s)", Month1, multiple = TRUE, selected = Month1),
    selectInput("DivisionGet", label = "Choose Division", division1, multiple = TRUE, selected = division1),
    selectInput("NIBRSGet", label = "Choose Crime", NIBRS1, multiple = TRUE, selected = NIBRS1),
    selectInput("LocationGet", label = "Choose Location", location1, multiple = TRUE, selected = location1),
    downloadButton("download1","Download entire Table  as csv"),
    mainPanel(DT::dataTableOutput("cmpd_dto"))
        )
  )
)


# Main that will render pages
# https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7

ui <- navbarPage(
  setBackgroundColor("LightYellow"),
  title = "Data Analyser",
  theme = shinytheme('united'),
  dashboard_page,
  about_page,
  explore_page,
  model_page,
  data_page
)

  
