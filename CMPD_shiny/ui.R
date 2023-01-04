#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# 
#
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
library(rpart)              #building decision tree model
library(rattle)             #visualize the tree
library(rpart.plot)
library(RColorBrewer)

df <- read_csv("df2022.csv")


NIBRS1     <- df %>% dplyr::select(NIBRS) %>% distinct() %>% pull()
division1  <- df %>% dplyr::select(DIVISION) %>% distinct() %>% pull()
location1  <- df %>% dplyr::select(LOCATION) %>% distinct() %>% pull()
Month1     <- df %>% dplyr::select(MONTH) %>% distinct() %>% pull()
NPA1       <- df %>% dplyr::select(NPA) %>% distinct() %>% pull()
Type1      <- df %>% dplyr::select(PLACE_TYPE) %>% distinct() %>% pull()
Detail1    <- df %>% dplyr::select(PLACE_DETAIL) %>% distinct() %>% pull()
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
    #Get selection Plots
    selectInput("num_var_3", "Variable 1 - Plot", choices = c(not_sel)),
    selectInput("num_var_4", "Variable 2 - Plot Group By", choices = c(not_sel)),
    #Get selection Summary Table
    selectInput("num_var_1", "Variable 1 - Summary", choices = c(not_sel)),
    selectInput("num_var_2", "Variable 2 - Summary", choices = c(not_sel)),
    selectInput("fact_var", "Variable 3 - Summary", choices = c(not_sel)),
    br(),
    actionButton("run_button", "Run Analysis", icon = icon("play")),
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Instructions", icon = icon("info"),
        column(10,includeMarkdown("help.md"))
      ),
      tabPanel(
        title = "Data Selection", icon = icon("table"),
        DTOutput("tbl")
      ),
      tabPanel(
        title = "Visualization", icon = icon("chart-simple"),
        sidebarLayout(
          sidebarPanel(
            radioButtons("plotType", "Select a Plot Type",
                         choices = list("Bar Chart"= 1, "Box Plot"= 2),
            ),
          ),
          mainPanel(
            plotOutput("plot")
          )
        ), 
        
      ),
      tabPanel(
        title = "Summary", icon = icon("list-alt"),
        fluidRow(
          column(width = 4, strong(textOutput("var_1_title"))),
          column(width = 4, strong(textOutput("var_2_title"))),
          column(width = 4, strong(textOutput("fact_var_title")))
        ),
        fluidRow(
          column(width = 4, tableOutput("var1_summary_table")),
          column(width = 4, tableOutput("var2_summary_table")),
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
              strong("Fitting Models"),br(),
              ("Select the proportion, center & scale, and predictor variables for each model - GLM,                             Classification tree, and Random Forest."),
              br(),
              #GLM
              #Get proportions
              br(),strong("Generalized LM Modeling"),br(),
              selectInput(inputId = "n_prop",
                          label = "Choose Partition Proportion:",
                          choices = c(.65, .70, .75, .80),
                          selected = .75),
              #Get Preprocess
              checkboxInput("preprocessMe", 
                            "PreProcess with center & scale?", 
                            value = TRUE),
              #Get CV
              selectInput(inputId = "cross",
                          label = "Number CV:",
                          choices = c(5, 10, 15, 20),
                          selected = 5),
              #Get Predictors
              uiOutput("colPredict"),
              div(style="text-align:left","Select Predictors:"),
              textOutput("selectedTextp"),
              tags$hr(style="border-color: black;"),
              
              
              # Classification
              #Get proportions
              strong("Classification Tree Modeling"),br(),
              selectInput(inputId = "n_prop_C",
                          label = "Choose Partition Proportion:",
                          choices = c(.65, .70, .75, .80),
                          selected = .75),
              #Get Preprocess
              checkboxInput("preprocessMe_C", 
                            "PreProcess with center & scale?", 
                            value = TRUE),
              #Get CV
              selectInput(inputId = "cross_C",
                          label = "Number CV:",
                          choices = c(5, 10, 15, 20),
                          selected = 5),
              #Get Predictors
              uiOutput("colPredict_C"),
              div(style="text-align:left","Select Predictors:"),
              textOutput("selectedTextp_C"),
              tags$hr(style="border-color: black;"),
              
              # Random Forest
              #Get proportions
              strong("Random Forest Modeling"),br(),
              selectInput(inputId = "n_prop_R",
                          label = "Choose Partition Proportion:",
                          choices = c(.65, .70, .75, .80),
                          selected = 0.75),
              #Get CV
              selectInput(inputId = "cross_R",
                          label = "Number CV:",
                          choices = c(5, 10, 15, 20),
                          selected = 5),
              #Get Predictors
              uiOutput("colPredict_R"),
              div(style="text-align:left","Select Predictors:"),
              textOutput("selectedTextp_R"),
              br(),
              actionButton("run_model", "Run Models", icon = icon("play")),
              
              hr(style = "border-top: 1px solid #000000;"),
              
              #Create predictions
              strong("Prediction"),br(),
              ("Select predictor variables for the prediction.  Once complete, press Run Prediction and select the Prediction tab to view the results"),
              selectInput("pickmodel", 
                          "Choose a model:", 
                          choices = c("GLM (Generalized LM)" = "glm", 
                                      "Classification Tree" = "tree", 
                                      "Random Forest" = "rf")),
              selectInput("Division_ID", 
                          "Choose Division:", 
                          division1, multiple = FALSE, selected = "07"),
              selectInput("NPA", 
                          "Choose NPA:", 
                          NPA1, multiple = FALSE, selected = "371"),
              selectInput("Location", 
                          "Choose Location:", 
                          location1, multiple = FALSE, selected = "Indoors"),
              selectInput("Type", 
                          "Choose Type:", 
                          Type1, multiple = FALSE, selected = "Residental"),
              selectInput("Detail", 
                          "Choose Detail:", 
                          Detail1, multiple = FALSE, selected = "Apartment/Duplex"),
              selectInput("NIBRS", 
                          "Choose Crime:", 
                          NIBRS1, multiple = FALSE, selected = "All Other Offenses"),
              selectInput("Month", 
                          "Choose Summer Month:", 
                          Month1, multiple = FALSE, selected = "08"),
              actionButton("run_predict","Run Prediction", icon = icon("play"))
              
 ),
 mainPanel(
   tabsetPanel(
     tabPanel("Modeling Info", icon = icon("info"),
              column(10,includeMarkdown("info.md")),
              DTOutput("tbl2"),
     ),
     tabPanel("Modeling Fitting",icon = icon("table"),
              #plotOutput("treeplot"),
              #verbatimTextOutput("glmsummary"),
              plotOutput("rfplot"),
     ), 
     
     tabPanel("Prediction", icon = icon("list-alt"))
   )
 )
)
)




# Data Tab
data_page <- tabPanel(
  title = "Data",  icon = icon("table"),
  titlePanel("Data"), 
    sidebarLayout(
      sidebarPanel(
        title = "Inputs",
    selectInput("MonthGet", label = "Choose Summer Month(s)", Month1, multiple = TRUE, selected = Month1),
    selectInput("DivisionGet", label = "Choose Division(s)", division1, multiple = TRUE, selected = division1),
    selectInput("NIBRSGet", label = "Choose Crimes (NIBRS)", NIBRS1, multiple = TRUE, selected = NIBRS1),
    selectInput("LocationGet", label = "Choose Location(s)", location1, multiple = TRUE, selected = location1),
    selectInput("TypeGet", label = "Choose Type(s)", Type1, multiple = TRUE, selected = Type1),
    selectInput("DetailGet", label = "Choose Detail(s)", Detail1, multiple = TRUE, selected = Detail1),
    #Get Columns
    uiOutput("colControls_D"),
    div(style="text-align:left","Select Columns:"),
    textOutput("selectedTextc_D"),
    downloadButton("download1","Download entire Table  as csv"),
      ),
    mainPanel(
      tabsetPanel(
        tabPanel(DT::dataTableOutput("cmpd_dto"))
        )
    )
  ))





# Main to render pages

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

  

# Helper sites for UI portion
# https://stackoverflow.com/questions/43592163/horizontal-rule-hr-in-r-shiny-sidebar
# https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7