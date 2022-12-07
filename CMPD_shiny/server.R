#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
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


df <- read_csv("finalDF2.csv")
means_out <- read_csv("finalDF2.csv")
df1 <- read_csv("finalDF2.csv")



year1      <- df %>% select(YEAR) %>% distinct() %>% pull()
division1  <- df %>% select(CMPD_PATROL_DIVISION) %>% distinct() %>% pull()

not_sel <- "Not Selected"







############################################################################################################
# Define server logic required 
shinyServer(function(input, output) {

  # Filter data based on selections - Data
  output$table <- DT::renderDataTable(DT::datatable({
    data <- df
    if (input$yr != "All") {
      data <- data[data$YEAR == input$yr,]
    }
    if (input$loc != "All") {
      data <- data[data$LOCATION_TYPE_DESCRIPTION == input$loc,]
    }
    if (input$div != "All") {
      data <- data[data$CMPD_PATROL_DIVISION == input$div,]
    }
    data
  }))
  

#############################################################################################################
# Part of the DATA section - this works
# https://community.rstudio.com/t/download-dataset-filtered-in-shiny-input/75770/2
# this filter rows by year and division and allows user to download a csv for the new data
  
  thedata <- reactive({
    df %>% 
      filter(df$YEAR== input$YearGet & df$CMPD_PATROL_DIVISION== input$DivisionGet )
  })
  
  output$iris_dto <- renderDataTable({
    thedata()  %>% 
      datatable(extensions = 'Buttons',
                options = list(
                  #Each letter is a dif element of a datatable view, this makes 
                  #buttons the last thing that's shown.
                  dom = 'lfrtipB',
                  buttons = c("copy", "csv", "pdf")),
                filter = list(
                  position = 'top'),
                rownames = FALSE)
  })
  
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste("iris_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(thedata(), file)
    }
  )  


  ###############################################################################################################
  #COLUMNS and ROWS for Exploration TAB  
  
  output$colControls <- renderUI({
    
    pickerInput(inputId="cols", "Choose Columns", choices= df %>% colnames(),
                multiple = TRUE)
  })
  
  txtc <- reactive({ input$cols })
  output$selectedTextc <- renderText({paste0(txtc() ,sep=", ") })
  
  
  thedata1 <- reactive({
    if(is.null(input$cols)){
      df %>% 
        filter(df$YEAR== input$YearGet1 & df$CMPD_PATROL_DIVISION== input$DivisionGet1)
    }else{
      df %>% 
        filter(df$YEAR== input$YearGet1 & df$CMPD_PATROL_DIVISION== input$DivisionGet1) %>% 
        dplyr::select({paste0(txtc())})
    }
    
  })
  
  output$tbl <- renderDataTable(head(thedata1(), 7))
  
  
  #############################################################################################################   
  #Continue exploration plot and summary
  #https://github.com/MatePocs/rshiny_apps/blob/main/data_analyser/app.R
  #https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7
  
  observeEvent(thedata1(),{
    choices <- c(not_sel,names(thedata1()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
  })
  
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  fact_var <- eventReactive(input$run_button,input$fact_var)
  
  
  plot <- eventReactive(input$run_button,{
    draw_plot_1(thedata1(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$plot <- renderPlot(plot())
  

  

  
})
