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
library(shinythemes)
library(data.table)
library(ggplot2)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(caret)
library(randomForest)
library(rpart)

df <- read_csv("df2022.csv")

NIBRS1     <- df %>% select(NIBRS) %>% distinct() %>% pull()
division1  <- df %>% select(DIVISION) %>% distinct() %>% pull()
location1  <- df %>% select(LOCATION) %>% distinct() %>% pull()
Month1     <- df %>% select(MONTH) %>% distinct() %>% pull()
not_sel    <- "Not Selected"



# plot
draw_plot_1 <- function(thedata1, num_var_1, num_var_2, fact_var){
  if(fact_var!=not_sel){
    thedata1[,(fact_var):= as.factor(thedata1[,get(fact_var)])]
  }
  if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = thedata1,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
      geom_point()
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = thedata1,
           aes_string(x = num_var_1, y = num_var_2)) +
      geom_point()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = thedata1,
           aes_string(x = fact_var, y = num_var_1)) +
      geom_violin()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = thedata1,
           aes_string(x = fact_var, y = num_var_2)) +
      geom_violin()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel){
    ggplot(data = thedata1,
           aes_string(x = num_var_1)) +
      geom_histogram()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = thedata1,
           aes_string(x = num_var_2)) +
      geom_histogram()
  }
  else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = thedata1,
           aes_string(x = fact_var)) +
      geom_bar()
  }
}


# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
create_freq_table <- function(thedata1, var_1, var_2, fact_var){

  if(var_1 != not_sel & var_2 != not_sel & fact_var != not_sel){
    list <- c(var_1, var_2,fact_var)
    freq_tbl <- table(thedata1[[list[1]]],thedata1[[list[2]]], thedata1[[list[3]]])
  }
  if(fact_var!=not_sel){
    freq_tbl <- table(get(thedata1$fact_var))
  }
}


############################################################################################################
# Define server logic required 
shinyServer(function(input, output, session) {


#############################################################################################################
# Part of the DATA section - reworked
# https://community.rstudio.com/t/download-dataset-filtered-in-shiny-input/75770/2
# https://stackoverflow.com/questions/42261496/selectinput-have-multiple-true-and-filter-based-off-that
# this filter rows and allows user to download a csv for the new data
  
thedata <- reactive({
  thedata <- filter(df,df$MONTH %in% input$MonthGet)
  thedata <- filter(thedata,thedata$DIVISION %in% input$DivisionGet)
  thedata <- filter(thedata,thedata$LOCATION %in% input$LocationGet)
  thedata <- filter(thedata,thedata$NIBRS %in% input$NIBRSGet)
})
  

output$cmpd_dto <- renderDataTable({
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
      paste("cmpd_", Sys.Date(), ".csv", sep="")
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
      thedata1 <- filter(df,df$MONTH %in% input$MonthGet1)
      thedata1 <- filter(thedata1,thedata1$DIVISION %in% input$DivisionGet1)
      thedata1 <- filter(thedata1,thedata1$LOCATION %in% input$LocationGet1)
      thedata1 <- filter(thedata1,thedata1$NIBRS %in% input$NIBRSGet1)
    }else{
      thedata1 <- filter(df,df$MONTH %in% input$MonthGet1)
      thedata1 <- filter(thedata1,thedata1$DIVISION %in% input$DivisionGet1)
      thedata1 <- filter(thedata1,thedata1$LOCATION %in% input$LocationGet1)
      thedata1 <- filter(thedata1,thedata1$NIBRS %in% input$NIBRSGet1)
      thedata1 %>% dplyr::select({paste0(txtc())})
    }
    
})
  
output$tbl <- renderDataTable(thedata1())


#############################################################################################################   
#Continue exploration plot and summary
#https://github.com/MatePocs/rshiny_apps/blob/main/data_analyser/app.R
#https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7
  
observeEvent(thedata1(),{
    #thedata_EDIT <- thedata1() %>% dplyr::select(-YEAR)
    choices <- c(not_sel,names(thedata1()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
    
})
  
  var_1 <- eventReactive(input$run_button,input$num_var_1)
  var_2 <- eventReactive(input$run_button,input$num_var_2)
  fact_var <- eventReactive(input$run_button,input$fact_var)
  
  
plot <- eventReactive(input$run_button,{
    draw_plot_1(thedata1(), var_1(), var_2(), fact_var())
})
  
output$plot <- renderPlot(plot())
  
# Summary tables: this works for 3 variables... Need to clean up to address 1 or 2 variables
# https://stackoverflow.com/questions/40623749/what-is-object-of-type-closure-is-not-subsettable-error-in-shiny
output$var_1_title <- renderText(paste("Num Var 1:",var_1()))


choice <- reactive ({
   choice <- c(input$num_var_1, input$num_var_2, input$fact_var)
})

output$var1_summary_table <- renderTable({
  list <- choice()
  var1_summary_table <- table(thedata1()[[list[1]]],thedata1()[[list[2]]], thedata1()[[list[3]]])
  })




###############################################################################################################
# predictors for MODELING TAB  
  
output$colPredict <- renderUI({
    
pickerInput(inputId="colsP", "Choose Predictors", choices = c("YEAR","DIVISION","NPA", "LOCATION", "PLACE_TYPE","PLACE_DETAIL","DESCRIPTION"), multiple = TRUE)
  })
  
  txtp <- reactive({ input$colsP })
  output$selectedTextp <- renderText({paste0(txtp() ,sep=", ") })
  

  thedata2 <- eventReactive(input$run_model, {
    debug_msg("predictor submitted")

        thedata2 <- df2 %>% dplyr::select({paste0(txtp())}) 

  })
  
  output$tbl2 <- renderDataTable(head(thedata2(), 7))
  

  
##########################################################################################################
# Begin Processing Training and Testing Data
## Split Data

trainIndex <- eventReactive(input$run_model, {
  debug_msg("index submitted")
            data2 <- df2
            trainIndex <- createDataPartition(data2$STATUS, p = input$n_prop , list = FALSE)
            })
  
Train <- eventReactive(input$run_model,{
             data2 <- df2  
             Train <- data2[trainIndex(), ]
            })
      
Test  <- eventReactive(input$run_model,{
             data2 <- df2
             Test  <- data2[-trainIndex(), ]
            })




  
})