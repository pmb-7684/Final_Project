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

df <- read_csv("df.csv")

df2 <- df
df2$STATUS <- as.factor(df2$STATUS)
df2$STATUS <- factor(df2$STATUS, levels = c("Open", "Closed"), labels = c(0,1))
df2$YEAR<-as.factor(df2$YEAR)
df2$LOCATION<-as.factor(df2$LOCATION)
df2$LOCATION <- factor(df2$LOCATION, levels = c("Outdoors", "Indoors", "Parking Lot", "Other","Parking Deck"), labels = c(0,1,2,3,4))
df2$DIVISION <- as.factor(df2$DIVISION)
df2$PLACE_TYPE <- as.factor(df2$PLACE_TYPE)
df2$PLACE_DETAIL <- as.factor(df2$PLACE_DETAIL)
df2$NIBRS <- as.factor(df2$NIBRS)
df2$NPA <- as.factor(df2$NPA)


year1      <- df %>% select(YEAR) %>% distinct() %>% pull()
division1  <- df %>% select(DIVISION) %>% distinct() %>% pull()

not_sel <- "Not Selected"



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

create_num_var_table <- function(thedata1, num_var){
  if(num_var != not_sel){
    col <- thedata1[,get(num_var)]

    statistic <- c("mean", "median", "25th percentile", "95th percentile",
                   "Maximum", "Minimum")
    value <- c(round(mean(col),2), round(median(col),2),
               round(max(col),2), round(min(col),2))
    data.table(statistic, value)
  }
}



create_fact_var_table <- function(thedata1, fact_var){
  if(fact_var != not_sel){
    freq_tbl <- thedata1[,.N, by = get(fact_var)]
    freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
    freq_tbl
  }
}



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
      data <- data[data$LOCATION == input$loc,]
    }
    if (input$div != "All") {
      data <- data[data$DIVISION == input$div,]
    }
    data
}))
  

#############################################################################################################
# Part of the DATA section - this works
# https://community.rstudio.com/t/download-dataset-filtered-in-shiny-input/75770/2
# this filter rows by year and division and allows user to download a csv for the new data
  
thedata <- reactive({
    df %>% 
      filter(df$YEAR== input$YearGet & df$DIVISION== input$DivisionGet )
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
        filter(df$YEAR== input$YearGet1 & df$DIVISION== input$DivisionGet1)
    }else{
      df %>% 
        filter(df$YEAR== input$YearGet1 & df$DIVISION== input$DivisionGet1) %>% 
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
  
# 1-d summary tables

output$num_var_1_title <- renderText(paste("Num Var 1:",num_var_1()))

num_var_1_summary_table <- eventReactive(input$run_button,{
  create_num_var_table(thedata1, num_var_1())
})

output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)

output$num_var_2_title <- renderText(paste("Num Var 2:",num_var_2()))

num_var_2_summary_table <- eventReactive(input$run_button,{
  create_num_var_table(thedata1, num_var_2())
})

output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)

output$fact_var_title <- renderText(paste("Factor Var:",fact_var()))

fact_var_summary_table <- eventReactive(input$run_button,{
  create_fact_var_table(thedata1, fact_var())
})

output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)


# display debugging messages in R (if local) 
# and in the console log (if running in shiny)
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}





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



# fit a random forest model
fitrf <- eventReactive(input$run_model, {
  Train <- Train()
  
  #resp  <- list(c("STATUS"))
  #pick <- unlist(append(input$colsP, resp))
  #newdata <- Training[, pick]
  
  
  #fitrf <- train(STATUS ~ ., data = Train, method = "rf", 
   #              trControl = trainControl(method = "cv", number = 5))
  
  fitrf <- randomForest(STATUS~., data = Train,
                        importance = TRUE,
                        proximity = TRUE)
})
  
output$rfplot <- renderPlot({
            varimport <- varImp(fitrf())
             plot(varimport)
})
  
})