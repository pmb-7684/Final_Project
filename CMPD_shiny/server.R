#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
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





############################################################################################################
# Define server logic required 
shinyServer(function(input, output, session) {


#############################################################################################################
# Part of the DATA section - reworked
# https://community.rstudio.com/t/download-dataset-filtered-in-shiny-input/75770/2
# https://stackoverflow.com/questions/42261496/selectinput-have-multiple-true-and-filter-based-off-that
# this filter rows and allows user to download a csv for the new data

# column selection
output$colControls_D <- renderUI({
    
    pickerInput(inputId="cols_D", "Choose Columns", choices= df %>% colnames(),
                multiple = TRUE)
})
  
txtc_D <- reactive({ input$cols_D })
output$selectedTextc_D <- renderText({paste0(txtc_D() ,sep=", ") })

# combine column with row selection - creates thedata()
thedata <- reactive({
  
if(is.null(input$cols_D)){
  thedata <- filter(df,df$MONTH %in% input$MonthGet)
  thedata <- filter(thedata,thedata$DIVISION %in% input$DivisionGet)
  thedata <- filter(thedata,thedata$LOCATION %in% input$LocationGet)
  thedata <- filter(thedata,thedata$NIBRS %in% input$NIBRSGet)
}else{
  thedata <- filter(df,df$MONTH %in% input$MonthGet)
  thedata <- filter(thedata,thedata$DIVISION %in% input$DivisionGet)
  thedata <- filter(thedata,thedata$LOCATION %in% input$LocationGet)
  thedata <- filter(thedata,thedata$NIBRS %in% input$NIBRSGet)
  thedata %>% dplyr::select({paste0(txtc_D())})
}
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

  choices <- c(not_sel,names(thedata1()))
  updateSelectInput(inputId = "num_var_1", choices = choices)
  updateSelectInput(inputId = "num_var_2", choices = choices)
  updateSelectInput(inputId = "fact_var", choices = choices)
  updateSelectInput(inputId = "num_var_3", choices = choices)
  updateSelectInput(inputId = "num_var_4", choices = choices)
  
})

var_1 <- eventReactive(input$run_button,input$num_var_1)
var_2 <- eventReactive(input$run_button,input$num_var_2)
fact_var <- eventReactive(input$run_button,input$fact_var)

  
# Create Plots for exploration
# https://data.library.virginia.edu/getting-started-with-shiny/
  
  
  plot <- eventReactive(input$run_button,{
    if(input$plotType == 1){
      ggplot(thedata1(),
             aes_string(x = input$num_var_3)) +  geom_bar()
    }else{
      ggplot(thedata1(),
             aes_string(x = input$num_var_3, y = input$num_var_4)) +  geom_boxplot()
    }
  })
  
  
  output$plot <- renderPlot(plot())
  
# Summary tables: this works for 3 variables... Need to clean up to address 1 or 2 variables
# https://stackoverflow.com/questions/40623749/what-is-object-of-type-closure-is-not-subsettable-error-in-shiny
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# https://towardsdatascience.com/how-to-make-a-professional-shiny-app-and-not-get-intimidated-with-r-991e636dd111

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
#GLM
output$colPredict <- renderUI({
    
pickerInput(inputId="colsP", "Choose Predictors", choices = c("YEAR","DIVISION","NPA", "LOCATION", "PLACE_TYPE","PLACE_DETAIL","DESCRIPTION"), multiple = TRUE)
  })
  
  txtp <- reactive({ input$colsP })
  output$selectedTextp <- renderText({paste0(txtp() ,sep=", ") })
  
  thedata2 <- eventReactive(input$run_model, {
    response <- list(c("STATUS"))
    selected <- unlist(append(txtp(), response))
    
    thedata2 <- df %>% dplyr::select({paste0(selected)}) 
  })
  

output$tbl2 <- renderDataTable(head(thedata2(), 7))
  

# Classification 
  output$colPredict_C <- renderUI({
    
    pickerInput(inputId="colsP_C", "Choose Predictors", choices = c("YEAR","DIVISION","NPA", "LOCATION", "PLACE_TYPE","PLACE_DETAIL","DESCRIPTION"), multiple = TRUE)
  })
  
  txtp_C <- reactive({ input$colsP_C })
  output$selectedTextp_C <- renderText({paste0(txtp_C() ,sep=", ") })
  

  output$tbl2 <- renderDataTable(head(thedata2(), 7))
  
# Random Forest 
  output$colPredict_R <- renderUI({
    
    pickerInput(inputId="colsP_R", "Choose Predictors", choices = c("YEAR","DIVISION","NPA", "LOCATION", "PLACE_TYPE","PLACE_DETAIL","DESCRIPTION"), multiple = TRUE)
  })
  
  txtp_R <- reactive({ input$colsP_R })
  output$selectedTextp_R <- renderText({paste0(txtp_R() ,sep=", ") })
  
  
  output$tbl2 <- renderDataTable(head(thedata2(), 7))
  
  ##########################################################################################################
  # Begin Processing Training and Testing Data - Split Data
  # https://www.statology.org/train-test-split-r/     (other methods to split)
  # okay, I see what the problem is ...thedata2() does not contain the response variable STATUS
  # reason for error - Warning: Error in createDataPartition: y must have at least 2 data points
  # fixed above and verified by modeling data output
  
  trainIndex <- eventReactive(input$run_model, {
    data2 <- thedata2()
    trainIndex <- createDataPartition(data2$STATUS, p = input$n_prop, list = FALSE)
  })
  
  train <- eventReactive(input$run_model,{
    data2 <- thedata2()
    train1 <- data2[trainIndex(),]
  })
  
  test  <- eventReactive(input$run_model,{
    data2 <- thedata2()
    test  <- data2[!trainIndex(),]
  })
  
  
  # fit glm with selected variables and option to centering/scaling
  # https://stackoverflow.com/questions/64768969/r-shiny-creating-factor-variables-and-defining-levels
  fitglm <- eventReactive(input$run_model, {
    Train <- train()
    
    newdata <- thedata2()
    
    if (input$preprocessMe == 1) {
      fitglm <- train(STATUS ~ ., data = newdata, method = "glm", family = "binomial", 
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = input$cross))
    } else {
      fitglm <- train(STATUS ~ ., data = newdata, method = "glm", family = "binomial", 
                      trControl = trainControl(method = "cv", number = input$cross))
    }
  })
  
  # glm summary
  output$glmsummary <- renderPrint({
    summary(fitglm())
  })
  
  
  # fit classification tree
  treefit <- eventReactive(input$run_model, {
    Train <- train()
    
    newdata <- thedata2()
    
    if (input$preprocessMe == 1) {
      treefit <- train(STATUS ~ ., data = newdata, method = "rpart", 
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "cv", number = input$cross))
    } else {
      treefit <- train(STATUS ~ ., data = newdata, method = "rpart", 
                       trControl = trainControl(method = "cv", number = input$cross))
    }
  })
  
  
  # classification tree summary
  output$treeplot <- renderPlot({
    treefit <- treefit()
    
    plot(treefit$finalModel, main = "Classification Tree")
    text(treefit$finalModel, pretty = 0, cex = 0.6)
  })
  
  
  
  # fit random forest model
  fitrf <- eventReactive(input$run_model, {
    Train <- train()
    
    newdata <- thedata2()
    
    
    fitrf <- train(STATUS ~ ., data = newdata, method = "rf", 
                   trControl = trainControl(method = "cv", number = input$cross),
                   tuneGrid = expand.grid(mtry = c(1:round(sqrt(ncol(newdata)-1)))))
    
  })
  
  
  # random forest summary
  output$rfplot <- renderPlot({
    varimport <- varImp(fitrf())
    plot(varimport)
  })
  




  
})


# Other helpful sites
# https://github.blog/2022-05-19-math-support-in-markdown/          Helpful with .md files with mathjax


