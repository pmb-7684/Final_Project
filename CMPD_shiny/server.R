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
library(rpart)              #building decision tree model
library(rattle)             #visualize the tree
library(rpart.plot)
library(RColorBrewer)



df <- read_csv("df2022.csv")   #1/0

df1 <- read_csv("df2022_.csv") #open/closed


NIBRS1     <- df %>% dplyr::select(NIBRS) %>% distinct() %>% pull()
division1  <- df %>% dplyr::select(DIVISION) %>% distinct() %>% pull()
location1  <- df %>% dplyr::select(LOCATION) %>% distinct() %>% pull()
Month1     <- df %>% dplyr::select(MONTH) %>% distinct() %>% pull()
NPA1       <- df %>% dplyr::select(NPA) %>% distinct() %>% pull()
Type1       <- df %>% dplyr::select(PLACE_TYPE) %>% distinct() %>% pull()
Detail1       <- df %>% dplyr::select(PLACE_DETAIL) %>% distinct() %>% pull()
not_sel    <- "Not Selected"




# Define server logic required 
shinyServer(function(input, output, session) {


#############################################################################################################
# Tab DATA section 
# https://community.rstudio.com/t/download-dataset-filtered-in-shiny-input/75770/2
# https://stackoverflow.com/questions/42261496/selectinput-have-multiple-true-and-filter-based-off-that
# this filter rows and allows user to download a csv for the new data

# column selection for DATA tab
output$colControls_D <- renderUI({
    
    pickerInput(inputId="cols_D", "Choose Columns", choices= df %>% colnames(),
                multiple = TRUE)
})
  
txtc_D <- reactive({ input$cols_D })
output$selectedTextc_D <- renderText({paste0(txtc_D() ,sep=", ") })

# combine column with row selection for DATA tab - creates thedata()
thedata <- reactive({
  
if(is.null(input$cols_D)){
  thedata <- filter(df,df$MONTH %in% input$MonthGet)
  thedata <- filter(thedata,thedata$DIVISION %in% input$DivisionGet)
  thedata <- filter(thedata,thedata$LOCATION %in% input$LocationGet)
  thedata <- filter(thedata,thedata$NIBRS %in% input$NIBRSGet)
  thedata <- filter(thedata,thedata$PLACE_TYPE %in% input$TypeGet)
  thedata <- filter(thedata,thedata$PLACE_DETAIL %in% input$DetailGet)
}else{
  thedata <- filter(df,df$MONTH %in% input$MonthGet)
  thedata <- filter(thedata,thedata$DIVISION %in% input$DivisionGet)
  thedata <- filter(thedata,thedata$LOCATION %in% input$LocationGet)
  thedata <- filter(thedata,thedata$NIBRS %in% input$NIBRSGet)
  thedata <- filter(thedata,thedata$PLACE_TYPE %in% input$TypeGet)
  thedata <- filter(thedata,thedata$PLACE_DETAIL %in% input$DetailGet)
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


#Columns and rows for Data Exploration Tab- creates thedata1()
  
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


  
#Continue Data exploration tab 
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

  
# Create Plots for exploration tab
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
  
# Summary tables for exploration tab
# https://stackoverflow.com/questions/40623749/what-is-object-of-type-closure-is-not-subsettable-error-in-shiny
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# https://towardsdatascience.com/how-to-make-a-professional-shiny-app-and-not-get-intimidated-with-r-991e636dd111

output$var_1_title <- renderText(paste("Var 1:",var_1(),"Var 2:",var_2(),"Var 3:",fact_var()))


choice <- reactive ({
  choice <- c(input$num_var_1, input$num_var_2, input$fact_var)
})


output$var1_summary_table <- renderTable({
  list <- choice()
  
  if(var_1() != not_sel & var_2() != not_sel & fact_var() != not_sel){                         #123
        var1_summary_table <- table(thedata1()[[list[1]]],thedata1()[[list[2]]], thedata1()[[list[3]]])
  }
  else if(var_1() != not_sel & var_2() != not_sel & fact_var() == not_sel){                   #12
        var1_summary_table <- table(thedata1()[[list[1]]],thedata1()[[list[2]]])
  }
  else if(var_1() != not_sel & var_2() == not_sel & fact_var() != not_sel){                   #13
    var1_summary_table <- table(thedata1()[[list[1]]],thedata1()[[list[3]]])
  }
  else if(var_1() != not_sel & var_2() == not_sel & fact_var() == not_sel){                   #1
        var1_summary_table <- table(thedata1()[[list[1]]])  
  }
  else if(var_1() == not_sel & var_2() != not_sel & fact_var() == not_sel){                   #2
    var1_summary_table <- table(thedata1()[[list[2]]])  
  }
  else if(var_1() == not_sel & var_2() == not_sel & fact_var() != not_sel){                   #3
    var1_summary_table <- table(thedata1()[[list[3]]])  
  }
  else if(var_1() == not_sel & var_2() != not_sel & fact_var() != not_sel){                   #23
        var1_summary_table <- table(thedata1()[[list[2]]], thedata1()[[list[3]]])
  }
  
})



###############################################################################################################
# Data just for classification - able to run in .rmd using both Open/Close and 0/1
#


data_model_C <- eventReactive(input$run_model1, {
  
  df2 <- df %>% dplyr::select(-NPA, -DIVISION_ID)
  
  #Additional factoring
  df2$STATUS = as.factor(df2$STATUS)
  
  df2$DIVISION = factor(df2$DIVISION, levels = c( "North Tryon","Metro", "Eastway",
                                                  "Central",
                                                  "North",
                                                  "University City",
                                                  "Westover",
                                                  "Steele Creek",
                                                  "Providence",
                                                  "South",
                                                  "Freedom",
                                                  "Hickory Grove",
                                                  "Independence",
                                                  "Airport" ,
                                                  "Huntersville"))
  
  df2$LOCATION = factor(df2$LOCATION, levels = c("Indoor", "Parking Lot", "Outdoors", "Parking Deck", "Other"))
  
  df2$PLACE_TYPE = factor(df$PLACE_TYPE, levels = c( "Residential", "Open Area", "Retail", "Commercial Place"))
  
  df2$PLACE_DETAIL = factor(df2$PLACE_DETAIL, levels = c("Apartment/Duplex Private Res",
                                                         "Street/Highway",
                                                         "Private Residence",
                                                         "Department Store",
                                                         "Other - Commercial Place",
                                                         "Restaurant/Diner/Coffee Shop",
                                                         "Hotel/Motel",
                                                         "Grocery Store/Supermarket",
                                                         "Convenience Store",
                                                         "Other - Retail",
                                                         "Gas Station",
                                                         "Other - Open Area"))
  
  df2$NIBRS = factor(df2$NIBRS, levels = c( "All Other Offenses",
                                            "Simple Assault",
                                            "Other Unlisted Non-Criminal",
                                            "Damage/Vandalism Of Property",
                                            "All Other Thefts",
                                            "Intimidation",
                                            "Burglary/B&E",
                                            "Shoplifting",
                                            "Missing Person",
                                            "Motor Vehicle Theft",
                                            "Drug/Narcotic Violations",
                                            "Aggravated Assault"))
  
  df2$YEAR = factor(df2$YEAR, levels = c("2017", "2018", "2019", "2020", "2021", "2022"))
  
  df2$MONTH = factor(df2$MONTH, levels = c("07", "08", "09"))
  
  data_model_C <- df2
})



#############################################################################################################
# Predictor selection and creating model
# Random Forest           creates thedata4()
#   

#Slight cleanup to get predictions
data_model_RS <- eventReactive(input$run_model, {
  df4 <- df1 %>% dplyr::select(-DIVISION_ID, -NPA)
  data_model_RS <- df4 
})


#get predictors for RF
output$colPredict_R <- renderUI({
  
  pickerInput(inputId="colsP_R", "Choose Predictors", choices = c("YEAR", "DIVISION", "LOCATION", "PLACE_TYPE","PLACE_DETAIL","NIBRS", "MONTH"), multiple = TRUE)
})


txtp_R <- reactive({ input$colsP_R })
    output$selectedTextp_R <- renderText({paste0(txtp_R() ,sep=", ") 
})

#data based on predeictors selected   
thedata4 <- eventReactive(input$run_model, {
    response_R <- list(c("STATUS"))
    selected_R <- unlist(append(txtp_R(), response_R))
    
    thedata4 <- data_model_RS() %>% dplyr::select({paste0(selected_R)}) 
})


#just output to check dataset for RF
output$tbl2 <- renderDataTable(head(thedata4(), 7))


# complete basic factoring for categorical variables
data_model_R <- eventReactive(input$run_model, {
  df4 <- df1 %>% dplyr::select(-DIVISION_ID, -NPA)
  #df4$YEAR <- as.factor(df4$YEAR)
  df4$MONTH <- as.factor(df4$MONTH)
  df4$DIVISION <- as.factor(df4$DIVISION)
  df4$LOCATION <- as.factor(df4$LOCATION)
  df4$PLACE_TYPE <- as.factor(df4$PLACE_TYPE)
  df4$PLACE_DETAIL <- as.factor(df4$PLACE_DETAIL)
  df4$NIBRS <- as.factor(df4$NIBRS)
  
  #change Open/Close to 0/1
  df4$STATUS <- as.factor(ifelse(df4$STATUS == "Closed",1,0))
  df4$STATUS <- as.factor(df4$STATUS)
  
  data_model_R <- df4 
})


#create train and test for RF
trainIndex_R <- eventReactive(input$run_model, {
  data2_R <- data_model_R()
  trainIndex_R <- createDataPartition(data2_R$STATUS, p = input$n_prop_R, list = FALSE)
})

train_R <- eventReactive(input$run_model,{
  data2_R <- data_model_R()
  train_R <- data2_R[trainIndex_R(),]
})

test_R  <- eventReactive(input$run_model,{
  data2_R <- data_model_R()
  test_R  <- data2_R[-trainIndex_R(),]
  
})

#applies One Hot Encoding by encoding the categorical independent variables for TRAINING

trainX <- eventReactive(input$run_model, {
  
      trainfactors <- model.matrix(train_R()$STATUS ~ train_R()$MONTH+ train_R()$DIVISION+ train_R()$LOCATION +
                                   train_R()$PLACE_TYPE+ train_R()$PLACE_DETAIL+ train_R()$NIBRS)[,-1]
      
  trainX <- as.matrix(data.frame(trainfactors, train_R()$YEAR)) 
})


#applies One Hot Encoding by encoding the categorical independent variables for TEST
#https://www.youtube.com/watch?v=qFeeldwPiNE
testX <- eventReactive(input$run_model, {
  
  trainfactors <- model.matrix(train_R()$STATUS ~ train_R()$MONTH+ train_R()$DIVISION+ train_R()$LOCATION +
                                 train_R()$PLACE_TYPE+ train_R()$PLACE_DETAIL+ train_R()$NIBRS)[,-1]
  
  testX <- as.matrix(data.frame(testfactors, train_R()$YEAR)) 
})




# fit random forest model
fitrf <- eventReactive(input$run_model, {
  #Train <- train_R()
  
  #response_R <- list(c("STATUS"))
  #selected_R <- unlist(append(txtp_R(), response_R))
  
  #newdata_R <- Train[, selected_R]
   newdata_R <- trainX()
  
  fitrf <- train(train_R()$STATUS ~ ., data = newdata_R, method = "rf", 
                 trControl = trainControl(method = "cv", number = input$cross_R),
                 tuneGrid = expand.grid(mtry = c(1:round(sqrt(ncol(newdata_R)-1)))))
  
})



# random forest summary
output$rfplot <- renderPlot({
  varimport <- varImp(fitrf())
  plot(varimport)
})





})


# Other helpful sites
# https://github.blog/2022-05-19-math-support-in-markdown/          Helpful with .md files with mathjax
# https://www.r-bloggers.com/2020/02/a-guide-to-encoding-categorical-features-using-r/
# https://www.r-bloggers.com/2022/01/handling-categorical-data-in-r-part-1/
# https://www.r-bloggers.com/2022/01/handling-categorical-data-in-r-part-2/  part 1-4 good info, nothing on ML
# https://www.r-bloggers.com/2022/01/handling-categorical-data-in-r-part-3/
# https://www.r-bloggers.com/2022/01/handling-categorical-data-in-r-part-4/
# https://www.geeksforgeeks.org/regression-with-categorical-variables-in-r-programming/
# https://stylizeddata.com/how-to-recode-factor-and-character-variables-in-r/
# https://stackoverflow.com/questions/71132387/nested-tabsets-in-shiny
# https://www.statology.org/one-hot-encoding-in-r/
# https://www.youtube.com/watch?v=rxbgmluhp4o

