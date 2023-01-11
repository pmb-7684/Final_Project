#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# 
#
#    

library(shiny)                  # build rich and productive interactive web apps in R
library(shinydashboard)         # create shiny dashboards
library(shinythemes)            # themes for shiny
library(data.table)             # fast and easy data manipulation for large datasets
library(tidyverse)              # collection of R packages designed for data science
library(DT)                     # R interface to the JavaScript library DataTables
library(shinyWidgets)           # custom widgets and components to enhance shiny applications
library(caret)                  # Streamline functions training and plotting classification & regression models
library(randomForest)           # Classification & regression based on decision trees
library(rpart)                  # Building decision tree model
library(rattle)                 # visualize the tree
library(rpart.plot)             # Recursive Partitioning and Regression Trees
library(RColorBrewer)           # Provides color schemes for maps (and other graphics)


df <- read_csv("df2022.csv")    #1/0 for STATUS               once complete model; one retain one dataset
df1 <- read_csv("df2022_.csv")  #open/closed for STATUS


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


################################################################################ DATA section
 

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

################################################################################ DATA EXPLORATION
#Columns and rows selection
  
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


  
# get selection for plots in exploration tab
#https://github.com/MatePocs/rshiny_apps/blob/main/data_analyser/app.R

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
  plot <- eventReactive(input$run_button,{
    if(input$plotType == 1){
      ggplot(thedata1(),
             aes_string(x = input$num_var_3)) +  geom_bar()
    }else{
      ggplot(thedata1(),
             aes_string(x = input$num_var_3, y = input$num_var_4)) +  geom_boxplot()
    }
  })
  
 
# output created plot   
  output$plot <- renderPlot(plot())
  
# Summary tables for exploration tab
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


# Data for ALL models plus Clean up
data_model_Group <- eventReactive(input$run_model, {
   df7 <- df1 %>% dplyr::select(-NPA, -DIVISION_ID)   

    #change year from double to char
    df7 $YEAR <-  as.character(df7$YEAR)

    #make each variable a factor; it should have the levels already
    df7 <- df7 %>% map_df(function(.x) as.factor(.x))

data_model_Group <- df7
})


# All data check - verify type of each variable; output to modeling tab
output$allcheck <- renderPrint({
  glimpse(data_model_Group())
})

# Confirm no missing data; output to modeling tab
output$missing <- renderPrint({
  map_dbl(data_model_Group(), function(.x) {sum(is.na(.x))})
})



################################################################################ RANDOM FOREST

#create train and test for Random Forest (user can make input changes)
trainIndex_R <- eventReactive(input$run_model, {
  
    trainIndex_R <- data_model_Group()$STATUS %>% createDataPartition(p = input$n_prop_R, list = FALSE) 
})


train_R <- eventReactive(input$run_model,{

    train_R <- data_model_Group()[trainIndex_R(), ]
})


test_R  <- eventReactive(input$run_model,{
 
    test_R  <- data_model_Group()[-trainIndex_R(), ]
  
})



#get predictors for RF
output$colPredict_R <- renderUI({
  
  pickerInput(inputId="colsP_R", "Choose Predictors", choices = c("YEAR", "DIVISION", "LOCATION", "PLACE_TYPE","PLACE_DETAIL","NIBRS", "MONTH"), multiple = TRUE)
})


txtp_R <- reactive({ input$colsP_R })
    output$selectedTextp_R <- renderText({paste0(txtp_R() ,sep=", ") 
})

    
    
#data based on predictors selected   
thedata4 <- eventReactive(input$run_model, {
    response_R <- list(c("STATUS"))
    selected_R <- unlist(append(txtp_R(), response_R))
    
thedata4 <- data_model_Group() %>% dplyr::select({paste0(selected_R)})  
})


# check output results RF dataset
output$tbl2 <- renderDataTable(head(thedata4(), 7))



# fit random forest model
fitrf <- eventReactive(input$run_model, {
  withProgress(message = 'Modeling in progress. Please wait ...', {
  Train <- train_R()
  
  response_R <- list(c("STATUS"))
  selected_R <- unlist(append(txtp_R(), response_R))
  
  newdata_R <- Train[, selected_R]

  #caret method (generated error)
  #fitrf <- train(newdata_R$STATUS ~ ., data = newdata_R, method = "rf", 
  #               trControl = trainControl(method = "cv", number = input$cross_R),
  #               tuneGrid = data.frame(mtry= 1:ncol(newdata_R)) 
  #               )
  
  #non caret method
  fitrf <- randomForest(newdata_R$STATUS ~ ., data = newdata_R, 
                        ntree = 300,  importance = TRUE)
  
  })
})

# random forest summary
output$rfsummary <- renderPrint({
  print(fitrf())
})

# random forest plot
output$rfplot <- renderPlot({  
   varImpPlot(fitrf())
})

# Var Importance
#output$rfmatrix <- renderPlot({
#    importance(fitrf())
#})



################################################################################ GLM - GENERALIZED LM
# Predictor selection and creating model

#create train and test for GLM (user can make input changes)
trainIndex <- eventReactive(input$run_model, {
  
    trainIndex <- data_model_Group()$STATUS %>% createDataPartition(p = input$n_prop, list = FALSE) 
})


train <- eventReactive(input$run_model,{
  
    train <- data_model_Group()[trainIndex(), ]
})


test  <- eventReactive(input$run_model,{
  
    test  <- data_model_Group()[-trainIndex(), ]
  
})


#gets predictors for GLM
output$colPredict <- renderUI({
  
  pickerInput(inputId="colsP", "Choose Predictors", choices = c("YEAR", "DIVISION", "LOCATION", "PLACE_TYPE","PLACE_DETAIL", "NIBRS","MONTH"), multiple = TRUE)
})

txtp <- reactive({ input$colsP })
output$selectedTextp <- renderText({paste0(txtp() ,sep=", ") })


#creates glm dataset to output for verification
thedata2 <- eventReactive(input$run_model, {
    response <- list(c("STATUS"))
    selected <- unlist(append(txtp(), response))
  
thedata2 <- data_model_Group() %>% dplyr::select({paste0(selected)}) 
})


# output to check dataset for glm
output$tbl3 <- renderDataTable(head(thedata2(), 7))


# fit glm with selected variables and option to centering/scaling
fitglm <- eventReactive(input$run_model, {
  withProgress(message = 'Modeling in progress. Please wait ...', {
    
  Train <- train()
  
  #creates selected glm dataset for modeling
  response <- list(c("STATUS"))
  selected <- unlist(append(txtp(), response))
  
  newdata <- Train[, selected]

  #caret method
  #if (input$preprocessMe == 1) {
  #  fitglm <- train(newdata$STATUS ~ ., data = newdata, method = "glm", family = "binomial", 
                    #preProcess = c("center", "scale"),
  #                  trControl = trainControl(method = "cv", number = input$cross))
  #} else {
  #  fitglm <- train(newdata$STATUS ~ ., data = newdata, method = "glm", family = "binomial", 
  #                  trControl = trainControl(method = "cv", number = input$cross))
  #}
  
  #non caret
  fitglm <-  glm(newdata$STATUS ~ ., data = newdata, family = "binomial")
  })

})

# glm summary
output$glmsummary <- renderPrint({
      summary(fitglm())
})


# ############################################################################## CLASSIFICATION TREE

#create train and test for Classification tree (user can make input changes)

trainIndex_C <- eventReactive(input$run_model, {
  
      trainIndex_C <- data_model_Group()$STATUS %>% createDataPartition(p = input$n_prop, list = FALSE) 
})


train_C <- eventReactive(input$run_model,{
  
      train_C <- data_model_Group()[trainIndex_C(), ]
})


test_C  <- eventReactive(input$run_model,{
  
      test_C  <- data_model_Group()[-trainIndex_C(), ]
  
})


#creates classification dataset to output for verification
output$colPredict_C <- renderUI({
  
    pickerInput(inputId="colsP_C", "Choose Predictors", choices = c("YEAR", "DIVISION", "LOCATION", "PLACE_TYPE","PLACE_DETAIL", "NIBRS", "MONTH"), multiple = TRUE)
})

txtp_C <- reactive({ input$colsP_C })
    output$selectedTextp_C <- renderText({paste0(txtp_C() ,sep=", ") })

thedata3 <- eventReactive(input$run_model, {
    response_C <- list(c("STATUS"))
    selected_C <- unlist(append(txtp_C(), response_C))
    
    thedata3 <- data_model_Group() %>% dplyr::select({paste0(selected_C)}) 
})


#just output to check dataset for classification
output$tbl4 <- renderDataTable(head(thedata3(), 7))




# fit classification tree
treefit <- eventReactive(input$run_model, {
  withProgress(message = 'Modeling in progress. Please wait ...', {
    Train <- train_C()
    
    response_C <- list(c("STATUS"))
    selected_C <- unlist(append(txtp_C(), response_C))
    
    newdata_C <- Train[, selected_C]
 
# caret method   
#    if (input$preprocessMe_C == 1) {
#      treefit <- train(STATUS ~ ., data = newdata_C, method = "rpart", 
#                       #preProcess = c("center", "scale"),
#                       trControl = trainControl(method = "cv", number = input$cross_C))
#    } else {
#      treefit <- train(STATUS ~ ., data = newdata_C, method = "rpart", 
#                       trControl = trainControl(method = "cv", number = input$cross_C))
      #treefit <- rpart(STATUS ~ ., data = newdata_C, method = "class", minsplit=2, minbucket=1)
#    }

    
    #non caret method
    treefit <- rpart(newdata_C$STATUS ~ ., data = newdata_C, method = "class")
    })
    
})


# tree summary
output$treesummary <- renderPrint({
  summary(treefit())
})


# classification tree plot
output$treeplot <- renderPlot({

  fancyRpartPlot(treefit())
})



#############################################################################################################
# table for accuracy comparison
output$accuracy <- renderTable({
  fitglm <- fitglm()
  treefit <- treefit()
  fitrf <- fitrf()
  
  Aglm  <- fitglm$results %>% dplyr::select(Accuracy)
  Atree <- treefit$results %>% filter(cp == treefit$bestTune$cp) %>% dplyr::select(Accuracy)
  Arf   <- fitrf$results %>% filter(mtry == fitrf$bestTune$mtry) %>% dplyr::select(Accuracy)
  
  accuracy           <- cbind(Aglm, Atree, Arf)
  colnames(accuracy) <- list("GLM", "Classification Tree", "Random Forest") 
  
  accuracy
})




#############################################################################################################
# PREDICTION TAB
#

output$predResults <- eventReactive(input$run_predict, {
#if (input$run_predict == 'glm') {
    output$predResults <-renderText({"Prediction - Underconstruction for GLM, Classification Trees, and Random Forest" })
#}
#  else if(input$run_predict == 'tree') {
#    output$predResults <-renderText({"Prediction - Underconstruction for Trees" })
#}
#  else if(input$run_predict == 'tree') {
#    output$predResults <-renderText({"Prediction - Underconstruction for Random Forest" })
#}
})


#------- NOTES ----- complete once resolve modeling error

# Get user predictions
# varI <-eventReactive(input$run_predict,input$Division_ID)
# varL <-eventReactive(input$run_predict,input$Location)
# varT <-eventReactive(input$run_predict,input$Type)
# varD <-eventReactive(input$run_predict,input$Detail)
# varN <-eventReactive(input$run_predict,input$NIBRS)
# varM <-eventReactive(input$run_predict,input$Month)


# create data from predictions
#selected_p <- unlist(append(varI,varL, varT, varD, varN, varM ))
#newdata_p <- Test[, selected_p]

#RF prediction
#rfPredict     <-predict(fitrf, data=newdata_p) %>% as_tibble()
#Random_Forest <- postResample(rfPredict(), obs = test_R$STATUS)

#LM prediction
#treePredict   <-predict(fittree, data=newdata_p %>% as_tibble()
#Glm           <-postResample(treePredict(), obs = test()$STATUS)



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
# https://www.r-bloggers.com/2017/01/random-forest-classification-of-mushrooms/
# https://www.r-bloggers.com/2017/11/predict-customer-churn-logistic-regression-decision-tree-and-random-forest/
# https://fderyckel.github.io/machinelearningwithr/case-study-mushrooms-classification.html
# https://stackoverflow.com/questions/34879305/need-to-use-same-input-for-multiple-outputs-in-shiny
# https://stackoverflow.com/questions/64768969/r-shiny-creating-factor-variables-and-defining-levels
# https://community.rstudio.com/t/download-dataset-filtered-in-shiny-input/75770/2
# https://stackoverflow.com/questions/42261496/selectinput-have-multiple-true-and-filter-based-off-that
# https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7
# https://data.library.virginia.edu/getting-started-with-shiny/
# https://stackoverflow.com/questions/40623749/what-is-object-of-type-closure-is-not-subsettable-error-in-shiny
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# https://towardsdatascience.com/how-to-make-a-professional-shiny-app-and-not-get-intimidated-with-r-991e636dd111
