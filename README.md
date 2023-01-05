

# Final_Project
Final Project - ST 558

• Brief description of the app and its purpose.

    – An About page. 
    – A Data Exploration page. 
    – A Modeling page. 
       ∗ Modeling Info tab:
       ∗ Model Fitting tab:
       ∗ Prediction tab:
    – A Data page.


• A list of packages needed to run the app.
``` library(shiny)
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
```


• A line of code that would install all the packages used.
install.packages(c("shiny", "shinydashboard", "shinythemes", "data.table", "tidyverse", "DT", "shinyWidgets", "caret", "randomForest", "rpart"))




• The shiny::runGitHub() code that we can copy and paste into RStudio to run your app.

` shiny::runGitHub('shiny_CMPD', 'pmb-7684', ref = "main") `
