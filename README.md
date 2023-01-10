

# Final_Project
Final Project - ST 558

• Brief description of the app and its purpose.
The purpose of this app is to analyze a portion of the Charlotte Mecklenburg Police Department (CMPD) crime data.  The data is located at the City of Charlotte's [Open data portal](https://data.charlottenc.gov/).  The data used comes from the CMPD incidents located [here](https://data.charlottenc.gov/datasets/cmpd-incidents-1/explore).  As of November 30, 2022, there are 542,153 observations with 25 variables (or features). It covers a period from January 2017 to the Present.

 The app contains the following sections
– An About page. 
It provides information about the app.
  
– A Data Exploration page. 
This tab allows the user to select features (columns) and filter the rows of the data.  The results are used to create categorical charts and summaries.  This data set does not contain any numerical variables. This tab is divided into three sub tabs named Instructions, Data Selection, Visualization and Summary.

– A Modeling page. 
This tab creates three supervised models - generalized linear regression model, classification tree, and a random forest model.  This tab is divided into three sub tabs named Model Info, Model Fitting, and Prediction.

– A Data page.
The tab allows the user to scroll through the data set and subset it by rows and columns.  The user also has the ability to save the subsetted data as a .csv file, pdf, or as a copy.


• A list of packages needed to run the app.
``` library(shiny)                  # build rich and productive interactive web apps in R
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
```


• Packages needed for installation.
install.packages(c("shiny", "shinydashboard", "shinythemes", "data.table", "tidyverse", "DT", "shinyWidgets", "caret", "randomForest", "rpart", "rattle", "rpart.plot", "RColorBrewer"))




• The shiny::runGitHub() code to run app in RStudio.

` shiny::runGitHub('shiny_CMPD', 'pmb-7684', ref = "main") `
