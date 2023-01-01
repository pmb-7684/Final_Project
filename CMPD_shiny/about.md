**What does this app do?**

The purpose of this app is to analyze a portion of the Charlotte Mecklenburg Police Department (CMPD) crime data

The data is located at the City of Charlotte's Open data portal `https://data.charlottenc.gov/` . The data used for this app comes from the CMPD incidents located here. `https://data.charlottenc.gov/datasets/cmpd-incidents-1/explore`. As of November 30, 2022, there are 542,153 observations with 25 variables (or features). The data covers a period from January, 2017 to the Present.

It contains the following features:

`Year,	Incident Report ID,	Location,	City,	State,	Zip,	X-Coordinate,	Y-Coordinate,	Latitude,	Longitude,	Division ID,	CMPD Patrol Division,	Neighborhood Profile Area ID (NPA),	Date Reported,	Date Incident Began,	Date Incident End,	Address, Description	Location Type, Description	Place Type, Description	Place Detail, Description	Clearance Status,	Clearance Detail Status,	Clearance Date,	Highest NIBRS Code, and	Highest NIBRS Description`.

This data has many attributes (or features) that represent the similar things.  For this project, only 10 attributes are used: `Year, Division, NPA, Location, Place_Type, Place_Detail, NIBRS, and Status`.  

The tasks used to complete the data cleaning can be viewed in the .rmd file named shiny_CMPD. This file is available on GitHub repo page. After data cleaning and subsetting, 13,599 observations are used for the project.  We filtered the data by year 2022, observations in summer months (07,08, and 09), and removed observations where total number of crimes within a category were less than 2000.  


The remaining tabs (pages) of the app:

**Data Exploration**

This tab allows the user to select features (columns) and filter the rows of the data.  The results are used to create categorical charts and summaries.  This data set does not contain any numerical variables. This tab is divided into three sub tabs named Instructions, Data Selection, Visualization and Summary which are discussed below.

***Instruction***

General instructions on how to use this section of the app.

***Data Selection***

User can view the data set based on selections from the left side panel.

***Visualization***

User can view either bar or box chart based on selections.

***Summary***

User can view summary table based on selections.


**Modeling**

This tab creates three supervised models - generalized linear regression model, classification tree, and a random forest model.  This tab is divided into three sub tabs named Model Info, Model Fitting, and Prediction which are discussed below.

***Model Info***
    
This tab explains the three modeling approaches including the benefits and drawbacks.
        
***Model Fitting***
    
This tab preps the data by splitting data into training and testing set. You have the ability to choose the proportion of data used and the functionality for choosing model settings for each model including number of cross validations when appropriate. For all models, you are able to select the variables used as predictors.  When the user is ready, press button to fit all three models on the training data.

The output includes RMSE on the training data for each model along with appropriate summaries, and a plot showing the variable importance from the random forest model. Finally, the models are compared on the test set and appropriate fit statistics reported.
        
***Prediction***
    
This tab allows the user to select one of the models for prediction. You have the ability to select the values of the predictors and obtain a prediction for the response.

**Data**

The tab allows the user to scroll through the data set and subset it by rows and columns.  The user also has the ability to save the subsetted data as a .csv file, pdf, or as a copy.



**Created with R Shiny** 

**2022 December**