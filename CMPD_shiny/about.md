**What does this app do?**

The purpose of this app is to analyze the Charlotte Mecklenburg Police Department (CMPD) crime data

The data is located at the City of Charlotte's Open data portal `https://data.charlottenc.gov/` . The data used for this app comes from the CMPD incidents located here `https://data.charlottenc.gov/datasets/cmpd-incidents-1/explore`. As of November 30, 2022, there are 542,153 observations with 25 variables (or features)

It contains the following features:

`Year	Incident Report ID	Location	City	State	Zip	X-Coordinate	Y-Coordinate	Latitude	Longitude	Division ID	CMPD Patrol Division	Neighborhood Profile Area ID (NPA)	Date Reported	Date Incident Began	Date Incident End	Address Description	Location Type Description	Place Type Description	Place Detail Description	Clearance Status	Clearance Detail Status	Clearance Date	Highest NIBRS Code	Highest NIBRS Description`

This data has many attributes (or features) that represent the similar things.  For this project, only 10 attributes are used: Year, Division, Division_ID, NPA, Location, Place_Type, Place_Detail, NIBRS, Month, and Status.  

The tasks used to complete the data cleaning can be viewed in the .rmd file named shiny_CMPD. After data cleaning, only approximately 13,599 observation are used for the project.


The remaining tabs (pages) of the app:

**Data Exploration**

This tab allows the user to select features (columns) and filter the rows of the data.  The results are used to create categorical charts and summaries.  This data set does not contain any numerical variables.

**Modeling**

This tab creates three supervised models - multiple linear regression or generalized linear regression model, regression or classification tree, and a random forest model.  This tab is divided into three sub tabs named Model Info, Model Fitting, and Prediction.

**Model Info**
    
This tab explains the three modeling approaches including the benefits and drawbacks.
        
**Model Fitting**
    
This tab preps the data by splitting data into a training and test set. You have the ability to choose the proportion of data used in each and functionality for choosing model settings for each model including number of cross validations when appropriate. For all models, you are able to select the variables used as predictors.  When the user is ready they should be able to press a button and fit all three models on the training data.

The output includes RMSE on the training data for each model along with appropriate summaries about the model, a plot showing the variable importance from the random forest model. Finally, the models are compared on the test set and appropriate fit statistics reported.
        
**Prediction**
    
This tab allows the user to select one of the models for prediction. You have the ability to select the values of the predictors and obtain a prediction for the response.

**Data**

The tab allows the user to scroll through the data set and subset it by rows and columns.  The user also has the ability to save the subsetted data as a .csv file.


**Created with R Shiny**
**2022 December**