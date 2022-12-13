What does this app do?

The purpose of this app is to analyze the Charlotte Mecklenburg Police Department (CMPD) crime data

The data is located at the City of Charlotte's Open data portal `https://data.charlottenc.gov/` . The data used for this app comes from the CMPD incidents located here `https://data.charlottenc.gov/datasets/cmpd-incidents-1/explore`. As of November 30, 2022, there are 542,153 observations with 25 variables (or features)

It contains the following features:

`Year	Incident Report ID	Location	City	State	Zip	X-Coordinate	Y-Coordinate	Latitude	Longitude	Division ID	CMPD Patrol Division	Neighborhood Profile Area ID (NPA)	Date Reported	Date Incident Began	Date Incident End	Address Description	Location Type Description	Place Type Description	Place Detail Description	Clearance Status	Clearance Detail Status	Clearance Date	Highest NIBRS Code	Highest NIBRS Description`

This data has many attributes (or features) that represent the similar things.  For this project, only 8 attributes are used: Year, Division, NPA, Location, Place_Type, Place_Detail, NIBRS, and Status.  


Remaining tabs (pages) of the app:

**Data Exploration**

The tab allows the user to select features (columns) and filter the rows of the data.  The results are used to create numerical and graphical summaries.  

**Modeling**

The tab creates three supervised models.  The models are a multiple linear regression or generalized linear regression model, regression or classification tree, and a random forest model.

**Model Info**
    
Explain the three modeling approaches, the benefits of each, and the drawbacks of each.
        
**Model Fitting**
    
Split your data into a training and test set. Give the user the ability to choose the proportion of data used in each.  The user should have functionality for choosing model settings for each model. For all models, they should be able to select the variables used as predictors (you can fix the response if you want). Cross validation should be used for selecting models on the training set where appropriate. When the user is ready they should be able to press a button and fit all three models on the training data.

Fit statistics (such as RMSE) on the training data should be reported for each model along with appropriate summaries about the model (for instance, summary() run on your lm() or glm() fit, a plot showing the variable importance from the random forest model,etc.). The models should be compared on the test set and appropriate fit statistics reported.
        
**Prediction**
    
You should give the user a way to use one of the models for prediction. That is, they should be able to select the values of the predictors and obtain a prediction for the response.

**Data**

The tab allows the user to scroll through the data set and subset it by rows and columns.  The user also has the ability to save the subsetted data as a .csv file.
