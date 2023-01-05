


**Generalized Linear Regression Model**

This regression model is an extension of linear general model for situations where the outcomes are not normally distributed nor do they always have continuous variables. It generalizes linear regression by allowing the linear model to be related to the response variable via a link function. With this model, it allows for both continuous and categorical predictors.

The GLM consists of three elements:

1. A particular distribution for modeling $Y$,
2. A linear predictor $\eta =X\beta$, and
3. A link function such that $E(Y\mid X)=\mu =g^{-1}(\eta )$.

For the Bernoulli and binomial distributions, the parameter is a single probability, indicating the likelihood of occurrence of a single event. The Bernoulli still satisfies the basic condition of the generalized linear model in that, even though a single outcome will always be either 0 or 1, the expected value will nonetheless be a real-valued probability, i.e. the probability of occurrence of a "yes" (or 1) outcome. Similarly, in a binomial distribution, the expected value is Np, i.e. the expected proportion of "yes" outcomes will be the probability to be predicted.

https://en.wikipedia.org/wiki/Generalized_linear_model

This model is suitable where the outcome variable is binary. This is the case for this project.  We are using predictor variables to predict if STATUS (of the crime) is Open or Closed.  This model can be used where outcomes are for categorical variables, count data, and continuous variables skewed.



**Classification Trees**
Classification is a decision tree method which offers two types classification and regression.  We use classification when the Y variable is a factor (categorical) and regression when Y variable is numeric.  The goal os this method is to classify or predict outcomes based on predictor variables. 

This tree based model partitions a data set of observations into smaller and smaller homogeneous subsets. At each step (or question), a subset is split into those smaller subsets based on a single variable (or feature).  After the tree is constructed, it can be used to classify new observations.


The pro for using classification trees:

1. Ease of use, easy to explain and visualize.
2. Does not require data normalization

The con for using classification trees:

1. There lack of predictive accuracy. By combining this method with other algorithms the accuracy can be improved.  However,  in general the predictive accuracy is limited.
2. Classification trees can have high variance.
3. Calculations can become complex which reuires more processing time.



**Random Forest**
Random Forest is a machine learning algorithm for classification and regression models.  Similar to the previous model, classification model refers to factor/categorical dependent variable and regression model refers to numeric or continuous dependent variable.

Random Forest is a variation of bagging that is used for classification of regression trees.  This bagging technique helps to improve the algorithm's performance.  This method is made up of a collection of trees, and each tree in the group is comprised of a data sample drawn from a training set with replacement. For regression task, the individual trees are averaged and for classification task by the largest number of votes.

The trees are created as follows:
1. Each tree is trained on roughly 2/3rd of the total training data.  Cases are drawn at random with replacement from the original data. This sample will be the training set for growing the tree.
2. Some predictor variables are selected at random out of all the predictor variables and the best split is used to split the node.
3. The remaining 1/3rd of data is used to calculate the misclassificiation rate.
4. Each tree "votes" for a class.  The algorithm choose the classification hacing hte most votes.

The pro for using Random forest:

1. Reduced risk of overfitting. 
2. Flexibility (it can handle both regression and classification).
3. Easy to determine importance attributes (or features)

The con for using Random Forest:

1. It can be slow as the algorithm grows trees which can affect it's optimization.
2. Requires more resources.



<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width">
  <title>MathJax example</title>
  <script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
  <script id="MathJax-script" async
          src="https://cdn.jsdelivr.net/npm/mathjax@3.0.1/es5/tex-mml-chtml.js">
  </script>
</head>
<body>