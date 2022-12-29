


**Generalized Linear Regression Model**

This regression model is an extension of linear general model for situations where the outcomes are not normally distributed nor do they always have continuous variables. It generalizes linear regression by allowing the linear model to be related to the response variable via a link function. With this model, it allows for both continuous and categorical predictors.

The GLM consists of three elements:

1. A particular distribution for modeling $Y$,
2. A linear predictor $\eta =X\beta$, and
3. A link function such that $E(Y\mid X)=\mu =g^{-1}(\eta )$.

When $a \ne 0$, there are two solutions to $(ax^2 + bx + c = 0)$ and they are 
$$ x = {-b \pm \sqrt{b^2-4ac} \over 2a} $$

This model is suitable where the outcome variable is binary. This is the case for this project.  We are using predictor variables to predict if STATUS (of the crime) is Open or Closed.  This model can be used where outcomes are for categorical variables, count data, and continuous variables skewed.



**Classification Trees**

This tree based model partitions a data set of observations into smaller and smaller homogeneous subsets. At each step (or question), a subset is split into those smaller subsets based on a single variable (or feature).  After the tree is constructed, it can be used to classify new observations.


The pro for using classification trees are there ease of use, easy to explain and visualize.

The con for using classification trees is there lack of predictive accuracy. By combining this method if other algorithms the accuracy can be improved.  However,  in general the predictive accuracy is limited. The other con is classification trees can have high variance.



**Random Forest**

Random Forest is a variation of bagging that is used for classification of regression trees.  This bagging technique helps to improve the algorithm's performance.  this method is made up of a collection of trees, and each tree in the group is comprised of a data sample drawn from a training set with replacement. For regression task, the individual trees are averaged and for classification task by the largest number of votes.

The pro for using Random forest are reduced risk of overfitting, flexibility (it can handle both regression and classification), and easy to determine importance attributes (or features)

The con for Random Forest is it can be slow as the algorithm grows trees which can affect it's optimization and it requires more resources.



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