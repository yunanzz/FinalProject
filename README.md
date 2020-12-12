# Project Name: Customer Revenue Prediction
Team members: Sean Gao, Yanran Li, Yunan Zhao


## Goal of the project:
This is the GitHub Repository for BST 260 Final Project. This project aims to apply data science and machine learning techniques to predict customer revenue. The dataset retrieves from Kaggle website. Motivation of the project comes from three parts:

1) 80/20 Rule: In e-commerce industry, small percentage of customers contribute to most of the revenue
2) Given the large dataset we have, machine learning may play an important role in prediction
3) Eventually we are hoping to transfrom our analysis into actionable operational changes so that we would be able to have a better utilization of marketing resources.


## Instructions: 
1. Look at Rmd and html files `260finalproj.Rmd`, `260finalproj.html`
2. Run our Shiny app `app1211.R`


## Data files:
1. ``


Our Website can be found [here] (https://sites.google.com/d/18Yhi3pULsGkuH-tIqzOHSZXXxUJc7B4m/p/1dKtKz8UdfQr8BFjuh3MTOhediHrTAXDk/edit).

Our screencase can be found [here] .


This project could be divided up into four main parts, i.e., Data Wrangling, R shiny, regression, and machine learning. 

### Data Wrangling
In data wrangling, main parts could be summarized as 1) transforming non-standard format data (JSON) into standard data, 2) deal with missing values.

### R Shiny
Our Shiny App can be divided into 2 parts:
1) Seeing 3 different measurements' (Pageviews/Revenue/Hits) distribution around the world on selected day.
2) The relationship between revenue v.s. users' visit number in different countries.


### Regression
Here we make statistical model about customers’ revenue. The main question is that whether there exist relationship between the number of customer accessing hits and the amount of revenue generated by the customer. 

### Machine Learning
Here we play with two types of machine learning, one being bagging based Random Forest, another one being boosting based XGBoost. Bagging and Boosting have different mechanisms, and given large percentage of categorical variables in our dataset, we are hoping to have a better comparison of model performances by applying that into the dataset.

