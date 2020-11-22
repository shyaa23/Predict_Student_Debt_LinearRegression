**Introduction**

This repository presents a statistical model to predict the amount of debt to be paid by students of GVSU who lived in Allendale. The dataset consisted of a random sample from the population of recent GVSU graduates that lived in Allendale during their time at GVSU.
The dataset consisted of about 8 variables: Scholarship (Amount of financial support given by GVSU), Distance (distance from home to GVSU), Parents (Percent of tuition paid by their parents), Car (Age of car), Housing (Type of housing (on campus, off campus), Major (STEM, business, other) and Debt (Student debt accumulated by end of the degree).

**Exploratory analysis**

Relationship between debt and all the other predictor variable were shown through visualization. Some of the findings from the exploratory analysis were:

•	The distribution of response variable debt seems to be normally distributed.

•	There was strong negative association between debt and the amount of scholarship. Student who does not have any scholarship had high amount of debt. While exploring the relationship, few outliers were also observed which were removed.

•	There was a weak positive relationship between debt and distance. There was a little higher amount of debt if the distance from student's hometown to GVSU was higher.

•	The business major student seems to have little higher amount of debt as compared to STEM and other majors. 

•	From the correlation plot, Scholarship and parents had negative relationship with amount of debt. If parents paid students debt, the amount of debt is decreased.


**Model Building**

For the modeling, multiple linear regression was used. Best subset was used for selection of important variables inorder to fit the model. A function was build which fit all possible models based on the independent variables that we specified and returned all the possible subset of models. The dataset consisted of 8 variables so possible number of subsets were 127. 
The intercept, slope, R-square, Adjusted R-square, p-value were computed for these models and stored. Of all the model, model 119 had the highest adjusted R-square of 0.797 with 5 explanatory variables and 111 had adjusted R-square of 0.795 with 3 explanatory variables.
 After best model was selected, the assumption of linearity and constant variance were also checked.

**Link to Report for more explaination** : [Rpubs Link](https://rpubs.com/shakyashreya23/LinearRegression)
