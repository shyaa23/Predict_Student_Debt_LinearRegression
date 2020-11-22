---
title: "Linear Regression - Predicting Debt for Students"
author: "Shreya shakya"
date: "11/07/2019"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
#### **Load packages**

```r
library(tidyverse)
library(dplyr)
library(broom)
library(modelr)
library(corrplot)
library(corrr)
library(ggplot2)
library(ggformula)
library(rje)
library(ggvis)
library(purrr)
```

### **Load Data**

In this application, we are working with Allendale student dataset which is a random sample from population of recent GVSU graduate that lived in Allendale. The goal of this project is to predict the amount of debt to be paid by students of GVSU. All the students had car and  50% of tution cost were paid by their parents.


```r
student_data <- read_csv('data/allendale-students.csv')
```

### **Exploratory Analysis**

This dataset consist of 6 explanatory variables namely, Distance, Scholarship, Parents, Car, Housing and Major. The goal here is to make prediction on the amount of debt that the student needs to pay based on these explantory variable. We see some relationship of explanatory variable with reponse as follows.


```r
#Distribution of reponse variable debt
ggplot(data=student_data, aes(x=debt))+
  geom_histogram(bins=30, fill = "white", color = "black") +
  theme_minimal() +
  labs(title = 'Distribution of Debt'
       ,y = 'Frequency'
       ,x = 'Debt'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

The distribution of response variable debt seems to be normally distributed but slighlty left skewed. We dont consider any transformation here and go ahead for looking other relationship.


```r
#Relationship between scholarship and debt
ggplot(data=student_data, aes(x=scholarship, y=debt))+
  geom_point() +
  theme_minimal() +
  labs(title = 'Relationship between scholarship and debt'
       ,y = 'Debt'
       ,x = 'Scholarship'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The scatterplot for scholarship by debt shows that more the amount of scholarship, te amount of debt need to be paid by student is less. Student who does not have any scholarship has high amount of debt. 


```r
#Outlier remove
student_data <- student_data %>%
filter(scholarship <= 20000)

#Check to see the relation again
ggplot(data=student_data, aes(x=scholarship, y=debt))+
geom_point()+
  theme_minimal() +
  labs(title = 'Relationship between scholarship and debt after removing outlier'
       ,y = 'Debt'
       ,x = 'Scholarship'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

There is one noticiable outlier here, so this point is removed.


```r
#Relationship between distance and debt
ggplot(data=student_data, aes(x=distance, y=debt))+
  geom_point()+
  theme_minimal() +
  labs(title = 'Relationship between distance and debt'
       ,y = 'Debt'
       ,x = 'Distance'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

There is a roughly positve relationship between debt and distance. There is a little higher amount of debt if there is higher distance from student's hometown to Allendale.


```r
#Relationship between parent and debt
ggplot(data=student_data, aes(x=parents, y=debt))+
  geom_point()+
  theme_minimal() +
  labs(title = 'Relationship between parents and debt'
       ,y = 'Dept'
       ,x = 'Parents'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

There doesnt seem to be any pattern between debt and parents. 


```r
#Relationship between car and debt
ggplot(data=student_data, aes(x=factor(car), y=debt))+
  geom_boxplot()+
  theme_minimal() +
  labs(title = 'Relationship between car and debt'
       ,y = 'Dept'
       ,x = 'car'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Similarly, the age of the car also does not have much relation with debt. 


```r
#Relationship between hoursing and debt
ggplot(data=student_data, aes(x=housing, y=debt))+
  geom_boxplot()+
  theme_minimal() +
  labs(title = 'Relationship between housing and debt'
       ,y = 'Dept'
       ,x = 'Scholarship'
  )
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

There is not much difference for the student living on campus or off campus with the amount of debt they need to pay.


```r
ggplot(data=student_data, aes(x=major, y=debt))+
  geom_boxplot()+
  theme_minimal() +
  labs(title = 'Relationship between Major and debt'
       ,y = 'Dept'
       ,x = 'Major'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The business major student seems to have little higher amount of debt as compared to STEM and other majors. There is wide spread of amount of debt for the STEM major student. It could vary alot.


#### **Correlation**

Looking at the correlation plot, we can see that Scholarship has moderate negative relationship with amount of debt. Similarly, if parents pay students debt, the amount of debt is decreased so it has some negative relation as well. There is some positive relation of distance with amount of debt as well.


```r
#Correlation between all variables.
data <- student_data[, sapply(student_data, is.numeric)]

#Correlation plot
M <- cor(data,use="pairwise.complete.obs")
c2 <- corrplot(M,type = "upper",order = "hclust", title="Correlation plot for Student data", method = "square", tl.cex = 0.85, cl.cex = 0.75)
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#Looking at the correlation of response with other exploratory variables
data %>% correlate(use="pairwise.complete.obs") %>% focus(debt) %>% data.frame()
```

```
##       rowname         debt
## 1    distance  0.496972804
## 2 scholarship -0.665933961
## 3     parents -0.372241049
## 4         car  0.001482082
```

```r
#Bargraph showing relation of explanatory with response
data %>% correlate() %>% focus(debt) %>%
  ggplot(aes(x = rowname, y = debt)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90,margin = margin(-0.15, unit = "cm"),vjust =1))+
  labs(title = 'Correlation of other variables with dept'
       ,y = 'Correlation with dept'
       ,x = 'Variables'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

**Dealing with categorical variables**

There are two categorical variables in this dataset. We need to recode these variables inorder to process it. If there are n number of levels for categorical variable, we always need to create n-1 variables to represent its indicator variables, zero is regarded as the reference group. For housing variable, there are two levels: on campus and off campus. Here, only one variable is required to represent its indicator variable, 1 could be treated as on campus and 0 as off campus.

Similarly, there are 3 levels for major category so we create 2 variables to represent it. The values are 01, 10 and 00, where 01 represent stem major, 10 represents business and 00 represents refernce group that is other major.


```r
#Create indicator variable for housing
student_data <- student_data %>%
  mutate(housing = as.factor(housing)) %>%
  mutate(housing = fct_recode(housing, 
                              `0` = "off campus",
                              `1` = "on campus"))%>%
  mutate(housing=as.factor(housing))

#Create indicator variable for major
student_data <- student_data %>%
  mutate(major = as.factor(major)) %>%
  mutate(stem= fct_collapse(major, 
                            `0` = c("business","other"),
                            `1` = "STEM")) %>%
  mutate(business= fct_collapse(major, 
                                `0` = c("STEM","other"),
                                `1` = "business")) %>%
  mutate(stem=as.factor(stem),business=as.factor(business))
```


### **Model Building**

In this function, the best subset variable selection is computed. The dataset consist of 8 variables so possible number of subsets are 127. These subset are computed using the powerset function of RJE library. For all these subset we find the formula for passing to lm function inorder to fit a linear model. Toal of 126 linear model are created and we compute  intercept, slope, rsquare, ajusted rsquare, p-value for these model and store as a tibble. The model number, explanatory variable used for that particular model and adjusted R square value is printed. Along with that, the adjusted r square for all the model are plotted in the graph as shown below. The highest adjusted R square among all the model is 0.797.


**Best subset selection**


```r
#Removing major from dataset
student_data <- student_data %>%
  select(-major)

#Arranging the data in form of explanatory variable and reponse at end
student_data <- student_data[c("distance", "scholarship", "parents", "car", "housing", "stem", "business","debt")]
```

**Function**


```r
options(warn=-1)
task3 <- function(dataset){
  #Get the names of explanatory variables
  x = rev(names(dataset))[-1]
  #Get the name of last variable which is response in dataset
  y = rev(names(dataset))[1]
  #Creates every possible subsets for explanatory variables
  p_set=powerSet(x)
  #select from 2nd row/ Changed here
  p_set = p_set[-(0:1)]
  
  f <- function(x){
    #Get the information of which variables are used
    variables_used = paste(y,",",paste(x,collapse=","))
    
    #Create a formula to pass through the linear model
    formula = formula(paste(y,"~", paste(x, collapse=" + ")))
    
    #The total number of explanatory variables
    num_explanatory = length(x)
    
    #Create a linear function for each subset passed as formula
    linear.full = lm(formula, data = dataset)
  
    term = tidy(linear.full)$term[-1]
    #Get the intercept from the summary of linear model
    
    intercept =tidy(linear.full)$estimate[1]
    
    #Get the values of slope from summary of linear model
    slope = paste(term,":",tidy(linear.full)$estimate[-1],collapse=",")
    
    #Get the pvalues from summary of linear model
    p_value = glance(linear.full)$p.value
    
    #Get the R sqaured from summary of linear model
    rsquare = glance(linear.full)$r.squared
    
    #Get the adjusted R square value from summary of linear model
    adjrsquare = glance(linear.full)$adj.r.squared
    
    #Rounding the adjusted and R square values to 3 digits
    adjrsquare=round(adjrsquare,3)
    rsquare=round(rsquare,3)
    
    #Just for the information to keep on the table which formula had been used in model creation for different subset
    model_formula = paste(paste(y,"~", paste(x, collapse=" + ")))
    
    #Create a dataframe that contains all the required information of every possible subset and its model
    df <- data.frame(variables_used,  
                     model_formula,intercept,slope,
                     num_explanatory,p_value,rsquare,adjrsquare) 
    return(df)
  }  
  
  #map the subset to above created function which returns a dataframe
  df = map_df(p_set,f)

  df = df %>% mutate(model_number=row_number())

  #The output tibble consist of only 1) the model number, 2) the explanatory variables in the    model, 3) the value of adjustedr-square for that model
  tibble_out <- df  %>%
    select(-c(model_formula,intercept,slope,
              num_explanatory,p_value,rsquare)) %>%
    as_tibble()
  
  #Plotting adjusted R square for all the models and labeling the highest one
  adj_r_plot <-  df%>% 
    ggplot(aes(x=num_explanatory,y=adjrsquare))+
    geom_point() +
    labs(title="Adjusted R square for all the models",x="Number of variables",y="Adjusted R2")+ geom_label(data = . %>%  filter(adjrsquare == max(adjrsquare)), aes(label = model_number), hjust = 0.1)
  
  print(adj_r_plot)
  
  #Output Tibble is saved as csv file and not printed
  write_csv(tibble_out,path='data/tibble_output.csv')

  return(tibble_out)
}
```


```r
task3 <- task3(student_data)
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

#### **Choosing Best Model**

Of all the model,model 119 has the highest adjusted R-square of 0.797 with 5 explanatory variables: business, stem, parents, scholarship and distance. 
From the graph obtained from above function, we can see that adjusted R2 with 3 variable is also around maximum adjusted R square. Looking into the table, we can see that with 3 explanatory variable: parent, scholarship and distance, about 79% of the variability could be explained by the model and p-value is also strongly significant (5.163395e-67	).
Therefore, since there is only slight improvement in adjusted R2 wi th 5 explanatory variable than 3, the best model would be to choose the simple model with less variable that could explain much variability that is model number 111 with only 3 explanatory variable. Therefore, the best model would have the following equation:

debt =38830.301 - 23240.799 * parent - 1.855*  scholarship + 40.568 * distance


```r
#Check the highest adjusted r square model
task3 %>%
  slice(which.max(adjrsquare))
```

```
## # A tibble: 1 x 3
##   variables_used                                         adjrsquare model_number
##   <chr>                                                       <dbl>        <int>
## 1 debt , business,stem,housing,parents,scholarship,dist~      0.797          119
```

```r
#Linear Model for best model
best_model = lm(debt~parents+scholarship+distance, data = student_data)
tidy(best_model)
```

```
## # A tibble: 4 x 5
##   term         estimate std.error statistic   p.value
##   <chr>           <dbl>     <dbl>     <dbl>     <dbl>
## 1 (Intercept)  38830.    782.          49.6 1.41e-112
## 2 parents     -23241.   1969.         -11.8 1.32e- 24
## 3 scholarship     -1.86    0.0914     -20.3 5.65e- 50
## 4 distance        40.6     3.06        13.3 4.93e- 29
```

```r
glance(best_model)
```

```
## # A tibble: 1 x 11
##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <int>  <dbl> <dbl> <dbl>
## 1     0.796         0.793 3840.      253. 5.16e-67     4 -1923. 3855. 3872.
## # ... with 2 more variables: deviance <dbl>, df.residual <int>
```


**Checking the residuals from the best model**


```r
#Assumption of linearity
plot(best_model, 1, title('Assumption of linearity'))
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

Looking at the graph, the assumption of linearity has been met. The points on the plot above appears to be randomly scattered aroung zero.


```r
#Check the assumption of constant variance
gf_point(rstandard(best_model)~predict(best_model)) %>%
  gf_hline(yintercept=0)+
    theme_minimal() +
  labs(title = 'Assumption for constant variance'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

Looking at residuals by predictions graph, the assumption of constant variance has been fairly met. There is no any noticable patterns.


```r
plot(best_model, 5,title('Checking Outliers and Influential points'))
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

There seems to be no outliers and influential points.

**Interaction Model**


```r
#Linear model which contains interaction
interaction_model = lm(debt~housing*scholarship+scholarship*parents, data = student_data)
#summary(interaction_model)
tidy(interaction_model)
```

```
## # A tibble: 6 x 5
##   term                    estimate std.error statistic  p.value
##   <chr>                      <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)           42602.      1444.      29.5    1.87e-73
## 2 housing1               2236.      1233.       1.81   7.14e- 2
## 3 scholarship              -1.87       0.291   -6.42   1.03e- 9
## 4 parents              -24886.      4380.      -5.68   4.85e- 8
## 5 housing1:scholarship     -0.155      0.259   -0.597  5.51e- 1
## 6 scholarship:parents      -0.0206     0.846   -0.0243 9.81e- 1
```

```r
glance(interaction_model)
```

```
## # A tibble: 1 x 11
##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <int>  <dbl> <dbl> <dbl>
## 1     0.622         0.612 5254.      63.4 6.48e-39     6 -1984. 3982. 4005.
## # ... with 2 more variables: deviance <dbl>, df.residual <int>
```
We see that there is no interaction between both scholarship + housing and parents + scholarship, the pvalues for both are not statistically significant at the 0.05 level. Therefore, we conclude for this problem, these interaction term does not contributes in a meaningful way to the predictive ability of the regression equation. Following are the interpretation of slope of these two interaction terms:


```r
#Interaction plot
ggplot(student_data, aes(x=scholarship, y=debt, color=housing)) + geom_point()+
geom_smooth(method="lm")+
  theme_minimal() +
  labs(title = 'Interaction plot for scholarship and housing'
       ,y = 'Debt'
       ,x = 'Scholarship'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

On average for students living on campus, every additional amount of scholarship is associated with a 1.545e-01 decrease in the amount of debt to be paid by student. From the plot we can see that as the scholarship increases, the debt decreases for both on campus and off campus students. With one unit increase in scholarship, students living on campus have debt slightly more than students living off campus.


```r
#Interaction plot
ggplot(student_data, aes(x=scholarship, y=debt, color=parents)) + geom_point()+
    theme_minimal() +
  labs(title = 'Interaction plot for scholarship and parents'
       ,y = 'Debt'
       ,x = 'parents'
  ) 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

On average for students whose tution is paid by their parents, every additional amount of scholarship is associated with a 2.056e-02 decrease in in the amount of debt to be paid by student. Also looking at the plot, we can see that as the amount of tution fees paid by parents are more, the debt of the student is less comparatively.

*Checking Assumptions*


```r
#Assumption of linearity
plot(interaction_model, 1, title('Assumption of linearity'))
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

Looking at the graph, the assumption of linearity has been roughly met.


```r
gf_point(rstandard(interaction_model)~predict(interaction_model)) %>%
  gf_hline(yintercept=0)+
  theme_minimal() +
  labs(title = 'Assumption for Constant Variance') 
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

Looking at residuals by predictions graph, the assumption of constant variance has been roughly met. The points on the plot above appears to be randomly scattered aroung zero. There is no any noticable patterns.


```r
plot(best_model, 5,title('Checking Outliers and Influential points'))
```

![](Linear_regression_debt_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

There seems to be no outliers and influential points.
