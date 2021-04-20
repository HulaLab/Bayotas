###Homework 1####
#a normal distribution with 1000 draws, a mean of 0 and a standard deviation of 2. 
##Then, use the hist() function to plot the data.
data = data.frame(nd = rnorm(1000, mean = 0, sd = 2))
hist(data$nd) #*UPDATE IT FINALLY WORKS
#A normal distribution with 100 draws, a mean of 5 and a standard deviation of 2. 
##Plot the output with hist.
data2 = data.frame(nd = rnorm(100, mean = 5, sd = 2))
hist(data2$nd) #FINALLYYY
#A normal distribution with 500 draws, a mean of -10 and a standard deviation of 5.
data3 = data.frame(nd = 500, mean = -10, sd = 5)


##Diamonds data##
library(ggplot2) 
diamonds = diamonds
length(diamonds) #The length of the dataframe is 10 
mean(diamonds$carat) #The mean of the carat variable is 0.7979397
sd(diamonds$carat) #The sd of the carat variable is 0.4740112
range(diamonds$price) #The range of the price variable is 326 18823
mean(diamonds$color) #running this code states that the "argument is not numeric or logical"

##Linear Regression Practice##
library(MASS)
Boston = Boston
df_b = Boston
summary(df_b)

#Linear Regression 1
lm(df_b) 
##create a linear model (linear regression model) and predict medv from lstat
lm1 =lm(medv~lstat ,data=Boston ) #lm.fit - some basic information about the model is output * changed it to lm1 bc lm.fit is a diff command
 #Estimate for the intercept 34.55 and estimate for lsat is -0.95
summary (lm1)

#Linear Regression 2 
lm(df_b) #lm(y∼x1+x2+x3 <- used to fit a model with three predictors
lm2 = lm(medv~lstat+age  ,data=Boston )#*UPDATE IT WORKED I WAS SILLY BC I SWITCHED THE TWO VARIABLES AHHHHH
summary(lm2) #Estimate for the intercept is 120.1302

#Linear Regression 3
library (ISLR)
Auto = ISLR::Auto
Auto$df_cars <- Auto$mpg * Auto$cylinders * Auto$displacement * Auto$horsepower * Auto$weight * Auto$acceleration * Auto$year * Auto$origin * Auto$name
#I'm not sure if I did it this correctly, I'm sorry! 
lm3 <- lm(mpg ~ horsepower, data = Auto)
summary(lm3)
my_lm = lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(my_lm)

#Linear Regression 4
library(ISLR)
car_seats <- Carseats
?Carseats
head(car_seats) #The head() used to display the first n rows present in the input data frame. 
str(car_seats)
##Fit a multiple linear regression with sales as the outcome variable and price, urban, and US as the predictors.
lm4 = lm(Sales ~ Price+Urban+US, data = car_seats)
summary(lm4)
