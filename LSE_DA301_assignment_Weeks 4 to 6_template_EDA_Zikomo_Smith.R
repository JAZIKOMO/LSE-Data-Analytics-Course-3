## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include yourinsights and observations.

###############################################################################

# 1. Load and explore the data

# Install necessary package.
install.packages('tidyverse') 
install.packages('skimr')  
install.packages('DataExplorer')    

# Import the necessary libraries.
library(tidyverse)
library(skimr)
library(DataExplorer)

#Import the dataset
turtle_sales <- read.csv(file.choose(), header=TRUE)


# View the data frame.

head(turtle_sales)

View(turtle_sales)

dim(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
salesfinal <- subset (turtle_sales, select = -c(Ranking, Year, Genre, Publisher))


# View the data frame.

View(salesfinal)

# Descriptive statistics
summary(salesfinal)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots
qplot(NA_Sales, EU_Sales, data=salesfinal)
qplot(NA_Sales, Global_Sales, data=salesfinal)
qplot(EU_Sales, Global_Sales, data=salesfinal)

## 2b) Histograms
# Create histograms.
qplot(NA_Sales, bins=20, data=salesfinal)
qplot(EU_Sales, bins=20, data=salesfinal)
qplot(Global_Sales, bins=20, data=salesfinal)


## 2c) Boxplots
# Create boxplots.
qplot(NA_Sales, data=salesfinal, geom="boxplot")
qplot(EU_Sales, data=salesfinal, geom="boxplot")
qplot(Global_Sales, data=salesfinal, geom="boxplot")

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
# EU and NA sales both correlate with global sales, respectively
# EU & NA sales have a weaker, though exisitng, correlation with one another
# The majority of sales happen with a long tail of lower-selling products


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# Check output: Determine the min, max, and mean values.
min(salesfinal$NA_Sales)
min(salesfinal$EU_Sales)
min(salesfinal$Global_Sales)

mean(salesfinal$NA_Sales) 
mean(salesfinal$EU_Sales)
mean(salesfinal$Global_Sales)

max(salesfinal$NA_Sales) 
max(salesfinal$EU_Sales)
max(salesfinal$Global_Sales)

# View the descriptive statistics.

summary(salesfinal)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
productsales <- salesfinal %>% group_by(Product) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))

# View the data frame.
head(salesfinal)

# Explore data frame.
summary(productsales)



## 2b) Determine which plot is the best to compare game sales.
# Create product scatterplots.
qplot(NA_Sales_sum, EU_Sales_sum, data=productsales)
qplot(NA_Sales_sum, Global_Sales_sum, data=productsales)
qplot(EU_Sales_sum, Global_Sales_sum, data=productsales)

#Create product histograms
qplot(NA_Sales_sum, bins=20, data=productsales)
qplot(EU_Sales_sum, bins=20, data=productsales)
qplot(Global_Sales_sum, bins=20, data=productsales)

# Create product boxplots.
qplot(NA_Sales_sum, data=productsales, geom="boxplot")
qplot(EU_Sales_sum, data=productsales, geom="boxplot")
qplot(Global_Sales_sum, data=productsales, geom="boxplot")


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(productsales$NA_Sales_sum)
qqline(productsales$NA_Sales_sum)

qqnorm(productsales$EU_Sales_sum)
qqline(productsales$EU_Sales_sum)

qqnorm(productsales$Global_Sales_sum)
qqline(productsales$Global_Sales_sum)


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments") 
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test((productsales$NA_Sales_sum))
shapiro.test((productsales$EU_Sales_sum))
shapiro.test((productsales$Global_Sales_sum))


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(productsales$NA_Sales_sum) 
kurtosis(productsales$NA_Sales_sum) 

skewness(productsales$EU_Sales_sum)
kurtosis(productsales$EU_Sales_sum)

skewness(productsales$Global_Sales_sum)
kurtosis(productsales$Global_Sales_sum)


## 3d) Determine correlation
# Determine correlation.

cor(productsales$EU_Sales_sum, productsales$Global_Sales_sum)

cor(productsales$NA_Sales_sum, productsales$Global_Sales_sum)



###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

ggplot(data=productsales, 
       mapping=aes(x=Global_Sales_sum, y=NA_Sales_sum)) + 
  geom_point(color = "blue") + geom_smooth(method = "lm")

ggplot(data=productsales, 
       mapping=aes(x=Global_Sales_sum, y=EU_Sales_sum)) + 
  geom_point(color = "blue") + geom_smooth(method = "lm")

ggplot(data=productsales, 
       mapping=aes(x=NA_Sales_sum, y=EU_Sales_sum)) + 
  geom_point(color = "blue") + geom_smooth(method = "lm")

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# Data is not normally distributed
# Data is heavily skewed left towards lower selling titles

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the wilrelation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the wilrelation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.


# Determine a summary of the data frame.
#see the top sellers to find a product to assess
# see that 107 is the highest seller
productsales %>%
  arrange(desc(Global_Sales_sum)) %>%
  slice(1:10) 

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the wilrelation between columns
# Create a linear regression model on the original data.

simple.fitNAG = lm(NA_Sales_sum~Global_Sales_sum, data=productsales)

simple.fitEUG = lm(EU_Sales_sum~Global_Sales_sum, data=productsales)

simple.fitNAEU = lm(NA_Sales_sum~EU_Sales_sum, data=productsales)

simple.fitEUNA = lm(EU_Sales_sum~NA_Sales_sum, data=productsales)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.


ggplot(simple.fitNAG, aes(NA_Sales_sum, Global_Sales_sum)) +
  geom_point()

ggplot(simple.fitEUG, aes(EU_Sales_sum, Global_Sales_sum)) +
  geom_point()

ggplot(simple.fitNAEU, aes(NA_Sales_sum, EU_Sales_sum)) +
  geom_point()

ggplot(simple.fitEUNA, aes(EU_Sales_sum, NA_Sales_sum)) +
  geom_point()



###############################################################################

# 3. Create a multiple linear regression model

# Multiple linear regression model.
multi.fit = lm(Global_Sales_sum~EU_Sales_sum+NA_Sales_sum+Product, data=productsales)
summary(multi.fit)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of rewilds.

# Compare with observed values for a number of rewilds.
# Predict based on NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.

Question1 <- data.frame(EU_Sales_sum=c(23.80), NA_Sales_sum=c(34.02), Product=c(107))

predict(multi.fit, newdata=Question1)

# Predict based on NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.

Question2 <- data.frame(EU_Sales_sum=c(3.93), NA_Sales_sum=c(1.56), Product=c(99))

predict(multi.fit, newdata=Question2)

# Predict based on NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.

Question3 <- data.frame(EU_Sales_sum=c(2.73), NA_Sales_sum=c(0.65), Product=c(176))

predict(multi.fit, newdata=Question3)


# Predict based on NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.

Question4 <- data.frame(EU_Sales_sum=c(2.26), EU_Sales_sum=c(0.97), Product=c(258))

predict(multi.fit, newdata=Question4)

# Predict based on NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

Question5 <- data.frame(EU_Sales_sum=c(22.08), NA_Sales_sum=c(0.52), Product=c(326))

predict(multi.fit, newdata=Question5)


###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# 3 of the top ten selling products sell almost double in NA than they do in the EU
# Predictive model is most accurate for a high-selling product ID (107)
# The predictive model tends to overshoot results across the lower selling products


###############################################################################
###############################################################################




