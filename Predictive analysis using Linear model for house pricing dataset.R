###
# Title: Predicting house pricing using linear regression model
# Problem Statement: Predict the house pricing using Linear Regression
# Author: "Mousaidna Rosario"
# Dataset description : Dataset from Kaggle.com called Housing prices dataset
# Predictive analytics methods:
# Import data > Data cleaning > build a model > Train model > Test model > Improve efficiency
###


# Step 1. Import data 

pricing <- read.csv("housingdataset.csv")

head(pricing) # show the first 5 data of the data
dim(pricing)  # summary dimension of data,(rows, columns)
summary(pricing) # quick view of how the data looks, according to mean, median, min and max


# STEP 2 . Data cleaning

# Display structure of data
str(pricing)

# Pull important columns from the dataset and put it to another table for ease of use. 
# The dataset is big with 81 columns and it gets harder to process if we don't remove unnecessary data in the model.

house.pricing <-pricing[,c("LotArea","YearBuilt","BedroomAbvGr","SalePrice")]

str(house.pricing) # from 81 columns to 4 important variables to the Problem
summary(house.pricing)
plot(house.pricing )  # initial plot, will show you if the data will fit a linear model. 

par(mfrow=c(2,2))

plot(SalePrice ~ YearBuilt , data=house.pricing )
plot(SalePrice ~ BedroomAbvGr , data=house.pricing )
plot(SalePrice ~ LotArea  , data=house.pricing )

# Data preparation

# Identify rows with missing data
# Pull data that has no missing values
# Missing values can cause inaccurate data

house.pricing <- house.pricing[complete.cases(house.pricing),]

str(house.pricing) # no data is removed because all variables have values. 

# STEP 3. Build the model

set.seed(2)
install.packages("caTools") # splitting and predicting data
library(caTools)

# Split data into training and testing set. Training is always bigger than testing set 
n <- nrow(house.pricing)
s <- n *0.8 # 80% : data from 1:s
s1 <- s+1   # 20% : s+1 to the end of dataset

training.Set <- house.pricing[1:s,1:4]
testing.Set <- house.pricing[s1:n,1:4]

nrow(training.Set)
nrow(testing.Set)

# STEP 4 Train the model 
model <- lm(SalePrice ~ ., data = training.Set ) # dot shows multiple linear regression model

summary(model)

# Prediction where we use the testing data

predict.model <- predict(model, testing.Set) # predict using testing set. part of catools package
predict.model

# STEP 5. TEST THE MODEL
# Test outcomes by comparing predicted data vs observed data
plot(testing.Set$SalePrice, lty = 2, col = 2, type="l")
lines(predict.model, type="l",col=4) # plot overlap testing
plot(predict.model, lty = 2, col = 4, type="l")

# STEP 6. Improve efficiency

# Find accuracy
head(predict.model)
head(house.pricing$SalePrice)
rmse <- sqrt(mean(house.pricing$SalePrice-predict.model)^2) # root difference error
rmse
# Using RMSE (regression model accuracy measurement),
#371.4961 means The next time I will use this model to predict sales, I am off around $371.4961. 
