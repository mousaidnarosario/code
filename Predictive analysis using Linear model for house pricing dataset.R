###
# Title: Explore linear regression model in predicting data
# Problem Statement: Predict the USA house prices using Linear Regression
# Author: "Mousaidna Rosario"
# Dataset description : Dataset from studygyaan.com called usa housing dataset
# Predictive analytics methods:
# Import data > Data cleaning > build a model > Train model > Test model > Improve efficiency
###


# Step 1. Import data 

usa.pricing <- read.csv("usaHousing.csv")

head(usa.pricing) # show the first 5 data of the data
dim(usa.pricing)  # summary dimension of data,(rows, columns)
summary(usa.pricing) # quick view of how the data looks, according to mean, median, min and max


# STEP 2 . Data cleaning

# Display structure of data
str(usa.pricing)

# fix the label of the data
names(usa.pricing) <- c("Avg.Area.Income", "Avg.House.Age","Avg.Rooms" ,"Avg.Area.Bedrooms",
                        "Population","Price","Address")


str(usa.pricing)

# Pull important columns from the dataset and put it to another table for ease of use. 
# I will not need address for this model

uhp <-usa.pricing[,c("Avg.Area.Income", "Avg.House.Age","Avg.Rooms" ,"Avg.Area.Bedrooms",
                           "Population","Price")]

str(uhp) # I will use this df moving forward
summary(uhp)
plot(uhp)  # initial plot, will show you if the data will fit a linear model. 

par(mfrow=c(2,2))

# initial look at different things to train on to know the price.
plot(Price ~ Avg.Area.Income , data=uhp )
plot(Price ~ Avg.Rooms , data=uhp )
plot(Price ~ Avg.House.Age , data=uhp )

# Data preparation

# Identify rows with missing data
# Pull data that has no missing values
# Missing values can cause inaccurate data

uhp <- uhp[complete.cases(uhp),]

str(uhp) # no removed data because there is no missing values. 

# STEP 3. Build the model

set.seed(2)
#install.packages("caTools") # splitting data
library(caTools)

# Split data into training and testing set. Training is always bigger than testing set 
n <- nrow(uhp)
s <- n *0.8 # 80% : data from 1:s
s1 <- s+1   # 20% : s+1 to the end of dataset

training.Set <- uhp[1:s,1:6] # taking 80% of the data for all variables
testing.Set <- uhp[s1:n,1:6] # taking 20% of the data for all variables

nrow(training.Set) #count training set
nrow(testing.Set) #count testing set

head(training.Set) 
head(testing.Set)

# STEP 4 Train the model 
model <- lm(Price ~ ., data = training.Set ) # dot compare prices among all variables to train on. (y dependent var ~x independent var)

summary(model)

# Prediction where we use the testing data

predict.model <- predict(model, testing.Set) # predict using testing set. part of catools package
head(predict.model)

# STEP 5. TEST THE MODEL
# Test outcomes by comparing predicted data vs observed data

par(mfrow=c(2,2))
plot(testing.Set$Price, lty = 2, col = 2, type="l", main="Predicted and testing model overlap")
lines(predict.model, type="l",col=4) # plot overlap testing
plot(predict.model, lty = 2, col = 4, type="l", main="Predicted model")

# The plot illustrates that the frequency of testing and predicted model is almost the same (predicted model covers test data well)

# STEP 6. Improve efficiency

hist(testing.Set$Price-predict.model) # The model and testing set shows a fair normal distribution.

plot(testing.Set$Price,predict.model, main="Predicted data vs testing data\n in LM model", col=c(2,3))
      # the model and testing set is linear 
legend("bottomright", "(Predicted, Testing)", pch=1, title= "bottomright")
     
     
# Find accuracy
head(predict.model)
head(uhp$Price)
mse <- mean((uhp$Price -predict.model)^2) 
rmse = sqrt(mse)
rmse

#or install caret for testing 
#install.packages('caret')
library(caret)

RMSE(predict.model,uhp$Price)
R2(predict.model,uhp$Price, form = "traditional")

