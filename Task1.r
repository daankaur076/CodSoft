# Task 1: Titanic Survival Prediction
library(dplyr)
library(titanic)
library(ggplot2)
library(tidyverse)
library(caret)

#titanic dataset
head(titanic)
summary(titanic)
str(titanic)

# modelling the training and test data
data("titanic_train") # 891 obs. of  12 variables
data("titanic_test") # 418 obs. of  11 variables

# Handling missing values
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = TRUE)

# Convert categorical variables to factors
titanic $ Sex <- as.factor(titanic $ Sex)
titanic $ Embarked <- as.factor(titanic $ Embarked)
titanic $ Pclass <- as.factor(titanic $ Pclass)

# design the formula for survival prediction
formula <- Survived ~ (Sex + Pclass + Age + Embarked)
model <- glm(formula, data = titanic_train, family = binomial)
summary(model)

# visualisation between variables and target variable
ggplot(titanic, aes(x = Survived, fill = Sex)) + geom_bar()
ggplot(titanic, aes(x = Survived, fill = Pclass)) + geom_bar()

# Predict survival probability
predictions <- predict(model, newdata = titanic_test, type = "response")

# Model Evaluation
confusionMatrix(predictions, test$Survived)

# Survival Probability is used as an Attribute
titanic_test$Predicted <- ifelse(predictions > 0.5, 1, 0)

str(titanic_test) # 418 obs. of  12 variables