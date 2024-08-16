# Task 3: Iris Flower Classification
library(caret)
library(tidyverse)
library(ggplot2)
library(e1071)

# load the data in iris dataset
data(iris)

#view the data
head(iris)
cat("\n statistical summary of iris dataset:\n")
summary(iris)
cat("\nstructure of iris datset:\n")
str(iris)

#convert the target variable into factor
iris$Species <- as.factor(iris$Species)

#use only sepal and petal measurements for prediction of species
predictors_iris <- iris[, 1:4]
head(predictors_iris)

#splitting data into training and test data
set.seed(127)
split_index <- createDataPartition(iris$Species, p = 0.6, list = FALSE)
train_data <- predictors_iris[split_index, ]
test_data <- predictors_iris[-split_index, ]
train_data_labels <- iris$Species[index]
test_data_labels <- iris$Species[-index]

#use support vector machine for classificatiion
svm_model <- svm(train_data, train_data_labels)

#predicting the species for test_data
predictions <- predict(svm_model, test_data)

#evaluating the model using confusion matrix
confusionMatrix(predictions, test_data_labels)

#visualise the dataset
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
