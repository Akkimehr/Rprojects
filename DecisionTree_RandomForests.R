# MODULE 5

# Loading and analyzing the data
library(AER)
data("Affairs")
data <- Affairs[,]
str(data)
summary(Affairs)



# TASK 1. 
# Creating a new column with nominal values "YES" and "NO"
library(dplyr)
required_data <- mutate(data , nominal_values = ifelse(data$affairs == 0 , "YES" , "NO"))

# Converting nominal_values into Factors and displaying the required dataset
required_data$nominal_values <- as.factor(required_data$nominal_values)
str(required_data)



# TASK 2. CREATING DECISION TREE
library(rpart)
library(caTools)
set.seed(1000)
split_data <- sample.split(required_data$nominal_values , SplitRatio = 0.75)
train_data <- subset(required_data , split_data == "TRUE")
test_data <- subset(required_data , split_data == "FALSE")
decisiontree <- rpart(affairs ~ . , data = train_data)
summary(decisiontree)
predictions <- round(predict(decisiontree , test_data , type = "vector") , 0)
predictions_req <- ifelse(predictions == 0 , "YES" , "NO")
ndata <- data.frame(predictions_req , test_data$nominal_values)

# Plotting the decision tree
plot(decisiontree , margin = 0.2)
text(decisiontree , use.n = TRUE , pretty = TRUE , cex = 0.6 , col = "black")

# Calculating the accuracy using the confusion matrix
library(caret)
confusionmatrix <- confusionMatrix(ndata$predictions_req , ndata$test_data.nominal_values)
confusionmatrix


# TASK 3. CREATING A RANDOM FOREST
library(randomForest)
library(caTools)
set.seed(1000)
split_data_forest <- sample.split(required_data$nominal_values , SplitRatio = 0.75)
train_data_forest <- subset(required_data , split_data_forest == "TRUE")
test_data_forest <- subset(required_data , split_data_forest == "FALSE")
randomforest <- randomForest(affairs ~ . , data = train_data_forest)
summary(randomforest)
predictions_forest <- round(predict(randomforest , test_data_forest , type = "response") , 1)
predictions_forest_req <- ifelse(predictions_forest < 1.0 , "YES" , "NO")
ndata_forest  <- data.frame(predictions_forest_req , test_data_forest$nominal_values) 

# Calculating the accuracy using confusion matrix
confusionmatrix_forest <- confusionMatrix(ndata_forest$predictions_forest_req , ndata_forest$test_data_forest.nominal_values)
confusionmatrix_forest

# Importance pof attributes using importance() function
importance(randomforest)

