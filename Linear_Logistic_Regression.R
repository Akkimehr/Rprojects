# MODULE 4

# LOADING AND CLEANING THE DATA
setwd("E:\\r scripts\\datsets")
videogame <- read.csv("Videogame.csv")
summary(videogame)
videogame$Critic_Score <- round(ifelse(is.na(videogame$Critic_Score) == TRUE , mean(videogame$Critic_Score , na.rm = TRUE) , videogame$Critic_Score) , 2)
videogame$Critic_Count <- round(ifelse(is.na(videogame$Critic_Count) == TRUE , mean(videogame$Critic_Count , na.rm = TRUE) , videogame$Critic_Count) , 2)
videogame$User_Count <- round(ifelse(is.na(videogame$User_Count) == TRUE , mean(videogame$User_Count , na.rm = TRUE) , videogame$User_Count) ,2)
finaldata <- videogame[ , c(7 , 8 , 9 , 10 , 11 , 12 , 13 , 15)]


# TASK 1 LINEAR REGRESSION
# Creating test and train datasets
library(caTools)
set.seed(4500)
split_data <- sample.split(finaldata$Other_players , SplitRatio = 0.75)
train_data <- subset(finaldata , split_data == "TRUE")
test_data <- subset(finaldata , split_data == "FALSE")

# Creating new linear regression model
model <- lm(Other_players ~ . , data = train_data)

# Predicting the values
pred <- predict(model , test_data)
View(pred)


# TASK 2
# Creating a subset of the test and the predictions
test_data_100 <- test_data[1:100 , ]
pred_100 <- pred[1:100]

# Plotting actual values of test data in black 
plot(test_data_100$Other_players , type = 'l' , col = "black")

# Plotting predicted values in red
lines(pred_100 , type = 'l' , col = "red")


# TASK 3 LOGISTIC REGRESSION
# Cleaning and loading the data
employeedata <- read.csv("M4_Employee_Data.csv")
table(employeedata$Emp_Sal)

# Splitting the data in test and train data
set.sedd(1000)
split_data1 <- sample.split(employeedata$Emp_Sal , SplitRatio = 0.75)
train_data1 <- subset(employeedata , split_data1 == "TRUE")
test_data1 <- subset(employeedata , split_data1 == "FALSE")

# Building a model and predicting values
model1 <- glm(Emp_Sal~., data = train_data1 , family = "binomial")
predict1 <- predict(model1 , test_data1)
predict1 <- ifelse(predict1< 0.5 , "<=50K" , ">50K")
data <- data.frame(predict1 , test_data1$Emp_Sal)

# Building confusion Matrix with cutt-off value as 0.4
library(caret)
?confusionMatrix
cm <- confusionMatrix(data$predict1 , data$test_data1.Emp_Sal)
cm
