# CERTIFICATION PROJECT 

# 1. SETTING DIRECTORY AND LOADING THE REQUIRED LIBRARIES
setwd("E:\\r scripts\\datsets")
library(ggplot2)
data <- read.csv("338_cert_proj_datasets_v3.0.csv")
View(data)


# 2. FINDING INSIGHTS OF THE DATA
## Finding the CORRELATION of the attributes of the data and structuring the data
cor_matrix <- cor(data[ , 1:8])
cor_matrix

## Visualizing the data 
data_left <- subset(data , left == 1)
### Visualizing for the whole data
ggplot(data , aes(x = data$time_spend_company)) + geom_histogram(bins = 50)
ggplot(data, aes(x = data$average_montly_hours , fill = data$salary)) + geom_histogram(bins = 250)
ggplot(data , aes(x = data$satisfaction_level , fill = data$salary)) + geom_histogram(bins = 150)
### Visualizing for the people who left
ggplot(data_left , aes(x = data_left$department)) + geom_histogram(stat = "count" , bins = 100)
ggplot(data_left , aes(x = data_left$time_spend_company)) + geom_histogram(bins = 20)
ggplot(data_left , aes(x = data_left$salary , y = data_left$satisfaction_level , col = data_left$time_spend_company)) + geom_point()

## Evaluating attributes for both left and non-left employees
data_unleft <- subset(data, left == 0)
correlation_data_unleft <- cor(data_unleft[ , c(1,2,3,4,5,6,8)])
correlation_data_left <- cor(data_left[ , c(1,2,3,4,5,6,8)])
correlation_data_left
correlation_data_unleft

## Finding the percentage of people leaving from each department
acc_per <-round(sum(data_left$department == 'accounting')/sum(data$department == 'accounting'),1) * 100
hr_per <- round(sum(data_left$department == 'hr')/sum(data$department == 'hr'),1) * 100
IT_per <- round(sum(data_left$department == 'IT')/sum(data$department == 'IT'),1) * 100
man_per <- round(sum(data_left$department == 'management')/sum(data$department == 'management'),1) * 100
mar_per <- round(sum(data_left$department == 'marketing')/sum(data$department == 'marketing'),1) * 100
prod_per <- round(sum(data_left$department == 'product_mng')/sum(data$department == 'product_mng'),1) * 100
Rand_per <- round(sum(data_left$department == 'RandD')/sum(data$department == 'RandD'),1) * 100
sales_per <- round(sum(data_left$department == 'sales')/sum(data$department == 'sales'),1) * 100
support_per <- round(sum(data_left$department == 'support')/sum(data$department == 'support'),1) * 100
tech_per <- round(sum(data_left$department == 'technical')/sum(data$department == 'technical'),1) * 100
total_per_dept <- as.matrix(list("Accounting" = acc_per , "HR" = hr_per , "IT" = IT_per , "Management" = man_per , "Marketing" = mar_per , "Product_mng" = prod_per , "RandD" = Rand_per , "Sales" = sales_per , "Support" = support_per , "Technical" = tech_per))
total_per_dept


# 3. BUILDING CLASSIFICATION MODELS FOR FORECASTING ATTRIBUTES OF PEOPLE WHO HAVE LEFT
library(caTools)
library(caret)
data$left <- as.factor(data$left)
data$Work_accident <- as.factor(data$Work_accident)
data$promotion_last_5years <- as.factor(data$promotion_last_5years)
split <- sample.split(data$left , SplitRatio = 0.75)
train_data <- subset(data , split == 'TRUE')
test_data <- subset(data , split == 'FALSE')
## Deciosion Tree Model
library(rpart)
dt_model <- rpart(left ~ . , data = train_data)
dt_preds <- predict(dt_model , test_data , type = "class")
acc_dt <- confusionMatrix(table(dt_preds , test_data$left))

## Random Forest Model
library(randomForest)
rf_model <- randomForest(left ~ . , data = train_data)
rf_preds <- predict(rf_model , test_data , type = "response")
acc_rf <- confusionMatrix(table(rf_preds , test_data$left))

## Naive Bayes Model
library(e1071)
nb_model <- naiveBayes(left ~ . , data = train_data)
nb_preds <- predict(nb_model , test_data)
acc_nb <- confusionMatrix(table(nb_preds , test_data$left))

## SVM Model
svm_model <- svm(left ~ . , data = train_data)
svm_preds <- predict(svm_model , test_data , decision.values = TRUE)
acc_svm <- confusionMatrix(table(svm_preds , test_data$left))

## Finding the best model
library(hash)
model_accuracy <- hash("Decision Trees" = acc_dt$overall['Accuracy'] , "Random Forest" = acc_rf$overall['Accuracy'] , "Naive Bayes" = acc_nb$overall['Accuracy'] , "SVM" = acc_svm$overall['Accuracy'])
val <- unlist(as.list(model_accuracy))
val

