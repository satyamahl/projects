
library(MASS)
library(caret) 
library(ggplot2) 
library(caTools) 

rm(list = ls())
getwd()


# Loading 3 files

churn_data<- read.csv("churn_data.csv", stringsAsFactors = F)
View(churn_data)

customer_data<- read.csv("customer_data.csv", stringsAsFactors = F)
internet_data<- read.csv("internet_data.csv", stringsAsFactors = F)

dim(churn_data)    
dim(customer_data) 
dim(internet_data) 

View(churn_data)
View(customer_data)
View(internet_data)

# merge the datasets to one . 

telecom<- merge(churn_data,customer_data, by="customerID", all.x = TRUE)
dim(telecom)
telecom<- merge(telecom,internet_data, by="customerID", all.X=TRUE)

View(telecom) 


str(telecom)


telecom$SeniorCitizen<- ifelse(telecom$SeniorCitizen==1,"Yes","No")

# Missing value

sapply(telecom, function(x) sum(is.na(x))) 

View(subset(telecom, is.na(TotalCharges)))


telecom <- telecom[!is.na(telecom$TotalCharges),]

dim(telecom)

################################################################
# Feature standardisation

# converting target variable churn from No/Yes character to factorwith levels 0/1 

telecom$Churn<- ifelse(telecom$Churn=="Yes",1,0)


View(telecom)
# creating a dataframe of categorical features

telecom_chr<- telecom[,-c(1,2,7,8,9)]

View(telecom_chr)

str(telecom_chr)

# extracting categorical variables 
# Convert all categorical columns into Factor
telecom_fact<- data.frame(sapply(telecom_chr, function(x) factor(x)))

str(telecom_fact)

# creating dummy variables for factor attributes

dummies<- data.frame(sapply(telecom_fact, 
                            function(x) data.frame(model.matrix(~x,data =telecom_fact))[,-1]))

View(dummies)

names(dummies)

# Final dataset. Combine original required columns and dummy columns
telecom_final<- cbind(telecom[,c(9,2,7,8)],dummies) 

View(telecom_final) 

########################################################################

set.seed(123)  
indices = sample.split(telecom_final$Churn, SplitRatio = 0.7) 

train = telecom_final[indices==T,]

test = telecom_final[indices==F,]

View(train)
View(test)
dim(train)
dim(test)

# MODEL BUILDING
# glm - generalize linear model

model_1 = glm(Churn ~ ., data = train, family = "binomial")


View(test)

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

test_pred = predict(model_1, newdata = test[,-1])

test_pred

test$prob <- test_pred

View(test)


# Let's use the probability cutoff of 50%.

test_pred_churn <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_churn <- factor(ifelse(test$Churn==1,"Yes","No"))
test_actual_churn

# table(test_actual_churn,test_pred_churn)

test_conf <- confusionMatrix(test_pred_churn, test_actual_churn, positive = "Yes")
test_conf
# Postive - Class 1

# The above model is predicting Class "No" very well (Specificity is high)  

test_pred_churn <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_churn, test_actual_churn, positive = "Yes")
test_conf

# Extracting Sensitivity, Specificity and Accuracy
acc <- test_conf$overall[1]
acc
sens <- test_conf$byClass[1]
sens
spec <- test_conf$byClass[2]
spec


