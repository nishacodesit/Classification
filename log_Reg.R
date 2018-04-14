
setwd("C:/Users/nimanick/Downloads")
my_data2 <- read.csv("train_data.csv")

summary(my_data2)
str(my_data2)


#-- Convert the Target variable to factor
my_data2$target <- as.factor(my_data2$target)

#unclass(my_data2$marital_status)


glm(target ~ working_sector, family="binomial", data = my_data2)

#converting the categorical variables to numeric
#--------------------------------------------------
my_data2$working_sector <- as.numeric(my_data2$working_sector)
my_data2$country <- as.numeric(my_data2$country)
my_data2$qualification <- as.numeric(my_data2$qualification)
my_data2$ethnicity <- as.numeric(my_data2$ethnicity)
my_data2$marital_status <- as.numeric(my_data2$marital_status)
my_data2$occupation <- as.numeric(my_data2$occupation)
my_data2$relationship <- as.numeric(my_data2$relationship)
my_data2$gender <- as.numeric(my_data2$gender)


str(my_data2)

#-----------------------------------------------
#remove the insignificant variables
my_data2 <- my_data2[,-1]  #index variable
my_data2 <- my_data2[,-6] #tax_paid
str(my_data2)

colSums(is.na(my_data2))
summary(my_data2)

#--- Still have NA values in the dataset, omitting the rows with NA values
my_data2 <- na.omit(my_data2)

summary(my_data2)

#splitting our data:
library(caTools)
split <- sample.split(my_data2,SplitRatio = 0.8)

split
training <- subset(my_data2, split=="TRUE")
testing <- subset(my_data2, split == "FALSE")
str(training)
str(testing)



#-----Log Reg Model Creation -----#
model <- glm(target~., training, family = "binomial")
summary(model)


#Predicting the value
res <- predict(model, testing, type = "response")
table(ActualValue=testing$target, PredictedValue=res>0.5)

#Null deviance: 24672  on 1698  degrees of freedom
#Residual deviance: 16539  on 1682  degrees of freedom
#AIC: 16571
#Accuracy is  0.8254749

#Trial 1 - removing a independent variable from the model----------
model <- glm(target~.-loan_taken, training, family = "binomial")
summary(model)
res <- predict(model, training, type = "response")
table(ActualValue=training$target, PredictedValue=res>0.5)
#Null deviance: 24672  
#Residual deviance: 16539
#AIC- 16569
#Accuracy is 0.8235455


#Trial 2 - removing a independent variable from the model----------no improvement for : country
model <- glm(target~. -country, training, family = "binomial")
summary(model)
res <- predict(model, testing, type = "response")
table(ActualValue=testing$target, PredictedValue=res>0.5)

#Null deviance: 24672  
#Residual deviance: 16540
#AIC: 16570 
#Accuracy is 0.8253383

#Trial 3
model <- glm(target~. -loan_taken -country, training, family = "binomial")
summary(model)
res <- predict(model, testing, type = "response")
table(ActualValue=testing$target, PredictedValue=res>0.5)

#Null deviance: 24672  
#Residual deviance: 16540
#AIC: 16568
#Accuracy is  0.8253383

head (res)

#we take the original model and ignore the trial 1, 2,3 since there is no improvement in the Accuracy.