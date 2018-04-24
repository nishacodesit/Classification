# Problem - To build a classifier model to identify the companies that would bankrupt from a given dataset.
# Approach - Preprocess the data, use decision tree to model the classifier, Prune the tree if needed.

#------------PREPROCESSING THE DATA--------------------
## Using CentralImputation, SMOTE
#-----------------------------------------------------
rm(list = ls(all = TRUE))

##Read the csv file 
getwd()
my_data <- read.csv("/Users/nishamanickam/Downloads/train.csv")
summary(my_data)
str(my_data)

##Convert the target attribute to categorical variable
my_data$target <- as.factor(my_data$target)


##check for NA values
colSums(is.na(my_data)) 
  #Using Central Imputation to impute the values
library(DMwR)
my_data <- centralImputation(my_data)
sum(is.na(my_data))

##Remove any known insignificant attributes from the dataset - ID column
my_data$ID <- NULL

## Check for Class imbalance in the dataset
summary(my_data$target)

#Try upsampling the data
newData <- SMOTE(target ~ ., my_data, perc.over = 600,perc.under=100)
table(newData$target)


## Splitting the dataset to training and validation 70/30
set.seed(123)
sample <- sample.int(n = nrow(newData), size = floor(.7 * nrow(newData)), replace = F)
train_set <- newData[sample, ]
validation_set  <- newData[-sample, ]


##--------Building Model-----------#
#Using Decision Tree, libray- c5.0 
#-------------------------------------
library(C50)
c5_tree <- C5.0(target ~ . , train_set)

#build tree with control params
ctrl = C5.0Control(noGlobalPruning = TRUE, CF = 0.95, winnow = TRUE, minCases = 2, label = "model-interaction")
c5_rules <- C5.0(target ~ . , train_set, control = ctrl)

#Check the importance of the attributes
C5imp(c5_tree, metric = "usage")
summary(c5_rules)
plot(c5_tree)

##-------Prediction using the Model--------
#Using Validation data --------------------
#------------------------------------------.
preds <- predict(c5_tree, validation_set)

#confusion matrix
library(caret)
confusionMatrix(preds, validation_set$target)
#precision <- TP / (TP + FP)
#recall <- TP / (TP + FN)
precision <- 3367 / (3367 + 353)
recall <- 3367 / (3367 + 447)
Fscore <-  (2*precision*recall) / sum(precision, recall)
Fscore
#------RESULTS on VALIDATION DATA----------#
#[1] 0.8938147 

#plotting the results
barplot(table(preds),col = "Green")

