# Random Forest Classification

# Importing the dataset

data_cap <- read_csv("Capstone_CSV.csv",col_names = TRUE)
data_cap<-data.frame(data_cap)
names(data_cap)<-gsub("\\.|X|\\d+","",names(data_cap))
data_cap$WEBSITE_ID <- seq.int(nrow(data_cap))
head(data_cap)

# Encoding the target feature as factor
data_cap$Result = factor(data_cap$Result, levels = c(1,- 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data_cap$Result, SplitRatio = 0.5)
training_set = subset(data_cap, split == TRUE)
training_set$Result = factor(training_set$Result)

test_set = subset(data_cap, split == FALSE)
test_set$Result = factor(test_set$Result)

# Fitting Random Forest Classification to the Training set
#install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-31],
                          y = training_set$Result,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-31])

# Making the Confusion Matrix
cm = table(test_set[, 31], y_pred)
cm
varImpPlot(classifier,n.var=12)


# Fitting Decision Tree Classification to the Training set
#install.packages('rpart')
library(rpart)
classifier_tree = rpart(formula = Result ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred_tree = predict(classifier_tree, newdata = test_set[-31], type = 'class')

# Making the Confusion Matrix
cm_tree = table(test_set[, 31], y_pred_tree)
cm_tree



# Fitting Logistic Regression to the Training set
classifier_LR = glm(formula = Result ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier_LR, type = 'response', newdata = test_set[-31])
y_pred_LR = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm_LR = table(test_set[, 31], y_pred_LR > 0.5)
cm_LR

#ROC Curves
#install.packages("ROCR")
library(ROCR)
#Random_forest
ROCRpred=prediction (as.numeric(y_pred),as.numeric(test_set$Result))
ROCRpref=performance(ROCRpred,"tpr","fpr")
plot(ROCRpref, colorize=TRUE,print.cutoff.at=seq(0,1,0.01))


#Decision_Tree
ROCRpred_tree=prediction (as.numeric(y_pred_tree),as.numeric(test_set$Result))
ROCRpref_tree=performance(ROCRpred_tree,"tpr","fpr")
plot(ROCRpref_tree, colorize=TRUE,print.cutoff.at=seq(0,1,0.01))

#Logistic_Regression
ROCRpred_LR=prediction (as.numeric(y_pred_LR),as.numeric(test_set$Result))
ROCRpref_LR=performance(ROCRpred_LR,"tpr","fpr")
plot(ROCRpref_LR, colorize=TRUE,print.cutoff.at=seq(0,1,0.01))

# Choosing the number of trees
plot(classifier)

# Plotting the tree
plot(classifier_tree)
text(classifier_tree)