##Collage Admission Project

##Problem Statement : An education department in the US needs to analyze the factors that influence
##the admission of a student into a college
##-------------------------------------------------------------------------------------------------------
#load all the required libraries
library(corrplot)
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(randomForest)
library(dplyr)

raw_df = read.csv(choose.files())
data = raw_df #take the backup of data

###-------------------------------------------------------------------------------------------------------
#Analyze the historical data and determine the key drivers. 
###-------------------------------------------------------------------------------------------------------

data[1:3,]
str(data)
table(data$admit) ##This is Target variable and is
table(data$gre)
table(data$gpa)
boxplot(data$gpa)
table(data$ses)
table(data$Gender_Male)
table(data$Race)
table(data$rank)

boxplot(data$gre) 

summary(data$gre)

#Check if there are any null records
colSums(is.na(data))

cor(data)
corrplot(cor(data))
##Corrplot shows that admission is weekly related to gre, gpa and rank
##However there is strong relation betwee gre and gpa

#Split the data into train and test
set.seed(100)
split = sample.split(Y = data$admit, SplitRatio = .7)
training = raw_df[split,]
test = raw_df[!split,]

remove(split)

##------------------------------------------------------------------------------------------------------
#Run logistic model to determine the factors that influence the admission process of a student 
#(Drop insignificant variables)
#Transform variables to factors wherever required
#Calculate accuracy of the model 
##------------------------------------------------------------------------------------------------------

#Use logistic regression using family="binomial" as admit variables has only 2 categories
lm_logreg = glm(formula = as.factor(admit) ~ ., family = "binomial", data = training)
summary(lm_logreg)

lm_logreg_step1 = step(object = lm_logreg, direction = "both")
summary(lm_logreg_step1)

remove(lm_logreg)
##------------------------------------------------------------------------------------------
##gre, gpa and rank are good predictors of the admit
##rank is significant factors for calculating admit and null hypothesis is rejected at
##0.1% significance level
##gpa is significant factors for calculating admit and null hypothesis is rejected at
##5% significance level
##gre is significant factors for calculating admit and null hypothesis is rejected at
##10% significance level

##Equation is
## admit = -0.677324 * rank + 1.005352 * gpa + 0.002398 * gre - 3.987027
##------------------------------------------------------------------------------------------

#Decide the threshold using ROC curve
ROCRpred = prediction(predictions = lm_logreg_step1$fitted.values,labels = training$admit)
ROCRperf = performance(prediction.obj = ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = T, print.cutoffs.at = seq(0,1,.025), text.adj=c(-.2,1.7))

rm(ROCRpred, ROCRperf)
#Based on the ROC curve 0.38 can be considered for threshold

#predict the probability values for the test data
predict_test = predict(object = lm_logreg_step1, newdata = test, type = "response")

final_predict = ifelse(predict_test >= 0.38, 1, 0)
table(final_predict)

#confusion matrix to check the accuracy model
table(actual = test$admit, predicted = final_predict)->tab
accuracy = (tab[1,1]+tab[2,2])/nrow(test)
accuracy

#Create a result table to store the model and accuracy

result = data.frame("Model" = "Logistic Regression", "Accuracy" = round(accuracy * 100, digits = 2))

result
rm(lm_logreg_step1, accuracy, final_predict, predict_test, tab)
##This model is 65.00% Accurate
###_-----------------------------------------------------------------------------------------------------

###-----------------------------------------------------------------------------------------------------
#Try other modeling techniques like decision tree and SVM and select a champion model
#Determine the accuracy rates for each model
#Select the most accurate model
#Identify other Machine learning or statistical techniques that can be used 
###-----------------------------------------------------------------------------------------------------

#Decision Trees----------------------------------------------------------------
lm_dt = rpart(as.factor(admit)~.,data = training )

prp(lm_dt)
plotcp(lm_dt)
mod = prune(tree = lm_dt, cp = .021)
prp(mod)

#make predictions on test data
predict_test = predict(object = mod, newdata = test, type = "class")

#confusion matrix
table(predicted = predict_test, actual = test$admit)->tab
accuracy = (tab[1,1]+tab[2,2])/nrow(test)
round(accuracy * 100, digits = 2)

result = rbind(result, data.frame("Model" = "Decision Tree", "Accuracy" = round(accuracy * 100, digits = 2)))

result

rm(lm_dt, mod, accuracy, predict_test, tab)
##This model is 69.17% Accurate

#SVM: Support vector Machines 

#We want to standardize our columns
head(training)
training_scaled = scale(training[,c(2, 3)])
training_scaled = cbind(training[,c(1,4,5,6,7)], training_scaled)

head(test)
test_scaled = scale(test[,c(2, 3)])
test_scaled = cbind(test[,c(1,4,5,6,7)], test_scaled)
head(training_scaled)

svm_cl = svm(as.factor(admit)~., training_scaled, type = 'C-classification', kernel = 'linear')
prediction = predict(svm_cl,test_scaled)
table(prediction, test$admit)->tab
tab
(tab[1,1]+tab[2,2])/nrow(test)->accuracy
accuracy

result = rbind(result, data.frame("Model" = "SVM Linear", "Accuracy" = round(accuracy * 100, digits = 2)))
result
#Since all the predicitions are 0, we conclude that this problem is a non-linear classification problem.
#   Any of the linear classification algorithms will not give good accuracy in classifcation
#   Eg. logistic regression / svm linear will not give good results

rm(svm_cl, accuracy, prediction, tab, training)

#support vector machines - radial
svm_radial_cf = svm(as.factor(admit)~., training_scaled, type = 'C-classification', kernel = 'radial')
prediction = predict(svm_radial_cf,test_scaled)
table(prediction, test$admit)->tab
tab
(tab[1,1]+tab[2,2])/nrow(test)->accuracy
round(accuracy * 100, digits = 2)

result = rbind(result, data.frame("Model" = "SVM radial", "Accuracy" = round(accuracy * 100, digits = 2)))
result

rm(svm_radial_cf, test, test_scaled, training_scaled, accuracy, prediction, tab)
##This model is 66.67% Accurate

#Random Forest model
#k-fold cross validation - to check the performance of the model on variance error
k = 10
folds = createFolds(y = data$admit,k = k)

fn = function(x)#x will take each and every fold one-by-one
{
  training_fold = data[-x,]# will have all the rows in raw_df except for row numbers in x
  test_fold = data[x,]#we will have all rows from raw_df db having row numbers = x
  
  rmf_cl = randomForest(as.factor(admit)~.,data = training_fold, type = 'C-classification')
  predict(rmf_cl, test_fold) -> prediction
  
  table(actual = test_fold$admit, predicted = prediction)-> tab
  accuracy = (tab[1,1]+tab[2,2])/nrow(test_fold)
  return(accuracy)
}

lapply(X = folds, fn)->cv
unlist(cv)
mean(unlist(cv))
round(mean(unlist(cv)) * 100, digits = 2)

##This model is 70.75% Accurate
result = rbind(result, data.frame("Model" = "Random Forest", "Accuracy" = round(mean(unlist(cv)) * 100, digits = 2)))
result

remove(cv, folds, k, fn)
#########################################################################################################
##From the result 10 fold Random Forest looks to be the best model
#########################################################################################################

####----------------------------------------------------------------------------------------------------
#Categorize the grade point average into High, Medium, and Low (with admission probability percentages)
#and plot it on a point chart. 
####----------------------------------------------------------------------------------------------------

#Recategorise the gre as Low, Medium, High
data[1:6,]
data$gre_category = cut(x = data$gre, breaks = c(0, 439, 579, max(data$gre)), labels = c("Low", "Medium", "High") )

data[1:6,]

PT_final = summarise(.data = group_by(data, gre_category), sum = sum(admit), length = length(admit), prob = sum(admit)/length(admit))
PT_final

ggplot(PT_final,aes(x=gre_category,y=prob))+geom_point()

table(data$admit, data$gre_category)
#####---------------------------------------------------------------------------------------------------



