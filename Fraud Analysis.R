#*********************************************
#PREDICTING EARNING MANIPULATORS BY INDIAN FIRMS
#*********************************************

#CHECKING ACCURACY OF THE BENEISH MODEL

#*********************TRAINING DATA*********************
MC1 <- Manipulators_train

MC1["M-Score"] = -4.84 + 0.92* MC1$DSRI + 0.528* MC1$GMI + 0.404* MC1$AQI + 0.892* MC1$SGI + 
  0.115* MC1$DEPI -0.172 * MC1$SGAI + 4.679 * MC1$ACCR - 0.327 * MC1$LEVI

nrow(MC1[MC1$`M-Score`>-2.22,])
MC1$`Predicted Category`[MC1$`M-Score`>-2.22] <- 1
MC1$`Predicted Category` <- 0

confusionMatrix(MC1$`C-MANIPULATOR`,MC1$`Predicted Category`,positive = "1")
#Sensitivity = 40% ; Specificity = 100% ; Kappa : 42.9% ; Accuracy = 73.6%



#*********************COMPLETE DATA*********************
MC1 <- Manipulators_complete

MC1["M-Score"] = -4.84 + 0.92* MC1$DSRI + 0.528* MC1$GMI + 0.404* MC1$AQI + 0.892* MC1$SGI + 
  0.115* MC1$DEPI -0.172 * MC1$SGAI + 4.679 * MC1$ACCR - 0.327 * MC1$LEVI

nrow(MC1[MC1$`M-Score`>-2.22,])
#Predicts 410 manipulators
MC1$`Predicted Category` <- 0
MC1$`Predicted Category`[MC1$`M-Score`>-2.22] <- 1
View(MC1)

confusionMatrix(MC1$`C-MANIPULATOR`,MC1$`Predicted Category`,positive = "1")
#Sensitivity = 9.5% ; Specificity = 100% ; Kappa : 12.33% ; Accuracy = 70%


str(Manipulators_train)
View(Manipulators_train)
cor(Manipulators_train[-10])
#DSRI and SGAI are correlated
Manipulators_train$Manipulator <- as.factor(Manipulators_train$Manipulator) 
pairs(Manipulators_train)

#******************************************************************************************************
#TRAINING DATA SET:220 DATAPOINTS
#************************************************************************************************************

attach(Manipulators_train)

formula_logit <- relevel(factor(`C-MANIPULATOR`),ref="0") ~ DSRI + GMI + AQI + SGI + DEPI + SGAI + ACCR + LEVI
manipulator_logit <- glm(formula = formula_logit, data = Manipulators_train, family = "binomial")
summary(manipulator_logit)
#difference in deviance(Null - Residual) = 205.58 - 119.77 = 85.81
#AIC = 137.12

formula_logit2 <- relevel(factor(`C-MANIPULATOR`),ref="0") ~ DSRI + GMI + AQI + SGI + DEPI + SGAI + ACCR 
manipulator_logit2 <- glm(formula = formula_logit2, data = Manipulators_train, family = "binomial")
summary(manipulator_logit2)
#AIC = 135.23

formula_logit3 <- relevel(factor(`C-MANIPULATOR`),ref="0") ~ DSRI + GMI + AQI + SGI + DEPI + ACCR 
manipulator_logit3 <- glm(formula = formula_logit3, data = Manipulators_train, family = "binomial")
summary(manipulator_logit3)
#AIC = 133.57

formula_logit4 <- relevel(factor(`C-MANIPULATOR`),ref="0") ~ DSRI + GMI + AQI + SGI  + ACCR 
manipulator_logit4 <- glm(formula = formula_logit4, data = Manipulators_train, family = "binomial")
summary(manipulator_logit4)
#difference in deviance(Null - Residual) = 205.58 - 119.77 = 85.81
#AIC = 131.77

exp(coef(manipulator_logit4))
exp(coef(manipulator_logit))

#Deviance of our model
with(manipulator_logit4, null.deviance - deviance)
with(manipulator_logit4, df.null- df,residual)
#test for significance
with(manipulator_logit4, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#P-value = 5.096617e-17
#Chi aquare of 85.80777, degree of freedom of 5, and p-value of less than 0.0001 indicates our model is 
#significantly better than an empty model

predicted1 <- predict(manipulator_logit, type = "response")
predicted2 <- predict(manipulator_logit2, type = "response")
predicted3 <- predict(manipulator_logit3, type = "response")
predicted4 <- predict(manipulator_logit4, type = "response")

library(ROCR)
prediction.obj1 <- prediction(predicted1,Manipulators_train$`C-MANIPULATOR`)
perf1 = performance(prediction.obj1,"tpr","fpr")

prediction.obj2 <- prediction(predicted2,Manipulators_train$`C-MANIPULATOR`)
perf2 = performance(prediction.obj2,"tpr","fpr")

prediction.obj3 <- prediction(predicted3,Manipulators_train$`C-MANIPULATOR`)
perf3 = performance(prediction.obj3,"tpr","fpr")

prediction.obj4 <- prediction(predicted4,Manipulators_train$`C-MANIPULATOR`)
perf4 = performance(prediction.obj4,"tpr","fpr")

plot(perf1,xlab="False Positive Rate (False Alarm)",ylab="True Positive Rate (Recall or Sensitivity)",
     main="ROC Curve", col = "red" )
plot(perf2, add = TRUE, col = "green")
plot(perf3, add = TRUE, col = "yellow")
plot(perf4, add = TRUE, col = "blue")

#col = "black", lty = 3, lwd = 3)

auc1 = unlist(slot(performance(prediction.obj1, "auc"), "y.values"))
auc2 = unlist(slot(performance(prediction.obj2, "auc"), "y.values"))
auc3 = unlist(slot(performance(prediction.obj3, "auc"), "y.values"))
auc4 = unlist(slot(performance(prediction.obj4, "auc"), "y.values"))

# Adding ROC AUC to the plot
aucVal1 = paste(c("AUC(red) = "), auc1, sep = "") 
aucVal2 = paste(c("AUC(green) = "), auc2, sep = "") 
aucVal3 = paste(c("AUC(yellow) = "), auc3, sep = "") 
aucVal4 = paste(c("AUC(blue) = "), auc4, sep = "") 
#Paste: Concatenate vetors after converting to character
legend(0.46, 0.5, c(aucVal1, aucVal2,aucVal3,aucVal4), cex = 0.7, box.col = "white")
abline(a= 0, b=1, col="gray")  #Add straight lines to a plot (a = intercept and b = slope)


opt.cut.distance = function(perf, pred)
{
  cut.ind = mapply(FUN=function(x, y, p)
  {
    d = ((x - 0)^2 + (y-1)^2)^(1/2)
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}


# Youden's coding : J = sensitivity + specificity ??? 1
opt.cut.youden = function(perf, pred)
{
  cut.ind = mapply(FUN=function(x, y, p)
  {
    J = y + (1-x) - 1
    ind = which(J == max(J))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

print(opt.cut.youden(perf3, prediction.obj3))
print(opt.cut.distance(perf3, prediction.obj3))

#Both distance and Youden"s index give the same cut-off point


#calculating the error with threshold = 0.13
Manipulators_train["Predicted Category"] <- rep(NA,nrow(Manipulators_train))
predicted4[predicted4 < 0.13]
Manipulators_train$`Predicted Category`[predicted4 < 0.13] <- 0
Manipulators_train$`Predicted Category`[predicted4 > 0.13] <- 1


library(e1071)
library(caret)
#Checking the train sensitivity, accuracy and Specificity
Error0.13 <- confusionMatrix(Manipulators_train$`Predicted Category`,Manipulators_train$`C-MANIPULATOR`)
Error0.13
#Accuracy = 82.73 Sensitivity = 81.77 Specificity = 87.18


#Checking the test sensitivity, presistion, f-score and kappa statistic
#Here we use repetitive cross validation dividing the dataset into 70% train and 30% test
#we Shuffle the dataset each time, divite the dataset, check the test error for each run and average 
#the test error across all runs to get a better idea of the how the model behaves as a whole
crossValidation70.30 = function(myFormula,dataset)
{
  s=0
  k=0
  f=0
  sp=0
  for (i in 1:10)
  {  
    dataset <- dataset[sample(nrow(dataset), nrow(dataset)), ]
    TestdataMani <- createDataPartition(dataset$`C-MANIPULATOR`,times=1, p=0.3,list=FALSE)
    Testdata <- dataset[TestdataMani[1:nrow(TestdataMani)],]
    Traindata <- dataset[TestdataMani[1:nrow(TestdataMani)],]
    x <- glm(myFormula, data =Traindata, family = "binomial")
    yprob <- predict(x, newdata = Testdata , class = "response")
    preds <- rep(NA,nrow(Testdata))
    preds[yprob<=0.13] <- 0
    preds[yprob>0.13] <- 1
    ConMat <- confusionMatrix(preds,Testdata$`C-MANIPULATOR`,positive = "1")
    #print(ConMat$table)
    recall = ConMat$table[2,2]/(ConMat$table[1,2]+ConMat$table[2,2])
    presicion = ConMat$table[2,2]/(ConMat$table[2,1]+ConMat$table[2,2])
    specificity = ConMat$table[1,1]/(ConMat$table[1,1]+ConMat$table[2,1])
    FScore = 2*recall*presicion/(recall+presicion)
    #print(c("Sensitivity is",recall))
    #print(c("F-Score ",FScore))
    #print(c("Precision ",presicion))
    #print(c("Specificity of fold",i,specificity))
    kappa = ConMat$overall[2]
    s=s+recall
    f=f+FScore
    k=k+kappa
    sp=sp+specificity
  }
  print(c("Average Sensitivity is", s/10))
  print(c("Average F-Score is", f/10))
  print(c("Average Kappa is", k/10))
  print(c("Average Specificity is", sp/10))
}


crossValidation70.30(formula_logit4,Manipulators_train)
#Average Sensitivity is 0.5505 ;  Average F-Score is 0.635124 ;Average Kappa is 0.5783481
#Specificity is 0.970982

mScore.threshold = log(0.13/(1-0.13))

detach(Manipulators_train)

library(DMwR)
attach(Manipulators_complete)
str(Manipulators_complete)
#Converting target category variables to as.factor
Manipulators_complete$Manipulater <- as.factor(Manipulators_complete$Manipulater)
Manipulators_complete$`C-MANIPULATOR` <- as.factor(Manipulators_complete$`C-MANIPULATOR`)

manipulator_data <- as.data.frame(Manipulators_complete[,c(1:11)])
#Using SMOTE to balance the unbalanced dataset
formula_logitS <- `C-MANIPULATOR` ~ DSRI + GMI + AQI + SGI + DEPI + SGAI + ACCR + LEVI
manipulator_SMOTEd <- SMOTE(formula_logitS, manipulator_data, perc.over = 300, perc.under = 0)
table(manipulator_SMOTEd$`C-MANIPULATOR`)

#combining the original dataset with the SMOTEd dataset to get the final balanced dataset
#the dataset has 17.7% positives (very close to the training dataset, so our comparison is valid)
Manipulators_completeSM <- rbind(Manipulators_complete,manipulator_SMOTEd)
table(Manipulators_completeSM$`C-MANIPULATOR`)
dim(manipulatorAll)

#******************************************************************************************************
#COMPLETE DATA SET:1200 DATAPOINTS
#***************************************************************************************************************
attach(Manipulators_completeSM)

formula_logitCom <- relevel(factor(`C-MANIPULATOR`),ref="0") ~ DSRI + GMI + AQI + SGI + DEPI + SGAI + ACCR + LEVI
manipulator_logitCom <- glm(formula = formula_logitCom, data = Manipulators_completeSM, family = "binomial")
summary(manipulator_logitCom)
#difference in deviance(Null - Residual) = 205.58 - 119.77 = 85.81
#AIC = 685.43

formula_logitCom2 <- relevel(factor(`C-MANIPULATOR`),ref="0") ~ DSRI + GMI + AQI + SGI + DEPI + SGAI + ACCR 
manipulator_logitCom2 <- glm(formula = formula_logitCom2, data = Manipulators_completeSM, family = "binomial")
summary(manipulator_logitCom2)
#AIC = 683.43

formula_logitCom3 <- relevel(factor(`C-MANIPULATOR`),ref="0") ~ DSRI + GMI + AQI + SGI + DEPI + ACCR 
manipulator_logitCom3 <- glm(formula = formula_logitCom3, data = Manipulators_completeSM, family = "binomial")
summary(manipulator_logitCom3)
#AIC = 684.23

formula_logitCom4 <- relevel(factor(`C-MANIPULATOR`),ref="0") ~ DSRI + GMI + AQI + SGI  + ACCR 
manipulator_logitCom4 <- glm(formula = formula_logitCom4, data = Manipulators_completeSM, family = "binomial")
summary(manipulator_logitCom4)
#AIC = 690.93

predictedCom1 <- predict(manipulator_logitCom, type = "response")
predictedCom2 <- predict(manipulator_logitCom2, type = "response")
predictedCom3 <- predict(manipulator_logitCom3, type = "response")
predictedCom4 <- predict(manipulator_logitCom4, type = "response")

prediction.objCom1 <- prediction(predictedCom1,Manipulators_completeSM$`C-MANIPULATOR`)
perfCom1 = performance(prediction.objCom1,"tpr","fpr")

prediction.objCom2 <- prediction(predictedCom2,Manipulators_completeSM$`C-MANIPULATOR`)
perfCom2 = performance(prediction.objCom2,"tpr","fpr")

prediction.objCom3 <- prediction(predictedCom3,Manipulators_completeSM$`C-MANIPULATOR`)
perfCom3 = performance(prediction.objCom3,"tpr","fpr")

prediction.objCom4 <- prediction(predictedCom4,Manipulators_completeSM$`C-MANIPULATOR`)
perfCom4 = performance(prediction.objCom4,"tpr","fpr")

plot(perfCom1,xlab="False Positive Rate (False Alarm)",ylab="True Positive Rate (Recall or Sensitivity)",
     main="ROC Curve", col = "red" )
plot(perfCom2, add = TRUE, col = "green")
plot(perfCom3, add = TRUE, col = "orange")
plot(perfCom4, add = TRUE, col = "blue")


aucCom1 = unlist(slot(performance(prediction.objCom1, "auc"), "y.values"))
aucCom2 = unlist(slot(performance(prediction.objCom2, "auc"), "y.values"))
aucCom3 = unlist(slot(performance(prediction.objCom3, "auc"), "y.values"))
aucCom4 = unlist(slot(performance(prediction.objCom4, "auc"), "y.values"))

aucVal1 = paste(c("AUC(red) = "), aucCom1, sep = "") 
aucVal2 = paste(c("AUC(green) = "), aucCom2, sep = "") 
aucVal3 = paste(c("AUC(orange) = "), aucCom3, sep = "") 
aucVal4 = paste(c("AUC(blue) = "), aucCom4, sep = "") 
#Paste: Concatenate vetors after converting to character
legend(0.46, 0.5, c(aucVal1, aucVal2,aucVal3,aucVal4), cex = 0.7, box.col = "white")
abline(a= 0, b=1, col="gray")  #Add straight lines to a plot (a = intercept and b = slope)


print(opt.cut.distance(perfCom3, prediction.objCom3))
#Best threshold value = 0.14

table(Manipulators_completeSM$`C-MANIPULATOR`)
Predictedvalue3 <- rep(NA,nrow(Manipulators_completeSM))
Predictedvalue3[predictedCom3<=0.14] <- 0
Predictedvalue3[predictedCom3>0.14] <- 1


Error0.14 <- confusionMatrix(Predictedvalue3,Manipulators_completeSM$`C-MANIPULATOR`, positive = "1")
Error0.14$overall[2]


library(caret)

kfolds <- createFolds(Manipulators_completeSM$`C-MANIPULATOR`, k = 10, list = FALSE)
#Making sure folds created are stratified folds
for (i in 1:10) 
{
  instances <- Manipulators_completeSM[kfolds==i,]
  xtab <- table(instances$`C-MANIPULATOR`)
  print(xtab)
}

crossValidation = function(myFormula,dataset)
{
  s=0
  k=0
  f=0
  sp=0
  kfolds <- createFolds(dataset$`C-MANIPULATOR`, k = 10, list = FALSE)
  for (i in 1:10)
  {
    Testdata <- dataset[kfolds==i,]
    Traindata <- dataset[kfolds!=i,]
    x <- glm(myFormula, data =Traindata, family = "binomial")
    yprob <- predict(x, newdata = Testdata , class = "response")
    preds <- rep(NA,nrow(Testdata))
    preds[yprob<=0.14] <- 0
    preds[yprob>0.14] <- 1
    ConMat <- confusionMatrix(preds,Testdata$`C-MANIPULATOR`,positive = "1")
    print(ConMat$table)
    recall = ConMat$table[2,2]/(ConMat$table[1,2]+ConMat$table[2,2])
    presicion = ConMat$table[2,2]/(ConMat$table[2,1]+ConMat$table[2,2])
    specificity = ConMat$table[1,1]/(ConMat$table[1,1]+ConMat$table[2,1])
    FScore = 2*recall*presicion/(recall+presicion)
    print(c("Sensitivity of fold",i,recall))
    print(c("F-Score of fold",i,FScore))
    print(c("Precision of fold",i,presicion))
    print(c("Specificity of fold",i,specificity))
    kappa = ConMat$overall[2]
    s=s+recall
    f=f+FScore
    k=k+kappa
    sp=sp+specificity
  }
  AvgSensitivity = s/10
  print(c("Cross validation Sensitivity is", AvgSensitivity))
  AvgFScore = f/10
  print(c("Cross validation F-Score is", AvgFScore))
  AvgKappa = k/10
  print(c("Cross validation Kappa is", AvgKappa))
  print(c("Cross validation Specificity is", sp/10))
  
}

#Using cross validation function above to find how the model is performing
crossValidation(formula_logitCom3,Manipulators_completeSM)

#Cross validation Sensitivity is 0.39447 
#Cross validation F-Score is 0.51112
#Cross validation Kappa is 0.4602
#Cross validation Specificity = 97%

data_new <- Manipulators_completeSM 


## Appending the smote data to dataset and creating the data_new to be used for stratified sampling 
data_new <- rbind(case, data_smote)
nrow(data_new)
nrow(data_smote)
nrow(case)

##Test data is 16.31 % of the total data
print(table(data_new$`C-MANIPULATOR`))

#Dividing the data into test and training sets using stratified sampling 
install.packages("caret")
library(caret)
set.seed(1234)
trainIndex <- createDataPartition(data_new$`C-MANIPULATOR`, p = .7, list = FALSE, times = 1)
data_train<- data_new[trainIndex, ]
data_test <- data_new[-trainIndex, ]

##Both test and training data sets have around ~16% of 1's of the total data
print(table(data_train$`C-MANIPULATOR`))
print(table(data_test$`C-MANIPULATOR`))
nrow(data_train)
nrow(data_test)

## Decision Trees
####Stratified Data
library(rpart)
library(rpart.plot)
train_dt <- rpart(Formula1, data = data_train, method = "class")
printcp(train_dt)
train_dt <- rpart(Formula1, data = data_train, method = "class", control = rpart.control(cp = 0.01))
rpart.plot(train_dt)

##Creating the confusion matrix to measure performance on test data 
install.packages("e1071")
library(e1071)
trainerr_dt <- confusionMatrix(predict(train_dt, type = "class"), data_train$`C-MANIPULATOR`, dnn = c("Predicted","Actual"))
testerr_dt <- confusionMatrix(predict(train_dt, type = "class", newdata = data_test), data_test$`C-MANIPULATOR`, dnn = c("Predicted","Actual"))
trainerr_dt
testerr_dt

##10 fold cross validation 
kfolds <- createFolds(data_new$`C-MANIPULATOR`, k = 10, list = FALSE)
crossValidation_dt = function(myFormula)
{
  s=0
  k=0
  f=0
  sp = 0
  for (i in 1:10)
  {
    Testdata <- data_new[kfolds==i,]
    Traindata <- data_new[kfolds!=i,]
    x <- rpart(Formula1, data = Traindata, method = "class",
               control = rpart.control(cp = 0.01))
    rpart.plot(x)
    preds <- predict(x, newdata = Testdata ,  type = "class")
    ConMat <- confusionMatrix(preds,Testdata$`C-MANIPULATOR`,positive = "1")
    #print(ConMat$table)
    recall = ConMat$table[2,2]/(ConMat$table[1,2]+ConMat$table[2,2])
    presicion = ConMat$table[2,2]/(ConMat$table[2,1]+ConMat$table[2,2])
    specificity = ConMat$table[1,1]/(ConMat$table[1,1]+ConMat$table[2,1])
    FScore = 2*recall*presicion/(recall+presicion)
    #print(c("Sensitivity of fold",i,recall))
    #print(c("F-Score of fold",i,FScore))
    #print(c("Specificity of fold",i,specificity))
    kappa = ConMat$overall[2]
    s=s+recall
    f=f+FScore
    k=k+kappa
    sp=sp+specificity
  }
  AvgSensitivity = s/10
  print(c("Cross validation Sensitivity is", AvgSensitivity))
  AvgFScore = f/10
  print(c("Cross validation F-Score is", AvgFScore))
  AvgKappa = k/10
  print(c("Cross validation Kappa is", AvgKappa))
  Avgspecificity = sp/10
  print(c("Cross validation Specificity is", Avgspecificity))
}

crossValidation_dt(Formula1)

###Random Forest
####Stratified Data
install.packages("randomForest")
library(randomForest)
random_forest = randomForest(Formula1, data = data_train, ntree = 2000, proximity = TRUE, 
                             replace= TRUE, sampsize = nrow(data_train), importance = TRUE,
                             mtry = sqrt(ncol(data_train)))
print(random_forest) 
plot(random_forest)

#Get accuracy of prediction on Test Data
rf_test=predict(random_forest,newdata = data_test)
rf_test
importance(random_forest)

##Measuring the performance through confusionMatrix 
trainerr_rf <- confusionMatrix(predict(random_forest), data_train$`C-MANIPULATOR`,dnn = c("Predicted","Actual"))
testerr_rf <- confusionMatrix(rf_test, data_test$`C-MANIPULATOR`,dnn = c("Predicted","Actual"))
trainerr_rf
testerr_rf

##10 fold cross validation
crossValidation_rf = function(myFormula)
{
  s=0
  k=0
  f=0
  sp = 0 
  for (i in 1:10)
  {
    Testdata <- data_new[kfolds==i,]
    Traindata <- data_new[kfolds!=i,]
    x <-randomForest(Formula1, data = Traindata, ntree = 2000, proximity = TRUE, 
                     replace= TRUE, sampsize = nrow(data_train), importance = TRUE,
                     mtry = sqrt(ncol(data_train)))
    preds <- predict(x, newdata = Testdata )
    ConMat <- confusionMatrix(preds,Testdata$`C-MANIPULATOR`,positive = "1")
    #print(ConMat$table)
    recall = ConMat$table[2,2]/(ConMat$table[1,2]+ConMat$table[2,2])
    presicion = ConMat$table[2,2]/(ConMat$table[2,1]+ConMat$table[2,2])
    specificity = ConMat$table[1,1]/(ConMat$table[1,1]+ConMat$table[2,1])
    FScore = 2*recall*presicion/(recall+presicion)
    #print(c("Sensitivity of fold",i,recall))
    #print(c("F-Score of fold",i,FScore))
    #print(c("Specificity of fold",i,specificity))
    kappa = ConMat$overall[2]
    s=s+recall
    f=f+FScore
    k=k+kappa
    sp=sp+specificity
  }
  AvgSensitivity = s/10
  print(c("Cross validation Sensitivity is", AvgSensitivity))
  AvgFScore = f/10
  print(c("Cross validation F-Score is", AvgFScore))
  AvgKappa = k/10
  print(c("Cross validation Kappa is", AvgKappa))
  Avgspecificity = sp/10
  print(c("Cross validation Specificity is", Avgspecificity))
}

crossValidation_rf(Formula1)

####Ada-boost 
install.packages("mboost")
library(mboost)
crossValidation_boosting = function(myFormula)
{
  s=0
  k=0
  f=0
  sp = 0
  for (i in 1:10)
  {
    Testdata <- data_new[kfolds==i,]
    Traindata <- data_new[kfolds!=i,]
    x <- mboost(myFormula,data = Traindata,control = boost_control(mstop = 400, nu = 0.7,risk = c("inbag", "oobag", "none"), stopintern = FALSE,
                                                                   center = TRUE, trace = FALSE), family= Binomial(type = c("adaboost", "glm"),link = c("logit"))
                , baselearner = c("bbs", "bols", "btree", "bss", "bns"))
    yprob <- predict(x, newdata = Testdata , class = "response")
    preds <- rep(NA,nrow(Testdata))
    preds[yprob<=0.14] <- 0
    preds[yprob>0.14] <- 1
    ConMat <- confusionMatrix(preds,Testdata$`C-MANIPULATOR`,positive = "1")
    #print(ConMat$table)
    recall = ConMat$table[2,2]/(ConMat$table[1,2]+ConMat$table[2,2])
    presicion = ConMat$table[2,2]/(ConMat$table[2,1]+ConMat$table[2,2])
    specificity = ConMat$table[1,1]/(ConMat$table[1,1]+ConMat$table[2,1])
    FScore = 2*recall*presicion/(recall+presicion)
    #print(c("Sensitivity of fold",i,recall))
    #print(c("F-Score of fold",i,FScore))
    #print(c("Specificity of fold",i,specificity))
    kappa = ConMat$overall[2]
    s=s+recall
    f=f+FScore
    k=k+kappa
    sp=sp+specificity
  }
  AvgSensitivity = s/10
  print(c("Cross validation Sensitivity is", AvgSensitivity))
  AvgFScore = f/10
  print(c("Cross validation F-Score is", AvgFScore))
  AvgKappa = k/10
  print(c("Cross validation Kappa is", AvgKappa))
  Avgspecificity = sp/10
  print(c("Cross validation Specificity is", Avgspecificity))
}


##Using the variables that were found significant in logistic regression
Formula2 <- relevel(factor(`C-MANIPULATOR`), ref="0")~DSRI+GMI+AQI+SGI+ACCR
crossValidation_boosting(Formula2)




